setUpUtil <- list(
  setLibs = function(){
    library(RSelenium)
    library(rvest)
    library(httr)
    library(jsonlite)
    library(data.table)
    library(dplyr)
    library(tidyverse)
    library(ggplot2)
    library(gganimate)
    library(gifski)
    library(data.table)
    library(rlang)
    library(stringi)
  },
  
  installLib = function(){
    setRepositories(ind=1:7)
    install.packages("RSelenium")
    install.packages("rvest")
    install.packages("httr")
    install.packages("jsonlite")
    install.packages("dplyr")
    install.packages("tidyverse")
    install.packages("ggplot2")
    install.packages("gganimate")
    install.packages("gifski")
    install.packages("rlang")
    
  },
  
  setRselenium = function(port=4445L){
    rD <- remoteDriver(browser = "chrome", port = port)
    rD$open()
    return(rD)
  }
)
###############################################################
castUtil <- list(
  numericCast = function(col, paterns = c(",", "%", "\\+", " ")){
    
    col <- as.character(col)
    
    for(i in 1:length(paterns)){
      col <- col %>% str_remove_all(paterns[i])  
    }
    
    print("col: ")
    print(col)
    
    ret <- as.integer(col)
    if(any(is.na(ret))){
      ret <- as.numeric(col)
    }
    
    print("casted col: ")
    print(ret)
    return(ret)
  },
  
  timeCast = function(x){
    
    try_cast <- function(expr) {
      tryCatch(expr, error = function(e) NULL, warning = function(w) NULL)
    }
    
    if(!is.character(x)) x <- as.character(x)
    
    res <- try_cast(ymd(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
    res <- try_cast(dmy(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
    res <- try_cast(as.Date(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
  },
  default_cast = function(x) {
    
    res <- castUtil$numericCast(x)
    if (!is.null(res) && !any(is.na(res))) return(res)
    
    res <- castUtil$timeCast(x)
    if (!is.null(res) && !any(is.na(res))) return(res)
    
    return(x)
  },
  
  default_cast_df = function(df) {
    df %>% mutate(across(everything(), castUtil$default_cast))
  }
)
##################################################
browerUtil <- list(
  waitUntilVisible = function(remDr, css_selector, timeout = 10, interval = 0.5) {
    start_time <- Sys.time()
    
    while (TRUE) {
      result <- tryCatch({
        elem <- remDr$findElement(using = "css selector", value = css_selector)
        TRUE  # 찾았으면 TRUE 반환
      }, error = function(e) {
        FALSE  # 못 찾았으면 FALSE
      })
      
      if (result) break  # 요소를 찾으면 루프 종료
      
      if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
        warning("waitUntilVisible: Timeout reached")
        break
      }
      
      Sys.sleep(interval)
    }
  }
)
#############################################
#크롤링 유틸
collectUtil <- list(
  parseAnything = function(rootNode, rowPath, colPath, rowLen = 0, padding = FALSE){
    
    rows <- rootNode %>% html_elements(rowPath)  # tr 기준으로 행 추출
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    
    print("rowlen:")
    print(length(rows))
    
    for(i in 1:length(rows)){
      elements <- rows[i] %>% html_elements(colPath)  # 셀 추출
      
      if(rowLen == 0){
        for(j in 1:length(elements)){
          colspan <- elements[j] %>% html_attr("colspan")
          if(is.na(colspan)) colspan <- 1 
          else colspan <- as.numeric(colspan)
          rowLen <- rowLen + colspan
        }
        cols <- vector(mode="list", length = rowLen)
        print("autoRowlen: ")
        print(rowLen)
      }
      
      
      if(length(elements) != rowLen){
        
        print("collen:")
        print(paste(i,length(elements)))
        print("rowLen:")
        print(rowLen)
        
        if(padding){
          if(length(elements) > rowLen){
            print("column Length is larger...")
          }
        }
        else{
          print("column Length mismatch...")
          break
        }
      }
      
      for(j in 1:length(elements)){
        rowspan <- elements[j] %>% html_attr("rowspan")  # 병합 여부
        colspan <- elements[j] %>% html_attr("colspan")
        
        if(is.na(colspan)) colspan <- 1 else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1 else rowspan <- as.numeric(rowspan)
        
        cols <- collectUtil$stackFromLeftEvenly(cols, rowLen, elements[j], rowspan, colspan)  # 셀 정렬
      }
      if(padding && (rowLen > length(elements))){
        print("pad:")
        print(rowLen - length(elements))
        for(j in 1:(rowLen - length(elements))){ #padding
          cols <- collectUtil$stackFromLeftEvenly(cols, rowLen, elements[length(elements)], rowspan, colspan)
        }
      }
    }
    
    print("row length: ")
    print(length(cols))
    print("col lengths: ")
    Map(print, lapply(cols, length))
    
    names(cols) <- as.character(1:rowLen)
    ret <- do.call(tibble, cols)
    return(ret)
  },
  
  # 범용 HTML 파서: 테이블 외 임의 구조도 → tibble로 변환
  parseChart = function(tableNode, rowLen=0, padding=FALSE){
    collectUtil$parseAnything(tableNode, "tr", "th, td", rowLen, padding)
  },
  
  stackFromLeftEvenly = function(tableCols, rowLen, toPut, rowspan, colspan){
    mapTable <- list()
    curPos <- 1
    height <- length(tableCols[[curPos]])
    
    for(end in 1:(rowLen+1)){
      if(end > rowLen){
        mapTable <- append(mapTable, list(tibble(st=c(curPos), len=c(end-curPos), h=c(height))))
      } else if(length(tableCols[[end]]) != height){
        mapTable <- append(mapTable, list(tibble(st=c(curPos), len=c(end-curPos), h=c(height))))
        curPos <- end
        height <- length(tableCols[[curPos]])
      }
    }
    
    mapTable <- bind_rows(mapTable) %>% arrange(h, st)
    
    for(i in 1:nrow(mapTable)){
      curSt  <- mapTable$st[i]
      curLen <- mapTable$len[i]
      if(colspan > curLen) next
      
      for(j in curSt:(curSt+colspan-1)){
        if(length(toPut) > 1) temp <- rep(list(toPut), rowspan)
        else temp <- rep(toPut, rowspan)
        tableCols[[j]] <- c(tableCols[[j]], temp)
      }
      break
    }
    return(tableCols)
  },
  
  
  # HTML 전체 구조 스캔: 태그, 클래스, id, href, 텍스트, 노드 자체까지 저장
  htmlCollector = function(parent, predict=NULL) {
    
    if(is.null(predict)){
      predict <- function(node){
        return(TRUE)
      }
    }
    
    all_nodes <- parent %>% html_elements("*")
    
    tag_info <- tibble(
      tag    = all_nodes %>% html_name(),
      class  = all_nodes %>% html_attr("class"),
      id     = all_nodes %>% html_attr("id"),
      href   = all_nodes %>% html_attr("href"),
      text   = all_nodes %>% html_text2(),
      node   = c(all_nodes)  # HTML 노드 원본
    ) %>% rowwise %>% filter(predict(node)) %>% ungroup()
    
    return(tag_info)
  },
  
  filterColumnElements = function(table, cols, paths){
    
    ret <- NULL
    supplier <- function(node){
        temp <- map(node, ~html_elements(., paths))
    } 
    
    if(is.numeric(cols)) col_ <- colnames(table)[cols]
    
    print("selected cols:")
    print(col_)
    
    ret <- table %>% mutate(across(all_of(col_), ~ supplier(.)))
    return(ret)
  }
)
#############################################################
afterCollectUtil <- list(
  toText = function(table){
    afterCollectUtil$toValue(table, html_text2)
  },
  
  toHref = function(table){
    afterCollectUtil$toValue(table, function(x){
      html_attr(x, "href")
    })
  },
  
  toValue = function(table, supplier){
    func1 <- function(x){
      #ret <- map(x, ~map(., html_text2))
      ret <- map(x, function(x){
        if(class(x)!="xml_node") map(x, supplier)
        else list(supplier(x))
      })
      ret <- map(ret, ~unlist(.))
      ret <- map(ret, ~Filter(function(x) length(x) > 0, .))
      ret <- map(ret, ~paste(., collapse = ",")) %>% unlist
    }
    table <- table %>% mutate(across(everything(), func1))
    return(table)
  },
  
  makeHeader = function(x, height){
    names <- x %>% slice_head(n=height) %>% summarise(across(everything(), ~ toString(.))) %>% unlist 
    for(i in 1:length(names)){
      if(length(names[[i]]) == 0) names[[i]] <- as.character(i)
    }
    
    print("generated_names:")
    print(names)
    print(class(names))
    
    names <- make.names(str_squish(names), unique = TRUE)
    names <- str_replace_all(names, "\\.{2,}", ".")
    
    print("generated_names:")
    print(names)
    print(class(names))
    
    names(x) <- names
    x <- x %>% slice(-1:(-1*height))
    return(x)
  }
)
