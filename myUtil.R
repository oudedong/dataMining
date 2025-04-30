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
fromTable <- list(
  
  tryCast = function(table, cols, paterns, suppliers){
    
    if(is.numeric(cols)) cols <- colnames(table)[cols]
    print("tryCast.selected cols:")
    print(cols)
    
    selected <- table %>% select(all_of(cols)) %>% mutate(across(everything(), as.character))
    #전처리
    for(i in paterns){
      selected <- selected %>% mutate(across(everything(), ~str_remove_all(., i)))  
    }
    #변환 후 확인
    for(i in cols){
      #print(sprintf("tryCast.curColName: %s", i))
      for(j in 1:length(suppliers)){
        temp <- selected %>% mutate(across(all_of(i), suppliers[[j]]))
        if(!any(is.na(temp[[i]])) && !any(is.null(temp[[i]]))){
          table[[i]] <- temp[[i]]
          break
        }
        if(i == cols[length(cols)]){
          warning(sprintf("tryCast: fail to convert row %s", i))  
        }
      }
    }
    #print("numericCast.casted: ")
    #print(ret)
    
    return(table)
  },
  
  tryNumericCast = function(table, cols, paterns = c(",", "%", "\\+", " ")){
    fromTable$tryCast(table, cols, paterns, list(as.integer, as.numeric))
  },
  
  tryTimeCast = function(table, cols, paterns = c()){
    
    getTry <- function(supplier) {
      function(col) {
        tryCatch(supplier(col), error = function(e) NULL, warning = function(w) NULL)
      }
    }
    fromTable$tryCast(table, cols, paterns, suppliers = list(getTry(ymd), getTry(dmy), getTry(as.Date)))
  },
  tryDefaultCast = function(table, cols) {
    #먼저 수치 변환
    ret <- fromTable$tryNumericCast(table, cols)
    #변환 안된 열들에 대해 다른 변환을 시도
    cols <- ret %>% select(where(is.character)) %>% colnames() %>% unlist()
    print("tryDefaultCast.selected cols:")
    print(cols)
    ret <- fromTable$tryTimeCast(ret, cols)
  },
  
  #일반 테이블에서 헤더를 만들때
  makeHeader = function(x, height){
    
    #헤더를 하나의 행으로 압축
    names <- x %>% slice_head(n=height) %>% summarise(across(everything(), ~ toString(.))) %>% unlist 
    for(i in 1:length(names)){
      if(length(names[[i]]) == 0) names[[i]] <- as.character(i)
    }
    names <- str_squish(names)
    
    names <- make.names(names, unique = TRUE)
    names <- str_replace_all(names, "\\.{2,}", ".")
    
    print("makeHeader.generated_names:")
    print(names)
    
    names(x) <- names
    x <- x %>% slice((height+1):nrow(x))
    return(x)
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
fromHtml <- list(
  parseAnything = function(rootNode, rowPath, colPath, rowLen = 0, padding = FALSE){
    
    rows <- rootNode %>% html_elements(rowPath)  # tr 기준으로 행 추출
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    rowCnt <- length(rows)
    
    print(sprintf("parseAnything.rowPath: %s", rowPath))
    print(sprintf("parseAnything.colPath: %s", colPath))
    print(sprintf("parseAnything.rowCnt: %d", rowCnt))
    
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
        print(sprintf("parseAnything.autoRowlen: %d", rowLen))
      }
      
      
      if(length(elements) != rowLen){
        
        print("parseAnything.rowLen mismatch")
        print(sprintf("at row %d", i))
        print(sprintf("rowLen/curLen: %d/%d", rowLen, length(elements)))
        
        if(padding){
          if(length(elements) > rowLen){
            stop("column Length is larger...")
          }
        }
        else{
          print("padding not allowed, return current work")
          break
        }
      }
      
      for(j in 1:length(elements)){
        rowspan <- elements[j] %>% html_attr("rowspan")
        colspan <- elements[j] %>% html_attr("colspan")
        
        if(is.na(colspan)) colspan <- 1 else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1 else rowspan <- as.numeric(rowspan)
        
        cols <- fromHtml$stackFromLeftEvenly(cols, rowLen, elements[j], rowspan, colspan)
      }
      if(padding && (rowLen > length(elements))){
        
        print(sprint("padded: %d", rowLen - length(elements)))
        
        for(j in 1:(rowLen - length(elements))){
          cols <- fromHtml$stackFromLeftEvenly(cols, rowLen, elements[length(elements)], rowspan, colspan)
        }
      }
    }
    
    names(cols) <- as.character(1:rowLen)
    ret <- do.call(tibble, cols)
    return(ret)
  },
  
  parseChart = function(tableNode, rowLen=0, padding=FALSE){
    fromHtml$parseAnything(tableNode, "tr", "th, td", rowLen, padding)
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
    supplier <- function(col){
        temp <- map(col, ~html_elements(., paths))
    } 
    
    if(is.numeric(cols)) cols <- colnames(table)[cols]
    
    print("filterColumnElements.selected cols:")
    print(cols)
    
    ret <- table %>% mutate(across(all_of(cols), ~ supplier(.)))
    return(ret)
  }
)
#############################################################
fromNodes <- list(
  colstoText = function(table, cols){
    fromNodes$colstoValue(table, cols, html_text2)
  },
  
  colstoHref = function(table, cols){
    fromNodes$colstoValue(table, cols, function(x){
      html_attr(x, "href")
    })
  },
  
  colstoValue = function(table, cols, supplier){
    
    if(is.numeric(cols)) cols <- colnames(table)[cols]
    print("colstoValue.selected cols:")
    print(cols)
    
    func1 <- function(x){
      ret <- map(x, function(x){
        if(class(x)!="xml_node") map(x, supplier)
        else list(supplier(x))
      })
      ret <- map(ret, ~unlist(.))
      ret <- map(ret, ~Filter(function(x) length(x) > 0, .))
      ret <- map(ret, ~paste(., collapse = ",")) %>% unlist
    }
    table <- table %>% select(all_of(cols)) %>% mutate(across(everything(), func1))
    return(table)
  },
  
  #노드 테이블에서 해더를 만들때때
  makeHeader = function(x, height, cssPath=NULL){
    
    #헤더를 하나의 행으로 압축
    names <- x %>% slice_head(n=height) 
    if(!is.null(cssPath)){
      names <- names %>% mutate(across(everything(), html_elements(cssPath)))
    }
    names <- names %>% fromNodes$colstoText(colnames(names))
    names <- names %>% summarise(across(everything(), ~ toString(.))) %>% unlist 
    for(i in 1:length(names)){
      if(length(names[[i]]) == 0) names[[i]] <- as.character(i)
    }
    names <- str_squish(names)
    
    names <- make.names(names, unique = TRUE)
    names <- str_replace_all(names, "\\.{2,}", ".")
  
    print("makeHeader.generated_names:")
    print(names)
    
    names(x) <- names
    x <- x %>% slice((height+1):nrow(x))
    return(x)
  }
)
