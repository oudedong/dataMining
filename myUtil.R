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
    install.packages("Rselenium")
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
  numericCaster = function(x){
    
    if(!is.character(x)) x <- as.character(x)
    
    x <- x %>% 
      str_remove_all(",") %>% 
      str_remove_all("%") %>% 
      str_remove_all("\\+")
    print("x: ")
    print(x)
    ret <- as.integer(x)
    if(any(is.na(ret))){
      ret <- as.numeric(x)
    }
    print("casted x: ")
    print(ret)
    return(ret)
  },
  
  timeCaster = function(x){
    
    try_cast <- function(expr) {
      tryCatch(expr, error = function(e) NULL, warning = function(w) NULL)
    }
    
    if(!is.character(x)) x <- as.character(x)
    
    res <- try_cast(as.Date(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
    res <- try_cast(ymd(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
    res <- try_cast(dmy(x))
    if (!is.null(res) && !all(is.na(res))) return(res)
  },
  smart_cast = function(x, allow_factor) {
    
    res <- castUtil$numericCaster(x)
    if (!is.null(res) && !any(is.na(res) & x != "NA")) return(res)
    
    res <- castUtil$timeCaster(x)
    if (!is.null(res) && !all(is.na(res))) return(res)
    
    if (allow_factor) {
      res <- as.factor(x)
      if (!is.null(res)) return(res)
    }
    
    return(x)
  },
  
  auto_cast_df = function(df, allow_factor = FALSE) {
    df %>% mutate(across(everything(), ~ castUtil$smart_cast(., allow_factor = allow_factor)))
  },
  
  # 집계 함수: 특정 열로 group한 뒤, 나머지 열은 supplier로 summarise
  groupTable = function(table, colNamesVector, supplier){
    colsToSummerize <- setdiff(colnames(table), colNamesVector)
    
    ret <- table %>% 
      group_by(across(all_of(colNamesVector))) %>%
      summarise(across(all_of(colsToSummerize), supplier, .names="{.col}"))  # 열별 요약
  },
  
  replaceString = function(x, paterns, to) {
    x %>% mutate(across(
      where(is.character),
      ~ str_replace_all(., paterns, to)
    ))
  }
)
##########################################
etcUtil <- list(
  # 벡터나 리스트의 앞 n개만 추출 (row trimming에 사용)
  sliceVector = function(vector, n){
    return(vector[1:n])
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
  parseChart = function(tableBody, rowLen){
    rows <- tableBody %>% html_elements('tr')  # tr 기준으로 행 추출
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    
    print("rowlen:")
    print(length(rows))
    
    for(i in 1:length(rows)){
      elements <- rows[i] %>% html_elements('td, th')  # 셀 추출
      
      print("collen:")
      print(length(elements))
      
      for(j in 1:length(elements)){
        rowspan <- elements[j] %>% html_attr("rowspan")  # 병합 여부
        colspan <- elements[j] %>% html_attr("colspan")
        
        if(is.na(colspan)) colspan <- 1 else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1 else rowspan <- as.numeric(rowspan)
        
        cols <- collectUtil$stackFromLeftEvenly(cols, rowLen, elements[j], rowspan, colspan)  # 셀 정렬
      }
    }
    
    print("lengths: ")
    print(length(cols))
    
    names(cols) <- as.character(1:rowLen)
    ret <- do.call(tibble, cols)
    return(ret)
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
  
  # rootNodes: html 요소들
  # rowSuplier: 행 추출 함수 (하나)
  # colSupliers: 열 추출 함수 리스트 (하나)
  # useHeader: 논리값 (TRUE/FALSE)
  parseAnythingToTable_list = function(rootNodes, rowSuplier, colSupliers, useHeader = FALSE, headerHeight=0) {
    len <- length(rootNodes)
    
    rowSuplier  <- rep(list(rowSuplier), len)       # 리스트로 묶어 반복
    colSupliers <- rep(list(colSupliers), len)      # 리스트 안에 리스트
    useHeader   <- rep(useHeader, len)              # 논리값 벡터
    headerHeight<- rep(headerHeight, len)
    
    Map(parseAnythingToTable, rootNodes, rowSuplier, colSupliers, useHeader,headerHeight)
  },
  
  # 범용 HTML 파서: 테이블 외 임의 구조도 → tibble로 변환
  parseAnythingToTable = function(rootNode, rowSuplier, colSupliers, useHeader=FALSE, headerHeight=0){
    rows <- rootNode %>% rowSuplier()  # 행 단위 노드 추출
    cols <- vector(mode="list", length = length(colSupliers))  # 열 공간 준비
    
    print(paste0("rowlen: ", length(rows)))
    
    for(i in 1:length(rows)){
      for(j in 1:length(cols)){
        temp = colSupliers[[j]]
        colItem <- rows[[i]] %>% temp %>% html_text2()  # 텍스트 추출
        cols <- collectUtil$stackFromLeftEvenly(cols, length(cols), colItem, 1, 1)  # col 정렬
      }
    }
    
    ret <- bind_cols(cols)  # 열 기준 결합
    if(useHeader){
      ret <- ret %>% dataUtil$makeHeader(headerHeight)
    }
    return(ret)
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
  
  filterColumnElements = function(table, col, paths){
    
    ret <- NULL
    supplier <- function(node){
      ret <- list()
      for(i in 1:length(paths)){
        temp <- map(node, ~html_elements(., paths[i]))
        ret[[i]] <- temp
      }
      ret <- do.call(Map, c(list(list), ret))
      ret <- map(ret, ~ Filter(function(x) length(x) > 0, .))
      print(ret)
      return(ret)
    } 
    
    if(is.character(col)){
      ret <- table %>% mutate(across(any_of(col), ~ supplier(.)))
    }
    else if(is.numeric(col)){
      ret <- table %>% mutate(across(col, ~ supplier(.)))
    }
    else{print("wrong input....")}
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
        else supplier(x)
      })
      ret <- map(ret, ~unlist(.))
      ret <- map(ret, ~Filter(function(x) length(x) > 0, .))
      ret <- map(ret, ~paste(., collapse = ","))
    }
    table <- table %>% mutate(across(everything(), func1))
    return(table)
  },
  
  makeHeader = function(x, height){
    names <- x %>% slice_head(n=height) %>% summarise(across(everything(), ~ toString(.))) %>% unlist 
    for(i in 1:length(names)){
      if(names[[i]] == "") names[[i]] <- as.character(i)
    }
    names <- Map(function(names){
      names <- str_split(names, ",\\s*")[[1]]
      names <- unique(names) %>% str_flatten(collapse = ".")
    }, names) %>% unlist
    names(x) <- str_replace_all(make.names(str_squish(names), unique = TRUE), "\\.{2,}", ".")
    x <- x %>% slice(-1:(-1*height))
    return(x)
  }
)
