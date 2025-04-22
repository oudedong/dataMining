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

caster <- list(
  numericCaster = function(x){
    
    if(!is.character(x)) x <- as.character(x)
    
    x <- x %>% 
      str_remove_all(",") %>% 
      str_remove_all("%") %>% 
      str_remove_all("\\+")
    if(any(str_detect(x, "\\."))){
      return(as.numeric(x))
    }
    return(as.integer(x))
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
  }
)

# 유틸리티 함수 모음 (웹 크롤링 후 정제용)
dataUtil <- list(
  
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
  },
  
  # 벡터나 리스트의 앞 n개만 추출 (row trimming에 사용)
  sliceVector = function(vector, n){
    return(vector[1:n])
  },
  
  smart_cast = function(x, allow_factor) {
    
    res <- caster$numericCaster(x)
    if (!is.null(res) && !any(is.na(res) & x != "NA")) return(res)
    
    res <- caster$timeCaster(x)
    if (!is.null(res) && !all(is.na(res))) return(res)
    
    if (allow_factor) {
      res <- as.factor(x)
      if (!is.null(res)) return(res)
    }

    return(x)
  },
  
  auto_cast_df = function(df, allow_factor = FALSE) {
    df %>% mutate(across(everything(), ~ dataUtil$smart_cast(., allow_factor = allow_factor)))
  },
  
  # rootNodes: html 요소들
  # rowSuplier: 행 추출 함수 (하나)
  # colSupliers: 열 추출 함수 리스트 (하나)
  # useHeader: 논리값 (TRUE/FALSE)
  parseAnythingToTable_list <- function(rootNodes, rowSuplier, colSupliers, useHeader = FALSE, headerHeight=0) {
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
        cols <- dataUtil$stackFromLeftEvenly(cols, length(cols), colItem, 1, 1)  # col 정렬
      }
    }
    
    ret <- bind_cols(cols)  # 열 기준 결합
    if(useHeader){
      ret <- ret %>% dataUtil$makeHeader(headerHeight)
    }
    return(ret)
  },
  
  # 여러 테이블에 parseChart 적용 (인자 리스트 대응, 길이 1이면 자동 반복)
  parseChart_list = function(tableBodies, textSupliers, rowLens, useHeaders=FALSE, headerHeights=0){
    len <- length(tableBodies)  # 기준 길이
    
    # 길이 1이면 자동 반복 (broadcasting)
    if (length(textSupliers) == 1) textSupliers <- rep(textSupliers, len)
    if (length(rowLens) == 1)      rowLens      <- rep(rowLens, len)
    if (length(useHeaders) == 1)   useHeaders   <- rep(useHeaders, len)
    if (length(useHeaders) == 1)   headerHeights <- rep(headerHeights, len)
    
    return(Map(dataUtil$parseChart, tableBodies, textSupliers, rowLens, useHeaders, headerHeights))
  },
  
  # HTML table 파싱: rowspan/colspan 포함 정제된 tibble 반환
  parseChart = function(tableBody, textSuplier, rowLen, useHeader=FALSE, headerHeight=0){
    rows <- tableBody %>% html_elements('tr')  # tr 기준으로 행 추출
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    
    for(i in 1:length(rows)){
      td <- rows[i] %>% html_elements('td, th')  # 셀 추출
      
      for(j in 1:length(td)){
        rowspan <- td[j] %>% html_attr("rowspan")  # 병합 여부
        colspan <- td[j] %>% html_attr("colspan")
        text <- td[j] %>% textSuplier %>% html_text2()  # 텍스트 추출 방식 선택
        
        if(is.na(colspan)) colspan <- 1 else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1 else rowspan <- as.numeric(rowspan)
        
        cols <- dataUtil$stackFromLeftEvenly(cols, rowLen, text, rowspan, colspan)  # 셀 정렬
      }
    }
    
    height <- Map(length, cols) %>% unlist %>% min  # 최소 높이 맞춰서 잘라내기
    sliced <- Map(dataUtil$sliceVector, cols, n=height)
    
    ret <- bind_cols(sliced)
    if(useHeader){
      ret <- ret %>% dataUtil$makeHeader(headerHeight)
    }
    return(ret)
  },
  
  # 집계 함수: 특정 열로 group한 뒤, 나머지 열은 supplier로 summarise
  groupTable = function(table, colNamesVector, supplier){
    colsToSummerize <- setdiff(colnames(table), colNamesVector)
    
    ret <- table %>% 
      group_by(across(all_of(colNamesVector))) %>%
      summarise(across(all_of(colsToSummerize), supplier, .names="{.col}"))  # 열별 요약
  },
  
  # 복잡한 테이블 정렬: rowspan/colspan 대응해서 왼쪽부터 채우는 열 정렬기
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
        temp <- rep(toPut, rowspan)
        tableCols[[j]] <- c(tableCols[[j]], temp)
      }
      break
    }
    return(tableCols)
  },
  
  replaceString = function(x, paterns, to) {
    x %>% mutate(across(
      where(is.character),
      ~ str_replace_all(., paterns, to)
    ))
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
  }
)

mySuppliers <- list(
  
  #parent의 자식노드(css)중 n번째를 선택함
  nthChildSelector = function(css, n){
    csss = paste0(css, ':nth-child(',n,')')
    function(parent){
      return(parent %>% html_element(csss))
    }
  },
  
  nthOfTypeSelector = function(tag, n) {
    function(parent) {
      selector <- paste0(tag, ":nth-of-type(", n, ")")
      parent %>% html_element(selector)
    }
  },
  
  lastChildSelector = function(tag) {
    function(parent) {
      selector <- paste0(tag, ":last-child")
      parent %>% html_element(selector)
    }
  },
  
  firstChildSelector = function(tag) {
    function(parent) {
      selector <- paste0(tag, ":first-child")
      parent %>% html_element(selector)
    }
  },
  
  last_tag_of_type = function(tag) {
    function(parent) {
      els <- parent %>% html_elements(tag)
      if (length(els) > 0) els[[length(els)]] else NA
    }
  },
  
  first_tag_of_type = function(tag) {
    function(parent) {
      els <- parent %>% html_elements(tag)
      if (length(els) > 0) els[1] else NA
    }
  },
  
  optional = function(selectors){
    function(parent){
      for(i in 1:length(selectors)){
        
        temp <- selectors[[i]]
        ret <- parent %>% temp
        
        if(!is.null(ret) && !is.na(ret)) return(ret)
      }
      return(NA)
    }
  },
  
  chained = function(selectors){
    function(parent){
      ret <- parent
      for(i in 1:length(selectors)){
        
        temp <- selectors[[i]]
        ret <- ret %>% temp
        
        if(is.null(ret) || is.na(ret)) return(NA)
      }
      return(ret)
    }
  },
  
  classSelectorStringGen = function(or = NULL, all = NULL, not = NULL) {
    # or: 하나라도 해당되면
    or_selector <- if (!is.null(or)) paste0(".", or, collapse = ", ") else ""
      
    # all: 전부 포함해야 됨
    all_selector <- if (!is.null(all)) paste0(".", all, collapse = "") else ""
      
    # not: 해당 클래스 없어야 함
    not_selector <- if (!is.null(not)) paste0(":not(.", not, ")", collapse = "") else ""
      
    # 최종 selector 조합
    if (or_selector != "" && all_selector != "") {
      selector <- paste0("(", or_selector, ")", all_selector, not_selector)
    } else {
      selector <- paste0(or_selector, all_selector, not_selector)
    }
      
    return(selector)
  }
)

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

