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
    library(xml2)
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
    install.packages("xm12")
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
  
  #nodeset에서 자손의 자손은 제거
  onlyOuter = function(nodeset){
    keep(nodeset, function(node) {
      !any(map_lgl(nodeset, function(other) {
        #!identical(node, other) && any(node %in% (html_elements(other, "*")))
        
        a <- !identical(node, other)
        b <- html_elements(other, xpath=".//*")
        c <- FALSE
        if(length(b) != 0) {
          for(i in 1:length(b)){
            if(identical(node, b[[i]])){
              c <- TRUE
              break
            }
          }
        }
        a && c
      }))
    })
  },
  
  getEmptyNodeset = function(){
    doc <- read_html("<html><p>padding</p></html>")
    empty_nodeset <- html_elements(doc, "p")
  },
  
  #행에서 지정된 노드들을 가져와서 배치함
  placeHolderParser = function(rootNode, rowPath, colPaths){
    
    rows <- rootNode %>% html_elements(rowPath)  # 행 추출
    rows <- fromHtml$onlyOuter(rows)
    print(sprintf("placeHolderParser.colLength: %d", length(rows)))
    rowLen <- length(colPaths)
    print(sprintf("placeHolderParser.rowLength: %d", rowLen))
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    names(cols) <- colPaths
    
    for(row in 1:length(rows)){
      for(col in colPaths){
        found <- rows[[row]] %>% html_elements(col)
        found <- fromHtml$onlyOuter(found)
        if(length(found) == 0){
          print(sprintf("placeHolderParser: elements at [%d, %s] is empty", row, col))
        }
        cols <- fromHtml$stackFromLeftEvenly(cols, rowLen, found, 1, 1)
      }
    }
    do.call(tibble, cols)
  },
  
  #행에서 모든 노드들을 모아줌
  justGiveRowPathParser = function(rootNode, rowPath){
    
    rows <- rootNode %>% html_elements(rowPath)
    rows <- rows %>% fromHtml$onlyOuter()
    print(sprintf("justGiveRowPathParser.colLength: %d", length(rows)))
    rowsElements <- list()
    
    for(row in 1:length(rows)){
      elements <- rows[[row]] %>% html_elements("*")
      elements <- fromHtml$onlyOuter(elements)
      
      tempState <- list()  #노드의 개수를 셈
      tempRet = list()     #결과 한줄

      for(e in elements){
        
        tagName <- e %>% xml_name()
        tagState <- tempState[[tagName]] #노드가 몇번째인지 확인
        print(tagState)
        
        if(is.null(tagState)){ #처음이면
          tempState[[tagName]] <- 1
          tempRet[[paste0(tagName, ".1")]] <- list(e)
          next
        }
        tempState[[tagName]] <- tagState+1
        tempRet[[paste0(tagName, ".", tagState+1)]] <- list(e)
      }
      print(names(tempRet))
      rowsElements <- c(rowsElements, list(tempRet))
    }
    bind_rows(rowsElements)
  },
  
  #노드를 테이블 형식으로 배치
  #padding 활성화시 행들의 길이가 다르면 빈 노드로 길이를 맞춤
  parseAnything = function(rootNode, rowPath, colPath, rowLen = 0, padding = FALSE){
    
    rows <- rootNode %>% html_elements(rowPath)  #행 추출
    rows <- fromHtml$onlyOuter(rows)
    cols <- vector(mode="list", length = rowLen)  # 열 개수만큼 초기화
    rowCnt <- length(rows)
    
    print(sprintf("parseAnything.rowPath: %s", rowPath))
    print(sprintf("parseAnything.colPath: %s", colPath))
    print(sprintf("parseAnything.rowCnt: %d", rowCnt))
    
    for(i in 1:length(rows)){
      elements <- rows[i] %>% html_elements(colPath)  # 셀 추출
      elements <- fromHtml$onlyOuter(elements)
      
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
      
      totalLength <- 0
      requiredLength <- rowLen
      for(j in 1:rowLen){
        if(length(cols[[j]]) >= i){
          requiredLength <- requiredLength-1
        }
      }
      for(j in 1:length(elements)){
        rowspan <- elements[j] %>% html_attr("rowspan")
        colspan <- elements[j] %>% html_attr("colspan")
        
        if(is.na(colspan)) colspan <- 1 else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1 else rowspan <- as.numeric(rowspan)
        
        cols <- fromHtml$stackFromLeftEvenly(cols, rowLen, elements[j], rowspan, colspan)
        totalLength <- totalLength + colspan
      }
      
      if(totalLength != requiredLength){
        
        print("parseAnything.rowLen mismatch")
        print(sprintf("parseAnything.length(elements): %d", length(elements)))
        print(sprintf("at row %d", i))
        print(sprintf("totalLength/requiredLength: %d/%d", totalLength, requiredLength))
        
        if(padding){
          if(totalLength > requiredLength){
            stop("column Length is larger...")
          }
          print(sprintf("padded: %d", requiredLength - totalLength))
          empty_nodeset <- fromHtml$getEmptyNodeset()
          for(j in 1:(requiredLength - totalLength)){
            cols <- fromHtml$stackFromLeftEvenly(cols, rowLen, empty_nodeset, 1, 1)
          }
        }
        else{
          stop("padding not allowed, return current work")
        }
      }
      
    }
    
    print("colLength:")
    print(paste(as.character(map(cols, length))))
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
  
  filterColumnElements = function(table, cols, paths, onlySelected=FALSE){
    
    ret <- NULL
    supplier <- function(col){
        temp <- map(col, ~html_elements(., paths))
        temp <- fromHtml$onlyOuter(temp)
    } 
    
    if(is.numeric(cols)) cols <- colnames(table)[cols]
    
    print("filterColumnElements.selected cols:")
    print(cols)
    
    if(onlySelected){table <- table %>% select(all_of(cols))}
    ret <- table %>% mutate(across(all_of(cols), ~ supplier(.)))
    return(ret)
  }
)
#############################################################
fromNodes <- list(
  colstoText = function(table, cols){
    fromNodes$colstoString(table, cols, html_text2)
  },
  
  colstoHref = function(table, cols){
    fromNodes$colstoString(table, cols, function(x){
      html_attr(x, "href")
    })
  },
  
  colstoString = function(table, cols, supplier){
    
    if(is.numeric(cols)) cols <- colnames(table)[cols]
    print("colstoValue.selected cols:")
    print(cols)
    
    func1 <- function(x){
      ret <- map(x, function(x){ #각 xml_node에서 문자들을 추출출
        if(class(x)!="xml_node") map(x, supplier)
        else list(supplier(x))
      })
      ret <- map(ret, ~unlist(.))
      ret <- map(ret, ~Filter(function(x) length(x) > 0, .))
      ret <- map(ret, ~paste(., collapse = ",")) %>% unlist %>% unname()
    }
    table <- table %>% select(all_of(cols)) %>% mutate(across(everything(), func1))
    return(table)
  },
  
  #노드 테이블에서 해더를 만들때
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
