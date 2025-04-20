#setRepositories(ind=1:7)
#install.packages("Rselenium")
#install.packages("rvest")
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("rlang")
#install.packages("R6")

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
library(R6)
library(stringi)

setwd("C:/Users/hhs10/mycode/R/assignments")

dataUtil <- list(
  sliceVector = function(vector, n){
    return(vector[1:n])
  },
  parseChart = function(tableBody, textSuplier, rowLen, useHeader=TRUE){
    
    rows <- tableBody %>% html_elements('tr')
    cols <- vector(mode="list", length = rowLen)
    
    for(i in 1:length(rows)){
      
      td <- rows[i] %>% html_elements('td, th')
      
      for(j in 1:length(td)){
        
        rowspan <- td[j] %>% html_attr("rowspan")
        colspan <- td[j] %>% html_attr("colspan")
        text <- td[j] %>% textSuplier %>% html_text2()
        
        if(is.na(colspan)) colspan <- 1
        else colspan <- as.numeric(colspan)
        if(is.na(rowspan)) rowspan <- 1
        else rowspan <- as.numeric(rowspan)
        
        cols <- dataUtil$stackFromLeftEvenly(cols, rowLen, text, rowspan, colspan)
      }
    }
    
    height <- Map(length,cols) %>% unlist %>% min
    sliced <- Map(dataUtil$sliceVector, cols, n=height) #인자를 추가로 받을 수 있음!
    
    ret <- bind_cols(sliced)
    #slice는 해당 행만 선택하거나 제외!!
    #setNames로 열의 이름변경가능!!!
    if(useHeader){
      ret <- ret %>% setNames(str_squish(unlist(slice_head(ret)))) %>% slice(-1)
    }
    return(ret)
  },
  groupTable = function(table, colNamesVector, supplier){
    
    colsToSummerize <- setdiff(colnames(table), colNamesVector)
    
    ret <- table %>% 
      group_by(across(all_of(colNamesVector))) %>% 
      summarise(across(all_of(colsToSummerize), supplier, .names="{.col}"))
  },
  stackFromLeftEvenly = function(tableCols, rowLen, toPut, rowspan, colspan){
    
    mapTable <- list()
    
    curPos <- 1
    height <- length(tableCols[[curPos]])
    
    for(end in 1:(rowLen+1)){
      if(end > rowLen){
        mapTable <- append(mapTable, list(tibble(st=c(curPos),len=c(end-curPos),h=c(height))))
      }
      else if(length(tableCols[[end]]) != height){
        mapTable <- append(mapTable, list(tibble(st=c(curPos),len=c(end-curPos),h=c(height))))
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
  fillMissing = function(df, method = "mean") {
    for(i in 1:ncol(df)) {
      if(any(is.na(df[[i]]))) {
        if(method == "mean") {
          df[[i]][is.na(df[[i]])] <- mean(df[[i]], na.rm = TRUE)
        } else if(method == "median") {
          df[[i]][is.na(df[[i]])] <- median(df[[i]], na.rm = TRUE)
        } else if(method == "mode") {
          df[[i]][is.na(df[[i]])] <- as.numeric(names(sort(table(df[[i]]), decreasing = TRUE)[1]))
        }
      }
    }
    return(df)
  },
  scanHtmlStructure = function(url) {
    all_nodes <- read_html(url) %>% html_elements("*")
    
    tag_info <- tibble(
      tag    = all_nodes %>% html_name(),
      class  = all_nodes %>% html_attr("class"),
      id     = all_nodes %>% html_attr("id"),
      href   = all_nodes %>% html_attr("href"),
      text   = all_nodes %>% html_text2(),
      node   = c(all_nodes)  # 노드 자체 포함!
    )
    return(tag_info)
  }
)
