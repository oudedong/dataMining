setUpUtil$setLibs()
rd <- setUpUtil$setRselenium()

eachPage <- function(url){
  rd$navigate(url)
  browerUtil$waitUntilVisible(rd, "table[id='top-games']")
  html <- rd$getPageSource()[[1]] %>% read_html()
  
  tableBody <- html %>% html_element("table[id='top-games']")
  chart <- dataUtil$parseChart(tableBody, function(x) x, 6, useHeader = TRUE) %>% 
    rename(rank=na)
  
  links <- dataUtil$htmlCollector(tableBody, function(node){
    temp <- node %>% html_attr("href")
    return(!is.na(temp) && !is.null(temp))
  }) %>% select(text, href)
  
  return(list(chart, links))
}
eachGame <- function(url){
  rd$navigate(url)
  browerUtil$waitUntilVisible(rd, "table.common-table")
  html <- rd$getPageSource()[[1]] %>% read_html()
  
  tableBody <- html %>% html_element("table.common-table")
  chart <- dataUtil$parseChart(tableBody, function(x) x, 5, useHeader = TRUE) %>% 
    dataUtil$auto_cast_df()
}

games <- list()
baseUrl <- "https://steamcharts.com/"
for(page in 1:4){
  url <- paste0(baseUrl,"top/p.",page)
  temp <- eachPage(url)
  ret <- left_join(temp[[1]], temp[[2]], join_by(Name == text))
  games <- c(games, list(ret))
}
ret <- bind_rows(games)
ret <- ret %>% dataUtil$auto_cast_df()
glimpse(ret)
gameDetails <- list()
for(i in 1:100){
  url <- paste0(baseUrl, ret$href[i])
  temp <- eachGame(url)
  gameDetails[[ret$Name[i]]] <- temp
}
Map(dataUtil$replaceString, gameDetails, rep(c("^-&")), rep("0")) -> gameDetails
Map(dataUtil$auto_cast_df, gameDetails) -> gameDetails
gameDetails[[1]] %>% view
