setRepositories(ind=1:7)

install.packages("Rselenium")
install.packages("rvest")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyverse")

library(RSelenium)
library(rvest)
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(tidyverse)

rs <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
rs$open()
baseUrl <- "https://sports.daum.net/record/epl/team?season="
ret <- list()
for(year in 2015:2024){
  
  url <- paste0(baseUrl, year, year+1)
    
  rs$navigate(url = url)
  html <- rs$getPageSource()[[1]]
  html <- read_html(html)
  html %>% html_elements(xpath = "//*[@id='recordList']/div/div/table/tbody/tr") -> nodes
  nodes %>% html_elements("span[class='txt_name']") %>% html_text() -> names
  nodes %>% html_elements("td[data-field='game']") %>% html_text() %>% as.numeric() -> games
  nodes %>% html_elements("td[data-field='win']") %>% html_text() %>% as.numeric()-> wins
  nodes %>% html_elements("td[data-field='draw']") %>% html_text() %>% as.numeric()-> draws
  nodes %>% html_elements("td[data-field='loss']") %>% html_text() %>% as.numeric()-> losss
  nodes %>% html_elements("td[data-field='gf']") %>% html_text() %>% as.numeric()-> gfs
  nodes %>% html_elements("td[data-field='ga']") %>% html_text() %>% as.numeric()-> gas
  nodes %>% html_elements("td[data-field='gd']") %>% html_text() %>% as.numeric()-> gds
  nodes %>% html_elements("td[data-field='rank']") %>% html_text() %>% as.numeric()-> ranks

  temp <- tibble(
    order = 1:length(names),
    name = names,
    game = games,
    win = wins,
    draw = draws,
    loss = losss,
    gf = gfs,
    ga = gas,
    gd = gds,
    rank = ranks
    )
  temp2 <- temp %>% slice_head() %>% print() %>% mutate(years = year) %>% print()
  ret[[year-2014]] <- temp2
  #리스트에 모아서 bind로 테이블을 만들자!!!
}
print(ret)
ret <- bind_rows(ret)
glimpse(ret)
view(ret)
ggplot(ret, aes(x=years,y=rank,fill=name))+
  geom_col()+
  geom_text(aes(label = name), hjust = 0, vjust = 0, size = 3, angle = 0)+
  coord_flip()
rs$close()
