library(rvest)

setUpUtil$setLibs()
rd <- setUpUtil$setRselenium()

rd$navigate("https://sonicboom.kbl.or.kr/match/team-ranking")

# 1. 페이지 로딩
html <- rd$getPageSource()[[1]] %>% read_html

# 2. 모든 테이블 요소 추출
tables <- html %>% html_elements("table")

# 3. textSuplier 정의: td 또는 span에서 텍스트 추출
textSuplier <- function(td) {
  td %>% html_element("span") %||% td
}

# 4. 테이블 파싱 (표마다 열 수는 다르지만 대충 최대 열수 10 정도로 추정)
parsedTables <- dataUtil$parseChart_list(
  tableBodies = tables,
  textSupliers = list(function(x){x}),
  rowLens = 10,
  useHeaders = TRUE,
  headerHeights = 1
)

# 5. 각 테이블에 타입 자동 변환
parsedTables <- Map(dataUtil$auto_cast_df,parsedTables)

# 6. 예시 출력
glimpse(parsedTables[[1]])
View(parsedTables[[1]])

#애니메이션 예시
p <- totalStandingTablesMerged %>%
  group_by(Year) %>%
  slice_head(n = 12) %>%
  ungroup() %>%
  mutate(Pos = factor(Pos, levels = rev(1:12))) %>%  # 순위 뒤집기
  ggplot(aes(x = Pos, y = Pts, fill = Driver, group = Driver)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Driver), hjust = -0.5) +
  geom_text(
    aes(x = 0, y = max(Pts), label = as.character(Year)),
    size = 15,
    color = "grey80",
    hjust = 2,
    vjust = 0,
    inherit.aes = FALSE
  ) + 
  geom_text(
    aes(label = as.factor(Pts)),
    size = 7,
    color = "black",
    hjust = -0.1
  ) + 
  labs(title = 'F1 Driver Wins Ranking 1950-2024', x = 'Ranks', y = 'Points') +
  guides(fill = "none") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold")) +
  transition_states(Year, transition_length = 2, state_length = 1) +
  ease_aes("quadratic-in-out") + enter_fade() + exit_fade()

animate(p, duration = 20, fps = 30, width = 800, height = 600, renderer = gifski_renderer("f1.gif"))