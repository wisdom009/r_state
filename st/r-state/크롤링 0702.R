url_base <- 'https://movie.naver.com/movie/bi/mi/point.nhn?code=163788#tab'
url <- paste(url_base,encoding="euc-kr",sep='')
html <- read_html(url)
head(html)
html

html1 <- html_nodes(html, '.obj_section')
html2 <- html_nodes(html1, '.score_result')
html2
lis <- html_nodes(html2, 'li')
lis
score <- c()
reple <- c()
company <- c()
name <- c()
for (li in lis) {
  star_score <- html_node(li, '.star_score')
  score <- c(score, trim(html_text(star_score, 'em')))
  score_reple <- html_node(li, '.score_reple')
  reps <- trim(html_text(score_reple, 'p'))
  reps <- str_split(reps, "\r\n")
}
