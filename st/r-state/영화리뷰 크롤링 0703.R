library(rvest)
library(stringr)
library(dplyr)
library(openxlsx)
url_b = 'https://movie.naver.com'
url_m = '/movie/bi/mi/point.nhn?code=83893'
url = paste0(url_b, url_m)
html = read_html(url)
html %>%
  html_node('iframe.ifr') %>%
  html_attr('src') -> if_url
ifr_url = paste0(url_b, if_url) 
html2 = read_html(ifr_url)

html2 %>%
  html_node('div.score_total') %>%
  html_nodes('em') -> ems
pages = ems[2] %>% html_text()
pages = gsub(",", "", pages)
total_page = ceiling(as.numeric(pages)/10)

lis = html_nodes(html2, 'li')

all.r =c()

for (page in 1:2700) {
  reple  = html_nodes(html2, ".score_reple")
  rep = html_text(reple)
  if(length(rep)==0){break}
  all.r = c(all.r,rep)

all.r = gsub("^\\s+|\\s+$", "", all.r)
all.r = gsub("", "", all.r)
all.r = gsub("\\t", "", all.r)
all.r = gsub("\\r", "", all.r)
all.r = gsub("\\n", "", all.r)
all.r = gsub("\\신고", "", all.r)

}
write(unlist(all.r), "영화_황해.txt")
df_all= data.frame(all.r)
write.xlsx(all.r, file="D:/workspace/r_state", sheetName="평점", 
           col.names=TRUE, row.names=FALSE, append=FALSE)



# --------------------------------------------------


url_base = 'https://movie.naver.com'
start_url = '/movie/bi/mi/point.nhn?code=83893'
url = paste0(url_base, start_url)
html = read_html(url)
html %>%
  html_node('iframe.ifr') %>%
  html_attr('src') -> if_url
ifr_url = paste0(url_base, if_url) 
html2 = read_html(ifr_url)

html2 %>%
  html_node('div.score_total') %>%
  html_nodes('em') -> ems
pages = ems[2] %>% html_text()
pages = gsub(",", "", pages)
total_page = ceiling(as.numeric(pages)/10)

html2 %>%
  html_node('div.paging') %>%
  html_node('a') %>%
  html_attr('href') -> tmp
page_url_base = str_sub(tmp, 1, -2)

df_points = data.frame(score=c(), review=c(), writer=c(), time=c())

for (i in 1:total_page) {
  if (i %% 100 == 0)
    print(i)
  page_url = paste0(url_base, page_url_base, i)
  html = read_html(page_url)
  html %>%
    html_node('div.score_result') %>%
    html_nodes('li') -> lis
  
  score = c()
  review = c()
  writer = c()
  time = c()
  for (li in lis) {
    score = c(score, html_node(li, '.star_score') %>% html_text('em') %>% trim())
    li %>%
      html_node('.score_reple') %>%
      html_text('p') %>%
      trim() -> tmp
    idx = str_locate(tmp, "\r")
    rev = str_sub(tmp, 1, idx[1]-1)
    #print(rev)
    review = c(review, rev)
    tmp = trim(str_sub(tmp, idx[1], -1))
    idx = str_locate(tmp, "\r")
    writer = c(writer, str_sub(tmp, 1, idx[1]-1))
    tmp = trim(str_sub(tmp, idx[1], -1))
    idx = str_locate(tmp, "\r")
    time = c(time, str_sub(tmp, 1, idx[1]-1))
  }
  points = data.frame(score=score, review=review, writer=writer, time=time)
  df_points = rbind.data.frame(df_points, points)
}
write.xlsx(df_points, file="D:/Workspace/R_Project/01_Crawling/cine.xlsx", 
           sheetName="평점", 
           col.names=TRUE, row.names=FALSE, append=FALSE)