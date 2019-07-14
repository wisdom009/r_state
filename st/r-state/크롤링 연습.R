
library(rvest)
library(stringr)
library(dplyr)
trim <- function (x) gsub("^\\s+", "", x)

url_base <- 'https://kr.investing.com/crypto/bitcoin/news#tab'
url <- paste(url_base,encoding="euc-kr",sep='')
html <- read_html(url)

html1 <- html_nodes(html, '.js-content-wrapper')
html2 <- html_nodes(html1, '.textDiv')

trim <- function (x) gsub("^\\s+", "", x)
ps <- html_nodes(html2, 'p')
as <- html_nodes(html2, 'a')
dt = html_nodes(html2, '.date')
reple <- c()
score <- c()
data = c()
title <- html_nodes(html2, 'a') 
score <- c(score, trim(html_text(title))) 
score_reple <- html_nodes(html2, 'p')
reple <- c(reple, trim(html_text(score_reple))) 
dt = html_node(html2, '.date')
data = c(data,trim(html_text(dt)))
score
reple
data
trim = function(x) gsub("^\\s+ ", "" , x)
data = gsub("년","/",data)
data = gsub("월","/",data)
data = gsub("일","",data)
data = gsub("<U+00A0>","",data)
data = gsub(" ","",data)
data = gsub("-","",data)
score =gsub("가격이","",score)
score =gsub("선","",score)
score
data
review = data.frame(score,  data, reple)
review


mergeUserDic(data.frame(readLines("영화_황해"), "ncn"))
txt <- readLines("영화_황해.txt") 
