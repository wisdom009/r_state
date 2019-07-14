install.packages('rJava')
install.packages('KoNLP')
library(tm)
useSejongDic()
buildDictionary(ext_dic = "woorimalsam")
install.packages("tm")
install.packages("")

library(KoNLP)
library(wordcloud)
library(RColorBrewer)
setwd("D:/R_data/st/r-state")
data= readLines("영화.txt",encoding="euc-kr")

data1 = sapply(data, extractNoun, USE.NAMES = F)

data2 = unlist(data1)

data2= Filter(function(x) {nchar(x) >= 2}, data2)
wc = table(data2)
head(sort(wc, decreasing = T),20)

head(unlist(data2),30)

data2 = gsub("씨네2","알라딘",data2)
data2 = gsub("씨네","사막",data2)
data2 = gsub("제목","아바브와브",data2)
data2 = gsub("\t","",data2)
data2 = gsub("김송","꿀잼",data2)
data2 = gsub("김성","보물",data2)
data2 = gsub("대비","실사",data2)
data2 = gsub("박평","평가",data2)
data2 = gsub("선방","평점",data2)
data2 = gsub("New","",data2)
data2 = gsub("World","NewWholeWorld",data2)
data2 = gsub("얼렁뚱","노래",data2)
data2 = gsub("임수","",data2)
data2 = gsub("원작","알리바바",data2)
data2 = gsub("지니로","지니",data2)
data2 = gsub("캐릭터","주인공",data2)
data2 = gsub("화려","대박",data2)
data2 = gsub("이용","프린스",data2)


write(unlist(data2), "영화1.txt")
data4=read.table("영화1.txt", encoding="euc-kr")
wc = table(data4)
head(sort(wc, decreasing = T),20)


palete = brewer.pal(10, "Set3")
wordcloud(names(wc),freq=wc,scale=c(1,0.5),rot.per=0.25,min.freq=1,
          random.order=F,random.color=T,colors=palete)

