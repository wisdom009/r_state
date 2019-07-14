# Selenium 설치와 크롤링
library(RSelenium)
library(rvest)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
ch = wdman::chrome(port=4445L)
remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open() #열림

remDr$navigate("https://nid.naver.com/nidlogin.login")
txt_id <- remDr$findElement(using="css selector", value="#id") #(using="css selector"", value="#id"),"using="id", value="id"
txt_pw <- remDr$findElement(using="id", value="pw") #필드에 필요한 요소 입력,로그인에 필요한 요소 입력 
login_btn <- remDr$findElement(using="class", value="btn_global")

txt_id$setElementAttribute("value", "hangul1115") # 아이디 입력
txt_pw$setElementAttribute("value", "*****") # *에 비밀번호 입력 
# id랑 pw는 열린 창에서 입력 가능 
login_btn$clickElement() 

remDr$navigate("https://mail.naver.com/")
mail_texts <- remDr$findElement(using="id", value="list_for_view") #메일창전체 id값이  list_for_view
# (using = 'css selector', "subject")
mail_texts
mail_texts <- mail_texts$getElementText()
tmp <- str_split(mail_texts, '\n') %>% .[[1]]



sender <- c()
subject <- c()
time <- c()
for (i in 1:20) {
  sender <- c(sender, tmp[4*i-3]) # #1번째 
  subject <- c(subject, tmp[4*i-2])  #2번째
  time <- c(time, tmp[4*i-1]) # 3번째
}
# 서로 교차로 1번부터 ㄷ번가지 배열번 후 다시 4번부터 제몰 서브타이틀 시간 순으로 나열
df_mail <- data.frame(sender=sender, subject=subject, time=time)
df_mail
remDr$close() # 마지막에는 항상 닫기 입력해주기

#subjects<-unlist(lapply(mail_subjects, function(x){x$getElementText()}))
#subjects
