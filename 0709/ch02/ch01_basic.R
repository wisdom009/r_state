x <- c(1:5)
factor(x, levels = c(1:4), labels = c("a","b","c","d"),ordered = T)

weekend <- 1:7
weekend <- factor(weekend, levels = c(1:7), labels = c("Mon","Tue","Wen","Thr","Fri","Sat","Sun"), ordered = T)
weekend

name <- c("철","영","길")
age <- c(21,20,31)
gender <- factor(c("M","F","M"))
character <- data.frame(name,age,gender)
str(character)
character

character$name
character[1,3]
