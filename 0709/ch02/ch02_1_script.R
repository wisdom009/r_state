
library(ggplot2)
cars
plot(cars$speed, cars$dist, main = "속도와 제동거리",
     xlab="속도", ylab="제동거리",pch=1,col="blue")
plot(jitter(cars$speed),jitter(cars$dist), main = "속도와 제동거리",
     xlab="속도", ylab="제동거리",pch=1,col="blue")
par(mfrow=c(1,1))
data1 <- c(1871:1970)
data <- data.frame(Nile)
d <- cbind(data,data1)
# d$time <- data1
colnames(d) <- c("Nile","ts")
plot(Nile,type="p")
plot.ts(Nile)
ggplot(data = d, aes(x=ts,y=Nile)) + geom_line()

df_Nile <- data.frame(date=time(Nile), y=as.matrix(Nile))
head(df_Nile)



data <- read.csv("2010년 인구사항.csv", header =F,na.strings=c("."))
str(data)
data$V1 <- factor(data$V1, levels = c(1,2), labels = c("남","여"))
data$V3 <- factor(data$V3, levels = 1:14,
                  labels = c("가구주","가구주의 배우자","자녀","자녀의 배우자","가구주의 부모",
                             "손자녀, 그 배우자", "증손자녀", "배우자의 부모",
                             "조부모", "형제자매, 그 배우자", "형제자매의 자녀, 그 배우자",
                             "부모의 형제자매,그 배우자","기타 친인척","그외같이사는사람"))
data$V4 <- factor(data$V4, levels = 1:8,
                  labels = c("안받았음","초등학교","중학교","고등학교","대학-4년제 미만","대학-4년제 이상","석사과정","박사과정"))

tableV5 <- table(data$V5)
barplot(tableV5,main = "출생아별 빈도", xlab="출생아수",ylab="빈도")


hist(data$V2, main = "연령별분포", xlab="연령",ylab="빈도")
hist(data$V2,breaks=c(seq(0,90,10)),right=F, main = "연령별분포", xlab="연령",ylab="빈도")
hist(data$V2,probability=T, main = "연령별분포", xlab="연령",ylab="밀도")


table(data$V4)
pie(table(data$V4),main = "학력수준별", cex=0.8)
