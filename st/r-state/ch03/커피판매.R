rani = read.csv("D:/workspace/r_state/ch02/cafedata.csv", stringsAsFactors = F)
library(ggplot2)
str(rani)
head(rani)
summary(rani)
dim(rani)
rani$Coffees = as.numeric(rani$Coffees)
sort(rani$Coffees)[1]
sort(rani$Coffees, decreasing = T)


min(rani$Coffees, na.rm = T)
max(rani$Coffees, na.rm = T)   # na.rm = T  na 값을 지우고 출력


stem(rani$Coffees) # 최빈값 4

hist(rani$Coffees)

rc = rani$Coffees 

weights = 1 / (length(rc) -1)  # na 항목이 있으면 개수를 빼줘야함

sum(rc * weights, na.rm = T)   #평균

mean(rc, na.rm = T)

weights
rc
rc[rc == max(rc, na.rm = T)]  = 480
rc

mean(rc, na.rm = T)

length(rc)
median.idx = (1 + length(rc)-1)/ 2
sort(rc)[median.idx]  # 중앙값
median(rc, na.rm = T)


height =c(164,166,168,170,172,174,176)
height.m=mean(height)
h.dev = height - height.m
h.dev
sum(h.dev)

h.dev2 = h.dev**2
h.dev2
sum(h.dev2)
variance = sum(h.dev2)/length(height)
variance
standard_deviation =sqrt(variance)
standard_deviation

mean(height)
var(height)
sd(height)

qs <- quantile(rc, na.rm = T)
qs
qs[4] - qs[2]         # 3분위수 - 1분위수 --> IQR(InterQuantile Range)
IQR(rc, na.rm = T)

bp <- boxplot(rc, main="커피 판매량에 대한 상자도표", axes=F)

# 이상치(Outlier)
boxplot(cars$dist)
qs <- quantile(cars$dist)
qs
iqr <- qs[4] - qs[2]
iqr
upperLimit <- qs[4] + 1.5 * iqr
lowerLimit <- qs[2] - 1.5 * iqr
lowerLimit; upperLimit
cars$dist[cars$dist > upperLimit]    # 이상치
cars$dist[cars$dist < lowerLimit]

ggplot(cars)

range(rani$Coffees, na.rm = T)
diff(range(rani$Coffees, na.rm = T))
diff(range(rani$Coffees, na.rm = T))/30

ggplot(rani, aes(Coffees))+
  geom_histogram(binwidth = 1.5, fill ="black" )


library(dplyr)
rani
ggplot(data = rani, aes(x=rani$Total.Soda.and.Coffee, y=rani$Coffees, colour=Coffees)) +
  geom_point(shape=19, size=3)
ggplot(data = rani, aes(x=rani$Total.Soda.and.Coffee, y=rani$Sodas , colour=Sodas)) +
  geom_point(shape=19, size=3)
ggplot(data = rani, aes(x=rani$Total.Soda.and.Coffee, y=rani$Juices, colour=Juices)) +
  geom_point(shape=19, size=3)


ggplot(data = rani, aes(x=rani$Code, y=rani$Coffees)) +
  geom_bar(stat = "identity")

ggplot(data = rani, aes(x=rani$Day.Code, y=rani$Juices, fill=rani$Juices)) +
  geom_bar(stat = "identity")
         

plot(rani)




