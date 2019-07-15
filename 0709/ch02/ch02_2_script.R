library(dplyr)
library(ggplot2)
Rc <- read.csv("cafedata.csv", stringsAsFactors = F)

str(Rc)
Rc$Coffees <- as.numeric(Rc$Coffees)
sort(Rc$Coffees)
head(Rc)
sort(Rc$)
sort(Rc$Coffees, decreasing = T)
sort(Rc$Coffees, decreasing = T)[1]          ##max
min(Rc$Coffees,na.rm = T)
max(Rc$Coffees,na.rm = T)

head(Rc)
stem(Rc$Coffees)
hist(Rc$Coffees)

rc <- Rc$Coffees
weight <- 1 / (length(rc)-1)  ### na값 때문에 숫자가 하나 늘어남
sum(rc * weight,na.rm = T)
mean(rc,na.rm = T)

rc[rc == max(rc, na.rm = T)] <- 480  ## 맥스값에 0하나를 더 붙여 바꾸어 주어라

median.idx <- (1 + length(rc) -1) / 2
sort(rc)[median.idx]
median(rc, na.rm = T)

height <- c(164 , 166, 168, 170, 172, 174 , 176)
height.m <- mean(height)
h.dev <- height - height.m
h.dev

h.dev2 <- h.dev ^ 2
var <- sum(h.dev2) / length(height)
var
stan_var <- sqrt(var) # 표준편차

mean(height)
var(height)
sd(height)


qs <- quantile(rc,na.rm = T)
qs[4] - qs[2]
IQR(rc,na.rm = T)


cs <- cars
boxplot(cs)
qcs <- quantile(cs$dist,na.rm = T)
iqr <- qcs[4] - qcs[2]
iqr
upperlimit <- qcs[4] + 1.5 * iqr
lowerlimit <- qcs[2] - 1.5 * iqr
cars$dist[cars$dist > upperlimit]
cars$dist[cars$dist < lowerlimit]

head(Rc)
Rc <- data.frame(Rc)
Rco <- na.omit(Rc)
Bread_Temp <- Rc %>%
  select(Date,Max.Daily.Temperature..F., Bread.Sand.Sold,Bread.Sand.Waste) 
  
mfrow=c(1,3)
ggplot(Bread_Temp, aes(x=Bread.Sand.Sold, y=Bread.Sand.Waste,fill=Max.Daily.Temperature..F.,color=Max.Daily.Temperature..F.)) + geom_point() 

ggplot(Rc, aes(x=Max.Daily.Temperature..F.,))

Week.Sales <- Rco %>%
  group_by(Day.of.Week) %>%
  summarise(avg = mean(Sales)) 
Week.Sales
ggplot(Week.Sales, aes(x=Day.of.Week, y=avg,fill=avg)) + geom_col()
week <- c("Mon","Tue","Wen","Thu","Fri")
Week.Sales <- transform(Week.Sales, Day.of.Week = factor(Day.of.Week,levels = week))
Week.Sales