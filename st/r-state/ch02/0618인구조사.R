setwd("D:/workspace/r_state")
data1=read.csv("2010인구조사.csv", header= F)

data1$V1 = factor(data1$V1, levels = c(1,2), labels = c("남자","여자"))
data1$V3 = factor(data1$V3, levels = 1:14, labels = c("가구주","가구주의 배우자",
                                                         "자녀","자녀의 배우자",
                                                         "가구주의 부모", "배우자의 부모",
                                                         "손자녀, 그 배우자",
                                                         "증손자녀, 그 배우자","조부모",
                                                         "형제자매, 그 배우자","형제자매의 자녀, 그 배우자",
                                                         "부모의 형제자매, 그 배우자","기타 친인척",
                                                         "그외같이사는사람"))
data1$V3
data1$V4 = factor(data1$V4, levels = 1:8, labels = c("안 받았음","초등학교","중학교",
                                                  "고등학교","대학4년제 미만","대학4년제 이상",
                                                  "석사과정","박사과정"))
str(data1)
save.image("data.rda")
data1 = read.csv("D:/workspace/r_state/data.rda")
tableV5 = table(data1$V5)
tableV5
barplot(tableV5, main= "출생아별 빈도",
        xlab = "출생아수" , ylab = "빈도")

# 히스토그램 -- hist()

hist(data1$V2, main= "연령별 분포포", xlab = "연령", ylab = "빈도")

hist(data1$V2, breaks = c(seq(0, 90, 10)) , right=F ,
     main= "연령별 분포포", xlab = "연령", ylab = "빈도")

par(mfrow=c(1,2))

hist(data1$V2, probability = T ,breaks = c(seq(0, 90, 10)) , right=F ,
     main= "연령별 분포포", xlab = "연령", ylab = "빈도")


tablev4= table(data1$V4)
tablev4
pie( tablev4, main = "학력수준")

pie( tablev4, main = "학력수준", cex=0.8)

#cex = 0.8 각 조각별 이름 크리를 0.8로 축소 1 기본으로 한 값으로 추정





# 참고자료 Nile & cars (ggplot2)

library(ggplot2)
cars
par(mfrow=c(1,2))
plot(cars$speed, cars$dist, main = "cars speed" ,
       xlab = "speed", ylab= "제동거리(ft)" , pch=1 , col="red")
plot(jitter(cars$speed), jitter(cars$dist), main = "cars speed" ,
     xlab = "speed", ylab= "제동거리(ft)" , pch=1 , col="red")
# geom_point()대신 goem_jitter()로도 가능

Nile
plot(Nile, main= "나일강 연별 유량",
     xlab="year", ylab="유량")

plot(Nile, type = "p",main= "나일강 연별 유량",
     xlab="year", ylab="유량")

plot.ts(Nile)
plot.ts(iris)

Nile
df_nile = as.data.frame(Nile)
head(df_nile)
year = c(1871:1970)
df_nile$year = year  #  year에 준 값을 nile$year이라는 값을 만들어서 준

ggplot(df_nile, aes(x = year, y=x))+
  geom_line()


# time series를  데이터프레임으로 변환

df_nile = data.frame(data = time(Nile),
                      y = as.matrix(Nile))  # 이름은 알아서  data / y 

df_nile = data.frame(year = time(Nile),
                     flood = as.matrix(Nile))
ggplot(df_nile, aes(year, flood))+      # 라인 형태
  geom_line()
ggplot(df_nile, aes(year, flood))+      # 포인트 형태
  geom_point()

  # 변환 최종
df_nile = data.frame(year = time(Nile),
                     flood = as.matrix(Nile))

ggplot(df_nile, aes(year, flood))+
  geom_point()


# 히스토그램 = 연속형 자료 
# 개수 또는 비율을 나타내는데 사용



