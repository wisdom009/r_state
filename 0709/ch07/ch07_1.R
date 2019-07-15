# 7장
# 7-1 모집단이 두개인 경우  
data <- read.table("data/chapter7.txt",header = T)
data2 <- data.frame(gender=c(1,1), weight=c(3350,3380))
data <- data
boy <- subset(data, gender==1)
girl <- subset(data, gender==2)


# 정규성 테스트 
shapiro.test(girl$weight)  # p-value > 0.05 , 정규성 o
qqnorm(girl$weight)    
qqline(girl$weight)

shapiro.test(boy$weight)  # p-value < 0.05 , 정규성 x 
qqnorm(boy$weight)
qqline(boy$weight)

# 등분산성 테스트
var.test(data$weight ~ data$gender)      # ( y축 , x축 )

# 2-sample t 
t.test(data$weight ~ data$gender, mu=0,alternative="less", var.equal=TRUE) # 여아 몸무게 평균 - 남아 몸무게 평균

# paired data
# 예제 2. 식욕부진증 치료요법의 효과검정
install.packages("psych")
library(psych)
library(PairedData)
data("Anorexia")

data <- Anorexia
summary(data)
describe(data)
anorexia
Anorexia

n <- length(data$Prior - data$Post)
m <- mean(data$Prior - data$Post)
s <- sd(data$Prior - data$Post)
t.t <- m/(s / sqrt(n))

alpha <- 0.05
qt(alpha,df=16)
pt(t.t, df=16)    # 검정통계량으로부터 구한 유의 확률


t.test(data$Prior, data$Post, paired = T, alternative = "less")




