data = read.table("D:/workspace/r_state/ch07/chapter7.txt", header = T)
boy = subset(data, gender == 1)
boy
girl = subset(data, gender == 2)
girl
var.test(data$weight ~ data$gender)

shapiro.test(boy$weight)
shapiro.test(girl$weight)
qqnorm(girl$weight)
qqline(girl$weight)

qqnorm(boy$weight)
qqline(boy$weight)

iris= subset(iris, Species == 'setosa')
qqnorm(iris$Sepal.Length)
qqline(iris$Sepal.Length)


#2-sample T

t.test(data$weight ~ data$gender,mu = 0, alternative ="less",
       var.equal=T)

data = read.table("D:/workspace/r_state/ch07/chapter7.txt", header = T)
data2 = data.frame(gender = c(1,1),weight = c(3350,3380))
data = data.frame(data, data2) # 합체



boy = subset(data, gender == 1)
boy
girl = subset(data, gender == 2)
girl
var.test(data$weight - data$gender, mu=0, var.equal=T)

#비교해서 나온 pvalue로 계산




#모집단이 2개
install.packages("PairedData")
library(PairedData)
data(Anorexia)
Anorexia
data = anorexia
str(anorexia)

install.packages('psych')
library(psych)
summary(data)
describe(anorexia)


n=length(data$Prior - data$Prior)
m=mean(data$Prior - data$Prior)
s=sd(data$Prior - data$Prior)
(t.t = m/(s/ sqrt(n)))

alpha = 0.05
qt(a, df=16)
pt(t.t, df=16)

t.test(data$Prior, data$Prior, paired = T, alternative = "less")


모집단이 3개 이상  