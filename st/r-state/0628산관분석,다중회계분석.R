#cov(x,y)  = e[(x-e(x)) * (y-e)] 
#
hf = read.table("http://www.randomservices.org/random/data/Galton.txt",
                header = T, stringsAsFactors = F)
str(hf)
hf$Gender= factor(hf$Gender, levels = c("M","F"))
hf.son = subset(hf, Gender == "M")
hf.son = hf.son[c("Father", "Height")]
f.mean = mean(hf.son$Father )
s.mean = mean(hf.son$Height )
cov.num = sum(hf.son$Father-f.mean) * (hf.son$Height - s.mean)
(cov.xy = cov.num  / (nrow(hf.son)-1))
cov(hf.son$Father, hf.son$Height)
(r.xy = cov.xy/(sd(hf.son$Father)* sd(hf.son$Height)))
cor(hf.son$Father, hf.son$Height)



#회귀계수
plot(hf$Height, hf$Father)
hf$Gender = factor(hf$Gender, levels=c("M","F"))
hf.son = subset(hf, Gender == "M")
hf.son = hf.son[c("Father", "Height")]

mean.x = mean(hf.son$Father) 
mean.y = mean(hf.son$Height)

sxy = sum((hf.son$Father - mean.x)* (hf.son$Height-mean.y))
sxx = sum((hf.son$Father - mean.x)^2)
b1 = sxy /sxx
b0 =mean.y - b1*mean.x


#lm()함수 이용  lm(y,x)
out = lm(hf.son$Height ~ hf.son$Father, data=hf.son)
summary(out)
anova(out)


#선형 회귀 구하고 확인
#정규성 점의 나열이 일정한지
women

plot(weight ~ height, data = women)
fit = lm(weight ~ height, data= women) #리니어모델
abline(fit, col='red', lwd=2)




#추가 정보
summary(fit)
cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit) # 정규성2-o ,독립성, 선형성1-x,등분성3-o
par(mfrow=c(1,1))

fit2 = lm(weight~height + I(height^2),data=women) # 2차식
plot(weight ~ height, data = women)
abline(fit, col='red', lwd=2)
lines(women$height, fitted(fit2), col="green", lwd=2)

fit3 = lm(weight~height + I(height^2) + I(height^3),data=women) # 3차식
plot(weight ~ height, data = women)
lines(women$height, fitted(fit3), col="cyan", lwd=2)

summary(fit3)
par(mfrow = c(2,2))
plot(fit3)
par(mfrow=c(1,1))


# AIC 가 작을수록 좋음 
AIC(fit2)
AIC(fit3)



#다중 회귀 
state.x77
head(state.x77)
states= as.data.frame(state.x77[,c("Murder", "Population",
                                   "Illiteracy", "Income", "Frost")])
fit = lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
summary(fit) 
par(mfrow = c(2,2))
plot(fit)
par(mfrow=c(1,1))


fit1 = lm(Murder ~ ., data=states) # ~ . 나머지 변수 모두를 선택
par(mfrow = c(2,2))
plot(fit1)
par(mfrow=c(1,1))


fit2 = lm(Murder ~ Population+Illiteracy, data=states)
summary(fit2)

AIC(fit1,fit2)
step(fit1,direction = 'backward')

#backward 모든변수부터 p갑이 제일크면 하나씩 제거
#forward
fit3 = lm(Murder ~1,data=states)
step(fit1,direction = "forward",
     scope = ~ Population+Illiteracy+Income+Frost)
step(fit1,direction = "forward", scope = list(upper=fit1,lower=fit3))

install.packages("leaps")
library(leaps)

subsets = regsubsets(Murder ~ .,data=states,
                    method = 'seqrep',nbest = 4)
subsets = regsubsets(Murder ~ .,data=states,
                    method = 'exhaustive',nbest = 2)

summary(subsets)
plot(subsets)


data = read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
str(data)
head(data)

data$rank = as.factor(data$rank)

train = data[1:200, ]
train = data[200:400, ]
model = glm(admit ~ gre + gpa + rank, data = data, family = "binomial")
summary(model)

model2 = glm(admit ~ gre +  rank, data = data, family = "binomial")
summary(model)


AIC(model, model2)
