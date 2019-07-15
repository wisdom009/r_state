### 12-1번 문제

# 부모의 키가 클수록 자식의 키도 상대적으로 크다고 하는데, 아버지의 키와 
# 아들의 키를 조사하였더니 아래와 같이 나왔다고 한다. 이 자료를 바탕으로 
# 해서 회귀식을 구하고 아버지의 키가 165cm일 때 아들의 키는 얼마인지 예측하시오.
# 아버지의 키(x):	150  160  170  180  190	아들의 키(y):	176  179  182  181  185

father <- c(150,160,170,180,190)
son <-  c(176,179,182,181,185)

data1 <- data.frame(father, son)
data1

mean.x <- mean(data1$father)
mean.y <- mean(data1$son)

sxy <- sum((data1$father - mean.x)*(data1$son-mean.y))
sxx <- sum((data1$father - mean.x)^2)

b1 <- sxy / sxx
b0 <- mean.y - b1 * mean.x

# 그러므로 회귀직선의 방정식은 다음과 같다.
# son_height_predict = b0 + b1 * father_height
# 식을 이용해 답을 구하면 

146 + 0.2 * 165

#또는 lm을 이용해 값을 구하면 

lm(son ~ father, data = data1)

### 12-2번 문제 
# 소득이 높을수록 신용카드 사용량이 많아진다고 하는데, 월 소득 대비 신용카드 
#사용량을 조사하였더니 아래와 같이 나왔다고 한다. 이 자료를 바탕으로 해서 
# 회귀식을 구하고,월 소득이 250만원일 때 신용카드 사용량을 예측하시오. (단위: 만원)

income <- c(100,200,300,400,500)
mu <- c(30,70,85,140,197)

data2 <- data.frame(income, mu)


mean.x2 <- mean(data2$income)
mean.y2 <- mean(data2$mu)

sxy2 <- sum((data2$income - mean.x2)*(data2$mu - mean.y2))
sxx2 <- sum((data2$income - mean.x2)^2)

b01 <- sxy2 / sxx2
b00 <- mean.y2 - b01 * mean.x2

# 그러므로 회귀직선의 방정식은 다음과 같다.
# card_use_predict = b00 + b01 * income
# 식을 이용해 답을 구하면 

b00 + b01 * 250

#또는 lm을 이용해 값을 구하면 

lm(mu ~ income, data = data2)

### 12-3 번문제 
# mtcars 데이터셋에서 배기량(disp)에 따른 마력(hp)의 회귀식을 구하시오.
library(dplyr)
mt <- mtcars

data3 <- mt %>%
  select(disp, hp)
data3

mean.x3 <- mean(data3$disp)
mean.y3 <- mean(data3$hp)

sxy3 <- sum((data3$disp - mean.x3)*(data3$hp - mean.y3))
sxx3 <- sum((data3$disp - mean.x3)^2)

b001 <- sxy3 / sxx3
b000 <- mean.y3 - b001 * mean.x3

# 회귀식은 다음과 같다. 
# 마력 = b001 + b000 * 배기량
#또는 lm을 이용해 값을 구하면 

lm(hp ~ disp, data = data3)


### 12-4 번문제
# MASS 패키지를 설치하고, 이 패키지 안에 있는 Boston 데이터셋을
# 이용하여 Boston 인근의 집값을 결정하는 다중회귀 모델을 만드시오.

library(MASS)
b <- Boston

# CRIM: 범죄율
# INDUS: 비소매상업지역 면적 비율
# NOX: 일산화질소 농도
# RM: 주택당 방 수
# LSTAT: 인구 중 하위 계층 비율
# B: 인구 중 흑인 비율
# PTRATIO: 학생/교사 비율
# ZN: 25,000 평방피트를 초과 거주지역 비율
# CHAS: 찰스강의 경계에 위치한 경우는 1, 아니면 0
# AGE: 1940년 이전에 건축된 주택의 비율
# RAD: 방사형 고속도로까지의 거리
# DIS: 직업센터의 거리
# TAX: 재산세율
# MEDV : 1,000달러대 자가주택의 중앙값

# Backward regression --------------------------------------------------------------------------------------

full.model <- lm(crim~.,data=b)
backward.model <- step(full.model,direction="backward")

summary(backward.model)

# 결과 값에서부터 rad dis 값을 가져와 그래프를 그려보기로 한다. forward 에서 도출되는 값을 보고  nox 나 medv정도 까지만 해보도록 하자.

# Forward Regression 
min.model <- lm(crim~1,data=b) 
fwd.model <- step(min.model,direction="forward",
                  scope=(crim ~ dis + rad + nox + zn + medv + black))

fwd.model <- step(min.model,direction="forward",
                  scope=(crim ~ dis + rad + nox + zn + medv + black),trace=0)

summary(fwd.model)

# rad 와 dis 는 정말 중요한 값인지 forward에서도 나왔다. 그러나 중요도가 덜한 부분에서 차이가 있었는데 forward에서는 medv가 중요도가 
# 높게 나왔다. 그러므로 rad, dis , medv와 crim의 그래프를 그려보는 것으로 하겠다. 

# 1차식 ( crim ~ rad )

fit_r <- lm(crim ~ rad, data=b)
abline(fit_r, col="red")

summary(fit_r)

par(mfrow = c(2,2))
plot(fit_r)

# 1차식 (crim ~ dis)
fit_d <- lm(crim ~ dis, data=b)
abline(fit_d, col="red")

summary(fit_d)

par(mfrow = c(2,2))
plot(fit_d)

# 1차식 (crim ~ medv)
fit_m <- lm(crim ~ medv, data=b)
abline(fit_m, col="red")

summary(fit_m)

par(mfrow = c(2,2))
plot(fit_m)

# 1차식 그래프는 여기까지

# 2차식 ( crim ~ rad )

fit2_r <- lm(crim ~ rad + I(rad^2), data=b)
plot(crim ~ rad, data=b)

summary(fit2_r)

par(mfrow=c(2,2))
plot(fit2_r)

# 2차식 ( crim ~ dis )

fit2_d <- lm(crim ~ dis + I(dis^2), data=b)

summary(fit2_d)

par(mfrow=c(2,2))
plot(fit2_d)


# 2차식 ( crim ~ medv )

fit2_m <- lm(crim ~ medv + I(medv^2), data=b)

summary(fit2_m)

par(mfrow=c(2,2))
plot(fit2_m)

# 마지막으로 BIC 값으로 그림을 그려 무엇을 선택해야 할지 생각해 보도록 하자

library(leaps)
par(mfrow=c(1,2))
subsets1 <- regsubsets(crim~., data=b,
                      method = "seqrep", nbest = 4)
summary(subsets1)
plot(subsets1)


subsets2 <- regsubsets(crim~., data=b,
                      method = "exhaustive", nbest = 4)

a <- summary(subsets2)$bic
plot(subsets2)
a
plot(a,type = "b")
which.min(a)
coef(subsets2,9)



