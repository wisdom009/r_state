# 5. 다중회귀모형에서 변수의 선택 ------------------------------------------------------------------------------
# AIC 값으로 최고의 모델 선택
AIC(fit, fit2)

# 변수선택법 - 단계 선택법 

# 1. Backward : 모든 변수부터 영향이 적은 변수를 하나씩 제거해 AIC 값이 가장 낮은 모델 선택 
# 2. Forward  : 상수항부터 시작해 변수를 하나씩 추가해 AIC값이 가장 낮은 모델 선택 

# step() 함수는 AIC 값을 이용하여 단계적 회귀를 수행하는 함수로 forward, backward stepwise regression을 모두 할 수 있다. 
# Backward regression은 가능한 많은 변수에서 시작해 하나씩 제거하는 방법이고,
# Forward regression은 적은 수의 변수에서 시작해 변수를 하나씩 추가하는 방법이다.


# 5.1 Backward regression --------------------------------------------------------------------------------------

# 먼저 가능한 많은 변수들을 포함하는 full.model을 만들고 의미 없는 변수들을 제거하기 위해 step 함수를 사용한다. 
# 그리고 그 결과를 reduced.model에 저장한다.

full.model <- lm(Murder~.,data=states)
backward.model <- step(full.model,direction="backward")


summary(backward.model)

# Forward Regression ----------------------------------------------------------------------------------------
min.model <- lm(crim~1,data=b) 

# Forward selection을 하기 위해서는 대상이 되는 변수들이 어떤 것인지 step() 함수에 알려주어야 한다. 
# scope 인수에 변수들을 알려주고 장황한 출력을 피하기 위해 trace=0을 추가했다.
fwd.model <- step(min.model,direction="forward",
                  scope=(crim ~ dis + rad + nox + zn + medv + black))

fwd.model <- step(min.model,direction="forward",
                  scope=(crim ~ dis + rad + nox + zn + medv + black),trace=0)

summary(fwd.model)


# BIC 값으로 모델을 선택하는 방법 
library(leaps)

subsets <- regsubsets(crim~., data=b,
                      method = "seqrep", nbest = 4)

subsets <- regsubsets(crim~., data=b,
                      method = "exhaustive", nbest = 4)

summary(subsets)
plot(subsets)
