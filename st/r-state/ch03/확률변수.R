# 3. 확률
install.packages("prob")
library(prob)
tosscoin(1) # 동전던지기
rolldie(1) #주사위
urnsamples(1:3, size = 2) #
urnsamples(1:3, size = 2, replace = T)

urnsamples(c(rep("R",3), rep("B", 2)) ,size = 2) 

tosscoin(1, makespace = T) # 확률도 같이 계산
 
# 확룰 변서 분산

x =c(0,1,2)
px = c(1/4,2/4,1/4)
ex= sum(x * px)
x2= x^2
ex2= sum(x2 * px)
ex
ex2
x * 2
x * (1:6)
x * (1:4)
vx = sum(x^2 *px) - ex^2
vx

varx= ex2-ex^2
varx
