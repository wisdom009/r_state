#### 이항분포 ####
### 1번 ###
# 다음의 문제가 베르누이 시행인지 판단하시오.
# 1) 영화관에서 줄을 기다리는 시간을 측정한다  
# 2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다.  
# 3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다.
# 4) 주사위를 한 번 던졌을 때, 숫자 2가 나오는지를 체크한다.

# 2번과 4번이 베르누이 시행, 베르누이 시행이랑 p의 확률로 원하는 결과가 나타났을 때를 성공으로 그 외의 상황을 1-p로 하는 결과를 나타내는 확률실험인데 
# 1번은 시간을 측정하는 것이고 2번은 전화를 한사람이 여자일 확률과 그렇지 않을 확률로 나뉜다. 3번은 나오는 숫자를 체크하는 것이고,
# 4번은 숫자 2가 나올확률과 그렇지 않을 확률로 나누어 계산이 가능하다.


### 2번 ###

# 한 축구 선수가 페널티킥을 차면 5번 중 4번을 성공한다고 한다.이 선수가 10번의 페널티킥을 차서 7번 성공할 확률을 구하시오.

n2 <- 10 
p2 <- 4/5
x2 <- 0:n2

pbinom(7,size=n2,prob= p2) - pbinom(6,size=n2, prob= p2)

px2 <- dbinom(x2, size = n2, prob = p2)
plot(x2, px2, type="s", xlab="성공횟수(x)", ylab="확률", main="페널티킥 성공 분포(확률4/5)")

### 3번 ###
# A라는 회사는 스마트폰의 한 부품을 만드는 회사로, 이 A사의 불량률은 5%로 알려져 있다.
# 이 회사의 제품 20개를 조사했을 때, 불량이 2개 이하로 나올 확률을 구하시오.
n3 <- 20
p3 <- 1/20
x3 <- 0:n3

pbinom(2, size = n3, prob = p3)

px3 <- dbinom(x3, size = n3, prob = p3)
plot(x3, px3, type="s", xlab="불량횟수(x)", ylab="확률", main="불량률 분포(확률1/20)")


### 4번 ###
# 어떤 희귀 바이러스에 감염되었을 때, 회복할 수 있는 치료율은 20%라고 한다. 
# 이 바이러스에 감염된 환자 20명을 치료했을 때, 적어도 2명 이상은 회복될 확률을 구하시오.

n4 <- 20 
p4 <- 1/5
x4 <- 0:n4

pbinom(20,size=n4,prob=p4) - pbinom(1,size=n4,prob=p4)
qbinom(0.0000000000001,size = n4,prob = p4)   # 이 확률보다 큰값들의 합

px4 <- dbinom(x4, size = n4, prob = p4)
plot(x4, px4, type="s", xlab="회복률(x)", ylab="확률", main="회복률 분포(확률1/5)")

### 5번 ###
# 주사위 두 개를 던졌을 때, 눈금의 합이 6이 될 확률을 구하시오.
# 합이 6이 되는 상황 -> 1,5  2,4  3,3 -> 1/12

x5 <- c(0, 1) 
px <- c(11/12, 1/12)
EX <- sum(x5 * px)
EX


#### 정규분포 #### 

### 1번 ###
# A라는 전구회사에서 생산하는 전구의 수명은 800일이고 표준편차는 40일인 정규분포를
# 따른다고 한다.  이때 전구의 수명이 750일 이하일 확률을 구하시오.

options(digits = 3)
mu <- 800
sigma <- 40 
ll <- mu - 3*sigma
ul <- mu + 3*sigma

x01 <- seq(ll, ul, by=0.01)
nd01 <- dnorm(x01, mean = mu,sd = sigma)
plot(x01 , nd01, type="l", xlab= "x",ylab="p(X=x)", lwd=2,col="red")

pnorm(750, mean = mu, sd = sigma)


### 2번 ###
# 어느 한 회사에 다니는 종업원들의 근무기간을 조사하였더니, 평균은 11년이고 분산이
# 16년인 정규분포를 따른다고 한다.    

# 1) 20년 이상 근무한 종업원의 비율을 구하시오.   

mu1 <- 11 
sigma1 <- 16
ll1 <- mu1 - 3*sigma1
ul1 <- mu1 + 3*sigma1

x02 <- seq(ll1, ul1, by=0.01)
nd02 <- dnorm(x02, mean = mu1,sd = sigma1)
plot(x02 , nd02, type="l", xlab= "x",ylab="p(X=x)", lwd=2,col="red")

qnorm(0.713,mean = mu1, sd=sigma1)
1-0.713 # 약 29퍼센트 

# 2) 근무연수가 가장 오래된 10%의 종업원은 이 회사에서 몇 년 이상 근무했다고 볼 수 있는가?

qnorm(0.90,mean = mu1, sd=sigma1) #31.5년

### 3번 ###

# 어느 고등학교 3학년 학생들의 수학성적은 평균이 70이고 표준편차가 8인 정규분포를
# 따른다고 한다.  이때 점수가 80점 이상이고 90점 이하인 학생의 비율을 구하시오.

mu2 <- 70
sigma2 <- 8
ll2 <- mu2 - 3*sigma2
ul2 <- mu2 + 3*sigma2

x03 <- seq(ll2, ul2, by=0.01)
nd03 <- dnorm(x03, mean = mu2,sd = sigma2)
plot(x03 , nd03, type="l", xlab= "x",ylab="p(X=x)", lwd=2,col="red")

pnorm(90 ,mean = mu2, sd=sigma2) - pnorm(80 ,mean = mu2, sd=sigma2) # 약10퍼센트

### 4번 ### 
# 확률변수 X가 평균이 1.5, 표준편차가 2인 정규분포를 따를 때, 
# 실수 전체의 집합에서 정의된 함수 H(t)는 H(t) = P(t ≤ X ≤ t+1) 이다.
# H(0) + H(2)의 값을 구하시오.
mu3 <- 1.5
sigma3 <- 2
ll3 <- mu3 - 3*sigma3
ul3 <- mu3 + 3*sigma3

x04 <- seq(ll3, ul3, by=0.01)
nd04 <- dnorm(x04, mean = mu3,sd = sigma3)
plot(x04 , nd04, type="l", xlab= "x",ylab="p(X=x)", lwd=2,col="red")

h0 <-  pnorm(1 ,mean = mu3, sd=sigma3) - pnorm(0 ,mean = mu3, sd=sigma3) 
h2 <-  pnorm(3 ,mean = mu3, sd=sigma3) - pnorm(2 ,mean = mu3, sd=sigma3) 

h2 + h0 # 약 35퍼센트

ph0 <- qnorm(0.227,mean=mu3,sd = sigma3)
ph1 <- qnorm(0.401,mean = mu3, sd=sigma3)
ph2 <- qnorm(0.599,mean=mu3,sd = sigma3)
ph3 <- qnorm(0.773,mean = mu3, sd=sigma3)
ph0;ph1;ph2;ph3

axis(1)
lines(c(-4, 6), c(0.009, 0.009))

points(0.00247, 0.005, pch=17, col="red")
text(0.00247, -0.002, "0", col="red")
points(0.998, 0.005, pch=17, col="red")
text(0.998, -0.002, "1", col="red")
points(2, 0.005, pch=17, col="red")
text(2, -0.002, "2", col="red")
points(3, 0.005, pch=17, col="red")
text(3, -0.002, "3", col="red")

s1 <- seq(0.00247, 0.998, by=0.001)
s.z1 <- dnorm(s1, mean=1.5, sd=2)
s1 <- c(0.00247, s1, 0.998)
s.z1 <- c(0.009, s.z1, 0.009)

s <- seq(2, 3, by=0.001)
s.z <- dnorm(s, mean=1.5, sd=2)
s <- c(2, s, 3)
s.z <- c(0.009, s.z, 0.009)


polygon(s1, s.z1, density=10, col="gold")
polygon(s, s.z, density=10, col="gold")
