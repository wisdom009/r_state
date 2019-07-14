# : 확률밀도함수, 또는 확률을 계산할 값을 저장한 벡터의 이름 
#q : 확률밀도함수, 또는 확률을 계산할 값을 저장한 벡터의 이름 
#p : 100p% 백분위수를 계산할 p값의 벡터를 설정 
#n : 발생할 난수의 개수
#df : 자유도
#ncp : 비중심모수(non-centrally parameter) 비중심모수는 음이 될 수 없으며 0인 경우 중심 카이제곱분포 
#log, log.p : 논리값을 설정하며 TRUE이면 확률값은 log(p)로 주어진다.
#lower.tail : 논리값을 설정 

mu= 1/2
sig.sq = 1/12

mu
sig.sq
 n =100
x.bar =NULL 
for (k in 1:10000) {
  x = runif(n)
  x.bar = c(x.bar, mean(x))
}
hist(x.bar, prob=T, col='gray')


m10 = rep(NA,1000)
m40 = rep(NA,1000)

set.seed(1)  # setseed는 는 메모라이즈형식 단 하나만 저장함

for (i in 1:1000) {
  m10[i] = mean(rnorm(10))
  m40[i] = mean(rnorm(40))
}
c(mean(m10),sd(m10))
c(mean(m40),sd(m40))

par(mfrow=c(1,2))
hist(m10, xlim = c(-1.5,1.5), xlab = 'x', ylab = '')
hist(m40, xlim = c(-1.5,1.5), xlab = 'x', ylab = '')

#중심극한정리




n= 1000
r.1.mean = rep(NA, n)
r.2.mean = rep(NA,n)
for (i in 1:n) {
  
}


c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

hist(r.1.mean, prob=T)
x1= seq(min(r.1.mean), max(r.1.mean), length=1000)
y1= dnorm( x = x1, mean=3, sd=(1/sqrt(4)))

lines(x1,y1, lty=2, lwd = 2, col= "blue")


hist(r.2.mean, prob=T)
x1= seq(min(r.1.mean), max(r.1.mean), length=1000)
y1= dnorm( x = x1, mean=3, sd=(1/sqrt(4)))

lines(x1,y1, lty=2, lwd=2, col= "blue")



#-------

t =10  
p = 0.1  
x=0:10
n=1000
b.2.mean = rep(NA, n)
b.4.mean = rep(NA, n)
b.32.mean = rep(NA, n)
b.64.mean = rep(NA, n)

for (i in 1:n) {
  b.2.mean[i] = mean( rbinom(2, size = t, prob = p))
  b.4.mean[i] = mean( rbinom(4, size = t, prob = p))
  b.32.mean[i]= mean( rbinom(32, size = t, prob = p))
  b.64.mean[i]= mean( rbinom(64, size = t, prob = p))
 
}

c(mean(b.2.mean), sd(b.2.mean))
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.32.mean), sd(b.32.mean))
c(mean(b.64.mean), sd(b.64.mean))

par(mfrow= c(2,2))

hist(b.2.mean, prob=T, xlim = c(0,4), col = "darkblue")
x1 = seq(min(b.2.mean),max(b.2.mean), length=1000)
y1 = dnorm( x= x1, mean=1 , sd= sqrt(0.9)/sqrt(2))
lines(x1,y1,lty=2, lwd=2,col="red")

hist(b.4.mean, prob=T, xlim = c(0,2.7), col = "darkblue")
x2 = seq(min(b.4.mean),max(b.4.mean), length=1000)
y2 = dnorm( x= x2, mean=1 , sd= sqrt(0.9)/sqrt(4))
lines(x2,y2,lty=2, lwd=2,col="red")

hist(b.32.mean, prob=T, xlim = c(0.5,1.5), col = "darkblue")
x3 = seq(min(b.32.mean),max(b.32.mean), length=1000)
y3 = dnorm( x= x3, mean=1 , sd= sqrt(0.7)/sqrt(32))
lines(x3,y3,lty=2, lwd=2,col="red")

hist(b.64.mean, prob=T, xlim = c(0.5,1.5), col = "darkblue")
x4 = seq(min(b.64.mean),max(b.64.mean), length=1000)
y4 = dnorm( x= x3, mean=1 , sd= sqrt(0.8)/sqrt(64))
lines(x4,y4,lty=2, lwd=2,col="red")


