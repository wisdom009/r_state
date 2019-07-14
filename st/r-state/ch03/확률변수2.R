#확률 분포함수
#기대값 p 분산 (1-p)   (1-p는 실패값)


n =6
p= 1/3
x =0:n
dbinom(2, size = n,prob = p)
dbinom(4, size = n,prob = p)

px = dbinom(x, size = n, prob = p)

plot(x, px, type = "h", xlab= "성공", ylab= "실패")
plot(x, px, type = "h", xlab= "성공", ylab= "실패", main = "B(6, 1/3)",
     lwd = 10, col = "red")


#  d 어떤 값일때 확률
pbinom(2,n,p)
pbinom(4,n,p)
pbinom(4,n,p) - pbinom(2,n,p)
dbinom(3,n,p) + dbinom(4,n,p)

qbinom(0.1, n,p)
qbinom(0.5, n,p)
#  p 어떤 값의 누적 확률
#  q 어떤 값일때의 확률

rbinom(10,n,p)
set.seed(1234)
rbinom(10,n,p)

# 예제 3-4 분포함수이용 기대값의 분산

n = 6
p=1/3
x=0:n

px = dbinom(x, size = n,prob = p)

ex = sum(x * ex)
ex2 = sum(x^2 *ex)
varx = ex2 -ex^2

n*p               # 이항분포의 기댓값 np
n*p *(1-p)       # 이항분포의 분산np(1-p)

options(digits = 3)
mu = 170
sigma = 6
ll = mu - 3*sigma   # lower limits
lu =mu +3*sigma     $upper limits

x= seq(ll, lu, by=0.01)
nd = dnorm(x, mean = mu, sd=sigma)
plot(x, nd, type='l', xlab="x" ,ylab="p(x*x)", main="표준편차 분포", lwd=2, col="gray")
 
pnorm(mu, mean = mu, sd=sigma)
pnorm(158,mean = mu, sd=sigma)
pnorm(180,mean = mu, sd=sigma) - pnorm(160,mean = mu, sd=sigma)
qnorm(0.25, mean=mu, sd=sigma)
qnorm(0.75, mean=mu, sd=sigma)


# ----------------------------
options(digits =  5)
set.seed(5)
smp = rnorm(400, mean=mu,sd=sigma)
c(mean(smp), sd(smp))


hist(smp, prob = T, xlab = '',ylab = '', lwd=2, col='gray')
lines(x, nd, lty=2, lwd=2,col="red")


options(digits = 4)
mu=0
sigma=1
p0.05=qnorm(0.05, mean = mu, sd=sigma)
p0.025=qnorm(0.025, mean = mu, sd=sigma)
p0.05;p0.025

z=seq(-3,3,by=0.001)
z.p=dnorm(z)
plot (z, z.p, axes=F, type="l",
     ylab="", ylim = c(-0.04, 0.4))
axis(1)
lines(c(-3,3),c(0,0))
points(-1.96,-0.02, pch=17,col="red")
text(-1.96, -0.035, "-1.96", col = "red")
points(1.96,-0.02, pch=17,col="red")
text(1.96, -0.035, "1.96", col = "red")

