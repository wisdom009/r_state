n <- 6
p <- 1/3
x <- 0:n

 (dbinom(2, size = n,prob = p))
 (dbinom(4,size = n, prob = p))
 (px <- dbinom(x,size = n,prob = p)) 
plot(x,px,type="s",xlab="성공횟수(x)",ylab="확률(P[X=x])",main="asdf")
plot(x,px,type="h",xlab="성공횟수(x)",ylab="확률(P[X=x])",main="asdf",lwd=5,col="red")


pbinom(2, size = n,prob = p)
pbinom(4, size = n,prob = p)
pbinom(4, size = n,prob = p) - pbinom(2, size = n,prob = p)

qbinom(0.1, size = n,prob = p)
qbinom(0.5, size = n,prob = p)

ex = sum(x * px)
ex2 = sum(x^2 * px)
varx <- ex2 -ex^2

n * p
n * p * (1-p)

options(digits = 3)
mu <- 170 
sigma <- 6
ll <- mu - 3*sigma
ul <- mu + 3*sigma

x <- seq(ll, ul, by=0.01)
nd <- dnorm(x, mean = mu, sd=sigma)
plot(x,nd,type="l",xlab="x",ylab="P",lwd=2,col="blue")

pnorm(175,mean=mu, sd=sigma)

options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu,sd=sigma)
c(mean(smp),sd(smp))
hist(smp,prob=T, xlab = "",ylab="")
lines(x,nd,lty=2,lwd=2,col="black")


options(digits = 4)
mu <- 0
sigma <-1 

z <- seq(-3,3,by=0.001)
z.p <- dnorm(z)
plot(z,z.p, axes=F, type="l",ylab="",ylim=c(-0.04,0.4))
axis(1)

lines(c(-3,3),c(0,0))
points(-1.96, -0.02, pch=17, col="red")
text(-1.96, -0.035,  "-1.96", col = "red")

s <- seq(-1.96, 1.96, by=0.001)
s.z <- dnorm(s, mean = 0, sd=1)
s <- c(-1.96,s,1.96)
