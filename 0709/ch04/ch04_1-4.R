# 4장 

m10 <- rep(NA, 1000)
m40 <- rep(NA, 1000)

set.seed(9)
for (i in 1:1000) {
  m10[i] <- mean(rnorm(10))
  m40[i] <- mean(rnorm(40))
}

options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))
hist(m10, xlim = c(-1.5,1.5), main = "", xlab = "x",ylab = "",
     col = "cyan", border = "blue")
hist(m40, xlim = c(-1.5,1.5), main = "", xlab = "x",ylab = "",
     col = "cyan", border = "blue")


# 4-2 중심극한정리

set.seed(9)

n <- 1000
r.1.mean = rep(NA, n)
r.2.mean = rep(NA, n)

for ( i in 1:n ) {
  r.1.mean[i] = mean( rnorm(4, mean = 3, sd=1))
  r.2.mean[i] = mean( rnorm(4, mean = 170,sd=6)) 
}
options(digits = 4)
c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

hist(r.1.mean,prob=TRUE,  main = "", xlab = "x",ylab = "",
     col = "white", border = "orange")
x1 <- seq(min(r.1.mean),max(r.1.mean),length=1000)
y1 <- dnorm(x=x1,mean=3,sd=(1/sqrt(4)))
lines(x1,y1,lty=2,lwd=2,col="blue")
hist(m40, xlim = c(-1.5,1.5), main = "", xlab = "x",ylab = "",
     col = "cyan", border = "blue")


# 4-3 임의의 표본에서 추출된 표본평균 의 분포

set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.32.mean <- rep(NA, n)
b.256.mean <- rep(NA, n)

for ( i in 1:n ) {
  b.2.mean[i] = mean( rbinom(4, size = t, prob = p))
  b.4.mean[i] = mean( rbinom(16, size = t,prob = p))
  b.64.mean[i] = mean( rbinom(64, size = t,prob = p))
  b.256.mean[i] = mean( rbinom(256, size = t,prob = p))
}

options(digits = 4)
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.16.mean), sd(b.16.mean))
c(mean(b.64.mean), sd(b.64.mean))
c(mean(b.256.mean), sd(b.256.mean))

hist(b.2.mean,prob=TRUE,  main = "", xlab = "",ylab = "",
     col = "white", border = "orange")
x1 <- seq(min(b.2.mean),max(b.2.mean),length=1000)
y1 <- dnorm(x=x1,mean=1,sd=(0.9/sqrt(2)))
lines(x1,y1,lty=2,lwd=2,col="blue")


hist(b.4.mean,prob=TRUE,  main = "", xlab = "",ylab = "",
     col = "white", border = "orange")
x2 <- seq(min(b.4.mean),max(b.4.mean),length=1000)
y2 <- dnorm(x=x2,mean=1,sd=(0.9/sqrt(4)))
lines(x2,y2,lty=2,lwd=2,col="blue")


ggplot(data.frame(x=c(0,10)), aes(x=x)) +
   stat_function(fun=dchisq, args=list(df=1), colour="black", size=1.2) +
   geom_text(x=0.6, y=1, label="df=1") +
   stat_function(fun=dchisq, args=list(df=2), colour="blue", size=1.2) +
   geom_text(x=0, y=0.55, label="df=2") +
   stat_function(fun=dchisq, args=list(df=3), colour="red", size=1.2) +
   geom_text(x=0.5, y=0.05, label="df=3") +
   ggtitle("Chisq-Distribution")




ggplot(data.frame(x=c(-3,3)), aes(x=x)) + 
  stat_function(fun=pt, args=list(df=1), colour="brown", size=1.5) +
  ggtitle("Cumulative t-Distribution : t(1)")

