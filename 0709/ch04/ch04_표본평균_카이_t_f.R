library(ggplot2)
library(prob)
library(gridExtra)

c4 <- rep(NA, 1000)
c16 <- rep(NA, 1000)
c64 <- rep(NA, 1000)
c256 <- rep(NA, 1000)

for ( i in 1:1000) {
  c4[i] <- mean(rnorm(4))
  c16[i] <- mean(rnorm(16))
  c64[i] <- mean(rnorm(64))
  c256[i] <- mean(rnorm(256))
  }

### 카이 분포 ###
cg1 <- ggplot(data.frame(x=c4), aes(x=x)) +
  stat_function(fun=dchisq, args=list(df=1), colour="black", size=1.2) +
  geom_text(x=1, y=0.5, label="c4") +
  ggtitle("카이분포1(표본크기=4,자유도=1)")

cg2 <- ggplot(data.frame(x=c16),aes(x=x)) +
  stat_function(fun=dchisq, args=list(df=1), colour="blue", size=1.2) +
  geom_text(x=0.5, y=0.55, label="c16",col="Blue") +
  ggtitle("카이분포2(표본크기=16,자유도=1)")

cg3 <- ggplot(data.frame(x=c64),aes(x=x)) +
  stat_function(fun=dchisq, args=list(df=1), colour="red", size=1.2) +
  geom_text(x=0.2, y=1.2, label="c64",col="red") +
  ggtitle("카이분포3(표본크기=64,자유도=1)")

cg4 <- ggplot(data.frame(x=c256),aes(x=x)) +
  stat_function(fun=dchisq, args=list(df=1), colour="orange", size=1.2) +
  geom_text(x=0.1, y=1, label="c256",col="orange") +
  ggtitle("카이분포4(표본크기=256,자유도=1)")

grid.arrange(cg1, cg2,cg3,cg4,nrow=2,ncol=2)


###t분포###

tg1 <- ggplot(data.frame(x=c4), aes(x=x)) +
  stat_function(fun=dt, args=list(df=1), colour="black", size=1.2) +
  geom_text(x=0.5, y=0.3, label="c4",col="black") +
  ggtitle("t 분포1(표본크기=4,자유도=1)")

tg2 <- ggplot(data.frame(x=c16), aes(x=x)) +
  stat_function(fun=dt, args=list(df=1), colour="blue", size=1.2) +
  geom_text(x=0.5, y=0.3, label="c16",col="blue") +
  ggtitle("t 분포2(표본크기=16,자유도=1)")

tg3 <- ggplot(data.frame(x=c64), aes(x=x)) +
  stat_function(fun=dt, args=list(df=1), colour="red", size=1.2) +
  geom_text(x=0.3, y=0.3, label="c64",col="red") +
  ggtitle("t 분포3(표본크기=64,자유도=1)")

tg4 <- ggplot(data.frame(x=c256), aes(x=x)) +
  stat_function(fun=dt, args=list(df=1), colour="orange", size=1.2) +
  geom_text(x=0.13, y=0.315, label="c256",col="orange") +
  ggtitle("t 분포4(표본크기=4,자유도=1)")

grid.arrange(tg1, tg2,tg3,tg4,nrow=2,ncol=2)

### F 분포 ###

ggplot(data.frame(x=c4), aes(x=x)) +
  stat_function(fun=df, args=list(df1=5, df2=10), colour="blue", size=1) +
  geom_text(x=0.5, y=0.3, label="c4",col="black") +
  ggtitle("f 분포1(표본크기=4,자유도=1)")

ggplot(data.frame(x=c16), aes(x=x)) +
  stat_function(fun=df, args=list(df1=5, df2=10), colour="blue", size=1) +
  geom_text(x=0.5, y=0.3, label="c4",col="black") +
  ggtitle("f 분포1(표본크기=4,자유도=1)")

  