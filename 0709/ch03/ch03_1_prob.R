install.packages("prob")
library(prob)
rolldie(2)
urnsamples(1:3,size = 2)
urnsamples(1:3,size = 2,replace = T)

tosscoin(2, makespace = T)

x <- c(0,1,2)
px <- c(1/4,2/4,1/4)
EX <- sum(x*px)

x2 <- x*x
EX2 <- sum(x2 * px)
varx <- EX2 - EX^2
varx
