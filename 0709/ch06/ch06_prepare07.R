# ready for ch7
data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header = F)

nrow(data)
ncol(data)

names(data) <- c("time","gender","weight","minutes")
names(data)[1] <- "time.24Hrs"

g1 <- data$gender
g2 <- data[,2]

g3 <- data["gender"]
g4 <- data[[2]]
g5 <- data[["gender"]]

gg1 <- data[, c(2,4)]
gg2 <- data[c("gender","minutes")]

str(data[data$gender==2, ])
str(subset(data, gender==2))

male.m <- mean(data$weight)
str(data[data$gender==2 & data$weight > male.m, ])
