# ch09-1 회귀 분석

# 예제 9-2 아버지와 아들 의 키
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",header = T, stringsAsFactors = FALSE)

hf$Gender <- factor(hf$Gender, levels = c("M","F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father","Height")]

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father-f.mean) * (hf.son$Height- s.mean))
cov.xy <- cov.num / (nrow(hf.son) -1 )

cov(hf.son$Father, hf.son$Height)

r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height))
cor(hf.son$Father,hf.son$Height)

plot(hf$Height, hf$Father)

out <- lm(Height ~ Father, data = hf.son)
summary(out)

# ch09-2 





# ch09-3 
#polynomial regression

women
plot(weight ~ height, data = women)
fit <- lm(weight ~ height, data = women)
abline(fit, col="red", lwd=2)

summary(fit)


# ch09-4 
state.x77
states <- as.data.frame(state.x77[,c( "Murder", "Population", 
                                      "Illiteracy", "Income","Forest" )])
