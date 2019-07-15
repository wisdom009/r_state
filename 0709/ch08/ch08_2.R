# 8-2 동질성 검정 독립성 검정
# 예제 -2 . SNS(동질성)

sns.c <- read.csv("data/snsbyage.csv",header = T,stringsAsFactors = FALSE)

sns.c <- transform(sns.c, age.c = factor(age, levels = c(1,2,3),
                                         labels = c("20대","30대","40대")))

sns.c <- transform(sns.c, service.c = factor(service, levels = c("F","T","k","c","e"),ordered = TRUE))

c.tab <- table(sns.c$age.c, sns.c$service.c)

a.n <- margin.table(c.tab, margin = 1)
s.n <- margin.table(c.tab, margin = 2)
s.p <- s.n / margin.table(c.tab)
expec <- a.n %*% t(s.p)
o.e <- c.tab-expec
t.t <- sum((o.e^2)/expec)


# 독립성 검정
# 예제-3. 성별에 따른 대학원

data("UCBAdmissions")
UCBAdmissions
ucba.tab <- apply(UCBAdmissions,c(1,2),sum)
ucba.tab
round(prop.table(ucba.tab, margin = 2)* 100,1)

# 독립성 검정
a.n <- margin.table(ucba.tab, margin = 1)
g.n <- margin.table(ucba.tab, margin = 2)

a.p <- a.n / margin.table(ucba.tab)
g.p <- g.n / margin.table(ucba.tab)

