# 2sample T 
#paired T
#대응표본 2개집단 처리전 후 표시 까지 배웠음

# 명목형 번수= 범주형자료 각 범주의 개수를 세는 것

#범주형자료 검정

x= seq(0,15,by=0.01)
d= dchisq(x, df =3)

a = 0.05
tol = qchisq(0.95, df=3)
par(mar=c(0,1,1,1))
plot(x,d,type = "l", axes = F, ylim = c(-0.03,0.25), xlab="", ylab = "")
abline(h=0)     
tol.g = round(tol,2)     
polygon(c(tol.g, x[x>tol.g],15), c(0, d[x>tol.g],0), col = "red")
text(0,-0.03, "0", cex = 0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex = 0.8)
polygon(c(tol.g, x[x>tol.g],15), c(0, d[x>tol.g],0), col = "red")


x = c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)


#동질성 검정


sns = read.csv("D:/workspace/r_state/ch08/snsbyage.csv")           
sns = transform(sns, age.c = factor(age, levels = c(1,2,3),
                                    labels = c("20대","30대","40대")))
sns = transform(sns, service.c =
                  factor(service, levels = c("F","T","K","C","E"),
                         ordered = T))

sns
# (3 x 1) (1x5) 매트릭스 매치 불가로 맞추려고 5x1로 만듬 
c.tab = table(sns$age.c, sns$service.c)

a.n = margin.table(c.tab, margin = 1)
s.n = margin.table(c.tab, margin = 2)
s.p = s.n /margin.table(c.tab)
expected = a.n %in% t(s.p)

o.e = c.tab - expected
t.t = sum(o.e)^2 / expected
qchisq(0.95, df= 8)

1- qchisq(t.t, df=8)

chisq.test(c.tab)





#독립성

data("UCBAdmissions")
data
ucba = apply(UCBAdmissions, c(1,2), sum)
ucba
round(prop.table(ucba, margin = 2) * 100, 1)


a.n = margin.table(ucba, margin = 1)
g.n = margin.table(ucba, margin = 2)

a.p = a.n /margin.table(ucba)
g.p = g.n /margin.table(ucba)


expected = margin.table(ucba) * (a.p %*% t (g.p))
  
addmargins(expected)
chisq.t = sum(o.e)
chisq.t
qchisq(0.95,df=1)
1-pchisq(chisq.t, df=1)
  
chisq.test(ucba)

o.e2 = (abs(ucba - expected)- 0.5)^2 / expected  
sum(o.e2)  
