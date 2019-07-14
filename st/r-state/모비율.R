# 모비율

data= read.table("D:/workspace/r_state/ch06/restitution.txt", header = T)

rel = ifelse(data$rst < 0.4134 | data$rst >0.4374,1,0)

n= length(rel)
nos= sum(rel)
sum
sp = nos/n
hp=0.1
z = (sp/-hp) /sqrt(hp* (1-hp))
a = 0.05
c.u = qnorm(1-a)
p.value = 1- pnorm(z)
prop.test(nos, n , p= 0.1 , alternative = "greater", correct = F)

par(mar=c(0,1,1,1))
x= seq(-3,3,by=0.001)
y= dnorm(x)
plot(x,y,type = "l",axes = F, ylim = c(-0.02, 0.4),main = "" , xlab = "z",ylab="")
abline(h=0)
polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col =2)
text( c.u, -0.02, expression(z[0.05] == 1.645))

polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), density = 20, angle = 45)
text(z, -0.02, paste("z=", round(z, 3)))
text(1.2, 0.3 , paste("p(Z>z)=", round(p.value,3)), cex=0.8)


