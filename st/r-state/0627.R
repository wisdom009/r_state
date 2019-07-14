#뷁

ad = read.csv("D:/workspace/r_state/ch07/age.data.csv")
ad
str(ad)
ad$score= ifelse(ad$score == 99, NA, ad$score)
ad$scale= factor(ad$scale)
ad$sex=   factor(ad$sex)

y1=  ad$age[ad$scale == "1"]
y2=  ad$age[ad$scale == "2"]
y3=  ad$age[ad$scale == "3"]
y1
y1.m= mean(y1)
y2.m= mean(y2)
y3.m= mean(y3)

sse.1 =sum((y1 - y1.m)^2)
sse.2 =sum((y2 - y2.m)^2)
sse.3 =sum((y3 - y3.m)^2)

sse = sse.1 +sse.2+sse.3
dfe = (length(y1) -1) + (length(y2) -1) + (length(y3) - 1)

y.m = mean(ad$age)

sst.1 = length(y1) * sum((y1.m - y.m)^2)
sst.2 = length(y2) * sum((y2.m - y.m)^2)
sst.3 = length(y3) * sum((y3.m - y.m)^2)

sst = sst.1 + sst.2 +sst.3
dft = length(levels(ad$scale)) -1

mst = sst / dft
mse = sse / dfe
f.t = mst / mse
f.t
alpha = 0.05
tol = qf(1 - alpha,2,147)
tol
p.v = 1 - pf(f.t,2,147)
p.v


x = seq(0,4, by = 0.01)
yf = df(x,2,147)
par(mar = c(2,1,1,1))
plot(x, yf, type = "l", ylim = c(-0.1,1), xlab = "",ylab = "", axes = F) 
abline(h=0) #아래 선 
tol.r = round(tol,2)
polygon(c(tol.r, x[x >= tol.r], 4),c(0,yf[x>=tol.r],0),col="red") #색칠
arrows(tol, 0.3, tol, 0.08, length = 0.1) 
text(tol, 0.32,
     paste("P(F(2"))
