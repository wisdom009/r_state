#가설검정
# 1. h0가설 차이가 없음을 알리는 가설 
# 2. h1대안 가설 h1 기존에 알려진것과 차이가 있다고 알리는 가설
# -1 x 는 표본 평균 
# -2 u 는 모집단으니 표본평균ㅇ

data <- read.csv("D:/workspace/R_State/ch06/2010_6차.csv")
str(data)

tmp <- subset(data, data$나이==7 )
tmp
height.p <- tmp$X104.키
height.p
set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

mean(height)
sd(height)
t.test(height, mu=1220)

# p-value = 0.4115/  p- value가  0.05 보다 크면  기준치 즉 채택력 안에 있다
# 가설검정 통계량은 t.test 에서 p-value를 추출해서 p-value가 알파보다 작으면 h0기각 크면 h0 채택
# h0란  기준점 이라고 생각하자

tmp = subset(data, data$나이 == 27)
tmp
heigth.p = tmp$X104.키
heigth.p
set.seed(10)
height = heigth.p[sample(length(heigth.p),15)]
height

mean(height)
sd(height)
t.test(height, mu = 1733)


#6-2

#신생아의 체중
# h0 영가설  신생아의 체중은  (h0 : u몸무게  =2.8kg(2800)) 이다 
# h1 대안가설  신생아의  채중은 2800보다 크가 . (h1: u몸무게 > 2800) 



#6-2 모집단의 가설검정
# 예제 6-1 평균검정

data = read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt", header= T)
str(data)
names(data)=c("time","gender","weight","minutes")
tmp = subset(data, gender == 1)
weigth = tmp[[3]]
weigth
barx = mean(weigth)
s = sd(weigth)
s
n=length(weigth)
n
h0 = 2800
t.t = (barx - h0) / (s / sqrt(n))
t.t
a = 0.05

c.u = qt(1-a, df = n-1)

p.value
(p.value = 1-pt(t.t, df=n-1))
t.test(weigth, mu = 2800 , alternative = "greater")

# 신생아 도안

par(mar = c(0,1,1,1))
x= seq(-3,3,by=0.001)
y=dt(x,df=n-1)
plot(x,y,type = "l",axes = F, ylim = c(-0.02, 0.38),main = "" , xlab = "t",ylab="")
abline(h=0)
polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col =2)
text( c.u, -0.01, expression(t[0.05] == 1.74))
text( 1.8, 0.2 , expression(a == 0.05), cex=0.8)
arrows(1.8, 0.18, 1.8, 0.09, length = 0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t], 0), density = 20, angle = 45)
text(t.t, -0.03, paste("t=", round(t.t, 3)), pos = 4)
text(2.55, 0.1 , expression(a(p)(T>2.233) == 0.0196), cex=0.8)
arrows(2.7, 0.08, 2.5, 0.03, length = 0.05)



