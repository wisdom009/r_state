#8-1
mt=mtcars
car1 = subset(mtcars, am == 1)
car1
car2 = subset(mtcars, am == 0)
var.test(mt$mpg  ~  mt$am)
#mtcars에서 차의 기어종류에따른 mpg통계가 유의한가?
#차의 기어 종류를 확인 후 mpg의 am에 따른 p데이터 추출
# cncnfgks p 데이터는 mu > 0.5인 0.06이다 따라서 h0로 데이터는 유효하다

# 8-2

library(MASS)
Cars93
Cars93$Origin == 'USA'
Cars93$Origin == 'non-USA'
t.test(Cars93$Price ~ Cars93$Origin ,mu = 0, alternative ="less",
       var.equal=T)
USA =18.5
non-USA= 20.5
p-value = 0.1684

# cars93데이터중 생산국의 값이 USA과non-USA의 가격 평균차이검정
# 검토 결과 p데이터는 0.16으로 h0에 해당 평균과 차이가 없음



#8-3
library(ggplot2)
mpg

mpg$class == c('subcompact','midsize')
mpg$class == 'midsize'
mpg$hwy
t.test(mpg$hwy ~ mpg$class == c('subcompact','midsize'),
       mu = 0,       alternative = "less",
       var.equal=T)
# subcompact 자동차와 midsize 자동차의 고속도로 연비
# 검토 결과 p데이터는 5.3으로 h1이다


#8-3-2
mpg$fl == c('r','p')

t.test(mpg$cty ~ mpg$fl == c('r','p'),
       mu = 0,       alternative = "less",
       var.equal=T)
# 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
# 검토 결과 p데이터는 0.8으로 h1이다

t.test(mpg$hwy ~ mpg$class == c('subcompact','midsize'),
       mu = 0,       alternative = "less",
       var.equal=T)

#8-3-3

mpg$drv == c('f','r' )
mpg$class == 'subcompact'

t.test(mpg$class == 'subcompact'~  mpg$drv == c('f','r' ),
       mu = 0,       alternative = "less",
       var.equal=T)

#subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
#검토 결과 p데이터는 0.02이다 따라서 h0이다.

d= mpg$drv == c('f')
f= mpg$drv == c('r' )
var.test(d,f)

describe(anorexia)

sam = c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
ple = c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52., 49.9, 52.5, 53.0)
di = sam  - ple
di
mean(di)
sd(di)

t.test(sam, ple, paired = T, alternative = "greater",conf.level = 0.95)


s=c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
h=c(14, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

su = s - h
su
mean(su)
sd(su)
t.test(s,h, alternative = 'less', mu =0 )


#두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서 
#10의 소년에게 한족은 a원료의 깔창을 다른 한쪽은 b원료의 갈창을 주고 일정시간이 지난후
#각 신발의 닳은 정도의 차이를 비교 분석후 차이를 검정
#추출한 데이터는 p데이터가 0.3으로  차이가 거의 없는 정로이다.
#닳은 정도의 평균이 10.6, 11 으로 조금의 차이가 있지만 큰 차이가 없는 h0에 속한다

