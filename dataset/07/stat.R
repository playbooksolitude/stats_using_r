#23-1031 tue 09:11

#
library(tidyverse)

#
read_delim("./dataset/07/Album Sales 1.dat") -> album1
read_delim("./dataset/07/Album Sales 2.dat") -> album2

album1;album2
lm(formula = sales ~ adverts, data = album1) -> albumsales.1
summary(albumsales.1)

lm(formula = sales ~ adverts + 
     airplay + attract, data = album2) -> albumsales.2
summary(albumsales.2)

#anova 모형비교분석
anova(albumsales.1, albumsales.2)
anova(albumsales.1, albumsales.2) |> summary() #의미없음


# ----------------------------------------------------------
#사례 진단 (특정 값)
  #잔차 resid()
  #표준화잔차 rstandard()
  #스튜던트와 잔차 rstudent()
  #쿡의 거리 cooks.distance()
  #DFBeta dfbeta()
  #DFFit dffits()
  #모자값 hatvalues()
  #공분산비 covratio()

#잔차
album2 |> print(n = Inf)
album2 |> resid() -> album2$residuals2
albumsales.2 |> resid()
(resid(albumsales.2) -> album2$residuals)


#
resid(albumsales.2) -> album2$residuals
rstandard(albumsales.2) -> album2$standardized.residuals
rstudent(albumsales.2) -> album2$studentized.residuals
cooks.distance(albumsales.2) -> album2$cooks.distance
dfbeta(albumsales.2) -> album2$dfbeta
dffits(albumsales.2) -> album2$dffits
hatvalues(albumsales.2) -> album2$leverage
covratio(albumsales.2) -> album2$covariance.ratios

album2 |> view()
album2 |> head() |> print(width = Inf)

write.table(album2, "Album Sales With Diagnostics.dat",
            sep = "\t", row.names = F)
