#23-1115 wedn 09:30

#
library(tidyverse)
read_delim("./dataset/08/eel.dat") -> eeldata
eeldata

#
eeldata$Cured |> 
  factor() |> 
  relevel(ref = "Not Cured") -> eeldata$Cured

  #기저 범주 확인
contrasts(eeldata$Cured)

#
eeldata$Intervention |> 
  factor() |> 
  relevel(ref = "No Treatment") -> eeldata$Intervention

  #기저 범주 확인
eeldata$Intervention |> contrasts()

#
glm(formula = Cured ~ Intervention, family = "binomial", 
    data = eeldata) -> eelmodel.1


glm(formula = Cured ~ Intervention + Duration, 
    family = "binomial", 
    data = eeldata) -> eelmodel.2

#?glm()
summary(eelmodel.1)
summary(eelmodel.2)

# ----------------------------------------------------------
#진단 통계량
fitted(eelmodel.1) -> eeldata$predicted.probabilities
rstandard(eelmodel.1) -> eeldata$standardized.residuals
rstudent(eelmodel.1) -> eeldata$studentized.residuals
dfbeta(eelmodel.1) -> eeldata$dfbeta
dffits(eelmodel.1) -> eeldata$dffit
hatvalues(eelmodel.1) -> eeldata$leverage

#eeldata |> view()

#예측된 확률 살펴보기
eeldata[, c("Cured", 
            "Duration", 
            "Intervention", 
            "predicted.probabilities")] |> head()

#
#불러오기
read_delim("./dataset/07/Album Sales 1.dat") -> album
read_delim("./dataset/07/Album Sales 2.dat") -> album2

album
album2

#회귀분석
(lm(formula = sales ~ adverts, data = album) -> albumSales.2)
summary(albumSales.2)
fitted(albumSales.2) -> album$fitted.probabilities # 기울기값
resid(albumSales.2) -> album$residuals  #잔차
rstandard(albumSales.2) -> album$standardized.residuals #잔차를 Z점수로 변환
rstudent(albumSales.2) -> album$studentized.residuals #수정 예측값과 원래의 관측값의 차이를 제외 잔차 혹은 삭제 잔차라고 부르고, 그것을 표준 오차로 나눈 값을 스튜던트화 잔차

cooks.distance(albumSales.2) -> album$cooks.distance #한 사례가 모형 전체에 미치는 영향

dfbeta(albumSales.2) -> album$dfbeta #모든 사례를 포함해 추정한 매개변수와 특정 사례를 제외하고 추정한 매개변수의 차이
dffits(albumSales.2) -> album$dffits #수정 예측값과 예측값의 차이
hatvalues(albumSales.2) -> album$leverage
covratio(albumSales.2) -> album$covariance.ratios

#수정 예측값과 관측값의 차이

album |> head()

#
album |> 
  mutate(num = row_number(), .before = 1) -> album

#
album |> 
  ggplot(aes(x = adverts, y = sales)) +
  #geom_point() +
  geom_text(aes(label = num)) +
  geom_smooth(se = F)

album |> filter(num %in% c("55", "188", "191")) #|> view()

album |> filter(dffits < 1)

#mpg dataset의 도시 연비, 고속도로 평균 연비
mpg |> 
  group_by(manufacturer, model) |> 
  reframe(mean_cty = mean(cty), 
          mean_hwy = mean(hwy)) |> 
  mutate(num = row_number(), .before = 1) -> temp1

temp1

#도시 연비를 예측변수로 고속도로 연비를 예측하는 회귀모델
lm(temp1$mean_hwy ~ temp1$mean_cty) -> temp2.model

#통계량
summary(temp2.model)

#통계량을 보기 좋게 dataset에 추가하기
fitted(temp2.model) -> temp1$predicted   #기울기의 fit point
resid(temp2.model) -> temp1$resid        #잔차 (관측값과 기울기의 차이)
rstandard(temp2.model) -> temp1$standard #표준화잔차
rstudent(temp2.model) -> temp1$student   #스튜던트 잔차

#그래프 그리기
temp1 |> 
  ggplot(aes(x = mean_cty, y = mean_hwy)) +
  #geom_smooth(se = F) +
  geom_smooth(se = F, method = "lm") +
  geom_text(aes(label = num))

#결과 확인
temp1 |> filter(num %in% c("5", "29", "32", "37"))
18.8 - (21.2)
                