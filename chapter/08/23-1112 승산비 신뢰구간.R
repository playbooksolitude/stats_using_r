#23-1113 월 12:19

#
library(tidyverse)

#
#파일 불러오기
(read_delim("./dataset/08/eel.dat") -> eeldata)

# 팩터 전환 Cured
eeldata$Cured |> factor() -> eeldata$Cured
eeldata$Cured |> contrasts()

#relevel 기저 범주 == Not Cured
eeldata$Cured |> relevel(ref = "Not Cured") -> eeldata$Cured
eeldata$Cured |> contrasts()


# 팩터 전환 Intervention
eeldata$Intervention |> factor() -> eeldata$Intervention
eeldata$Intervention |> contrasts()

eeldata$Intervention |> 
  relevel(ref = "No Treatment") -> eeldata$Intervention
eeldata$Intervention |> contrasts()


# ----------------------------------------------------------
#model 1 #위계적 방법
glm(formula = Cured ~ Intervention, 
    family = "binomial", 
    data = eeldata) -> eelmodel.1

  #model 2 #위계적 방법
glm(formula = Cured ~ Intervention + Duration, 
    family = "binomial", 
    data = eeldata) -> eelmodel.2

summary(eelmodel.1)
- summary(eelmodel.2)

# glm(formula = Cured ~ Intervention, 
#     data = eeldata)
# glm(formula = Cured ~ Intervention + Duration, data = eeldata)

  #1.233532 / 0.414565

#modelChi 카이제곱 통계량 (귀무 이탈도 - 이탈도)
eelmodel.1$null.deviance - eelmodel.1$deviance -> modelChi
modelChi  #9.926201

# 자유도
(eelmodel.1$df.null - eelmodel.1$df.residual -> chidf)

#p-value 계산
1 - pchisq(modelChi, chidf) #0.002


#Root-Square 에 해당하는 콕스-스넬 측도
1 - exp((eelmodel.1$deviance - eelmodel.1$null.deviance)/113) -> R.cs
R.cs

#네이글커크측도
R.cs / (1 - exp(-(eelmodel.1$null.deviance/113)))

#승산비
eelmodel.1$coefficients
exp(eelmodel.1$coefficients)

#승산비의 신뢰구간
exp(confint(eelmodel.1))
confint(eelmodel.1) |> exp()

# 모델 적합
eelmodel.2$deviance
eelmodel.2$df.residual

#모델 2의 카이제곱 분포 (이탈도)
  #모형 1 이탈도 - 모형 2 이탈도
  #이탈도 변화
(eelmodel.1$deviance - eelmodel.2$deviance -> modelChi) 

  #자유도 차이
(eelmodel.1$df.residual - eelmodel.2$df.residual -> chidf)

  #p값
(1 - pchisq(modelChi, chidf) -> chisq.prob)  #p-value
  #결과
modelChi; chidf; chisq.prob


#아노바
anova(eelmodel.1, eelmodel.2)


#승산비
eelmodel.1$coefficients |> exp() #3.416
eelmodel.2$coefficients |> exp() #3.433
confint(eelmodel.2) |> exp()
eelmodel.2$coefficients |> exp()

#잔차
resid(eelmodel.2)
eelmodel.2 |> plot()


#그래프
eeldata |> 
  mutate(num = row_number(), .before = 1, 
         Y = ifelse(Cured == "Cured", 1, 0)) |> 
  ggplot(aes(x = Intervention, y = Y)) +
  geom_point(position = position_jitter(.2, .2)) +
  scale_y_continuous(breaks = c(0,1)) +
  geom_smooth(se = F) +
  facet_wrap(.~Duration)

eeldata |> filter(Duration == 10)
