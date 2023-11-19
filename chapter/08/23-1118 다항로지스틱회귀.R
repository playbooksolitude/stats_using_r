#23-1118 sat 08:40

#
library(tidyverse)

read_delim("./dataset/08/Chat-Up Lines.dat") -> chatdata
chatdata

library(car)
vif(chatdata)

glm(formula = Success ~ Funny + Sex + Good_Mate + Gender, 
    family = "binomial", data = chatdata)

#factor 변환
chatdata$Success |> 
  factor() |> 
  relevel(ref = "No response/Walk Off") -> chatdata$Success

chatdata$Gender |> 
  factor() |> 
  relevel(ref = "Male") -> chatdata$Gender

# 확인
contrasts(chatdata$Gender)

#모델링
glm(formula = Success ~ Funny + Sex + Good_Mate + Gender, 
    family = "binomial", data = chatdata) -> temp1

summary(temp1)

#다중공선성
vif(temp1)   #다중공선성 없음
1/vif(temp1)

#승산비
temp1$coefficients |> exp()

#승산비 신뢰구간
temp1 |> confint() |> exp()
