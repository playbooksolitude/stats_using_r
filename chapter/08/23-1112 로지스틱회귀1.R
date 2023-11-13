#23-1111 mon 14:59

#
library(tidyverse)
library(MASS)

#
mpg
mtcars |> head()
Cars93 |> head() |> view()
Cars93 |> sample_n(10) |> view()

#
Cars93 |> colnames()
Cars93 |> 
  count(Manufacturer, AirBags) |> 
  pivot_wider(names_from = AirBags, values_from = n) -> temp2

temp2 |> colnames()
temp2 |> str()
temp2 |> rename(
  "제조사" = 1,
  "동승자석" = 2,
  "없음" = 3,
  "운전자석" = 4) |> dplyr::select(1,2,4,3)

temp3 |> colnames()
temp3 |> select(동승자석, 없음)

# MASS 라이브러리에는 select가 있다
mpg
mpg |> dplyr::select(model, displ)


#
library(car)
install.packages("mlogit")
library(mlogit)

#파일 불러오기
read_delim("./dataset/08/eel.dat") -> eeldata
eeldata

#기저 범주 지정 #가변수 범주 부호화
eeldata$Cured
eeldata$Cured |> factor() -> eeldata$Cured
contrasts(eeldata$Cured)

relevel(eeldata$Cured, ref = "Not Cured") -> eeldata$Cured
contrasts(eeldata$Cured)
relevel(eeldata$Cured, ref = "Not Cured") -> eeldata$Cured

eeldata
eeldata$Intervention |> factor() -> eeldata$Intervention
contrasts(eeldata$Intervention)
relevel(eeldata$Intervention, 
        ref = "No Treatment") -> eeldata$Intervention
