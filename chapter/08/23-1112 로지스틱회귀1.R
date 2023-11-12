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

#
mpg
mpg |> dplyr::select(model, displ)
?select

#
library(car)
install.packages("mlogit")
library(mlogit)

#
read_delim("./dataset/08/eel.dat") -> eeldata
eeldata
