#23-1030 mon 08:37

#
library(tidyverse)

#불러오기
read_delim("./dataset/07/Album Sales 2.dat") -> album2

album2

#회귀분석
lm(formula = sales ~ adverts + airplay + attract, 
   data = album2) -> albumSales.3

summary(albumSales.3)

#geom_point
  #adverts
album2 |> 
  ggplot(aes(x = adverts, y = sales)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F)

  #airplay
album2 |> 
  ggplot(aes(x = airplay, y = sales)) +
  geom_point()  +
  geom_smooth(method = "lm", se = F)

  #attract
album2 |> 
  ggplot(aes(x = attract, y = sales)) +
  geom_point(position = position_jitter(.3, .3)) +
  geom_smooth(method = "lm", se = F)


#응용 x축 없이 전체 산점도
album2 |> 
  pivot_longer(!sales, 
               names_to = "type", 
               values_to = "value") |> 
  ggplot(aes(x = value, y = sales)) +
  geom_point(position = position_jitter(.3, .3)) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(.~type, ncol = 2, scales = "free")
