#23-0927 wedn 10:43

#
library(tidyverse)
library(Hmisc)


#
(read_delim("./dataset/09/SpiderLong.dat") -> spiderlong)
read_delim("./dataset/09/SpiderWide.dat") -> spiderwider

#
spiderlong;spiderwider

#독립설계 vs 반복측정 설계
spiderlong |> glimpse()
spiderlong |> str()
spiderlong |> table()
spiderwider |> table()

#stat_summary() 
#평균을 구하지 않고 tidydata set 에서 바로 평균을 구한다

ggplot(spiderlong, aes(Group, Anxiety)) -> bar
bar + stat_summary(fun.y = mean, 
                   geom = "bar", 
                   fill = "white", 
                   color = "black")

  #중앙값
bar + stat_summary(fun.y = median, 
                   geom = "bar", 
                   fill = "white", 
                   color = "black")
  #막대 + 신뢰구간
bar + stat_summary(fun.y = mean,
                   geom = "bar", 
                   fill = "white", 
                   color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", width = .2)

  #errorbar
bar + stat_summary(fun.y = mean,
                   geom = "errorbar", 
                   color = "black") +
  stat_summary(fun.data = mean_cl_boot, 
               geom = "pointrange")

  #사분위수
bar + stat_summary(fun.data = median_hilow, 
                   geom = "pointrange", 
                   color = "red") #


#
install.packages("nycflights13")
library(nycflights13)
flights |> count(origin, dest) -> fligts_1origin
fligts_1origin |> group_by(origin) |> 
  summarise(mean = mean(n), 
            sd = sd(n)) 
  #   origin  mean   sd
  # 1  EWR    1405  1470
  # 2  JFK    1590  1964
  # 3  LGA    1539  2163

#
fligts_1origin |> ggplot(aes(origin, n)) -> fligts_2
fligts_2 + 
  stat_summary(geom = "bar", fun.y = "mean", 
               fill = "white", color = "black") +
  stat_summary(geom = "errorbar", 
               fun.data = mean_cl_boot, width = .2) +
  scale_y_continuous(breaks = c(1200, 1400, 1600))
  
#
fligts_1origin |> summary()







