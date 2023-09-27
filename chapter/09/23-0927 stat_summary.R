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
geom_errorbar()
