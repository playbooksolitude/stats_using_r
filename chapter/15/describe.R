#23-0925 #mon

#
library(psych)
library(pastecs)
library(MASS)


#
read_delim("./dataset/05/DownloadFestival(No Outlier).dat") -> dlf

#간략보기
psych::describe()
pastecs::stat.desc()

#
dlf |> colnames()
dlf |> describe()
dlf |> stat.desc()
dlf |> summary()


#
drug$test <- NA   #칸 만들기
drug$test <- NULL #칸 지우기
drug

# QQ
dlf |> 
  ggplot(aes(sample = dlf$day1)) + 
  stat_qq() +
  stat_qq_line() +
  geom_qq()

dlf$day1 |> qqPlot()
dlf$day2 |> qqPlot()
dlf$day3 |> qqPlot()
