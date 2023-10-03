#23-1002 mon 

#
library(tidyverse)
read_delim("./dataset/06/Exam Anxiety.dat") -> examdata

#
examdata |> glimpse()

#pearson
  #cor.test() 
  #p-value, 신뢰구간까지 구해준다
cor.test(examdata$Revise, examdata$Anxiety, 
         method = "pearson")

  #cor.test() 단점: 3개 이상은 구할 수 없다
cor.test(examdata$Revise, examdata$Anxiety, examdata$Exam,
         method = "pearson")


#regression
lm(examdata$Revise ~ examdata$Anxiety)

#
examdata |> ggplot(aes(Revise, Anxiety)) +
  geom_smooth()

ggsave(path = "./chapter/05/", "geom_smooth.png")

#
examdata
cor.test(examdata[,c("Revise", "Exam", "Anxiety")]) #error
cor(examdata[,c("Revise", "Exam", "Anxiety")])
