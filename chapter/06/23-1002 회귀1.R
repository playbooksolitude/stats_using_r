#23-1002 mon 

#
library(tidyverse)
read_delim("./dataset/06/Exam Anxiety.dat") -> examdata

#
examdata |> glimpse()

#pearson
cor.test(examdata$Revise, examdata$Anxiety, 
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
