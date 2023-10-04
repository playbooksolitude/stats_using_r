#23-1004 wedn 23:10 #편상관

#
library(tidyverse)
install.packages("ggm")
library(ggm)

#
read_delim("./dataset/06/Exam Anxiety.dat") -> examdata
read_delim("./dataset/06/The Biggest Liar.dat", 
           col_names = T) -> liardata


#
(examdata[,c("Revise", "Exam", "Anxiety")] -> examdata2)
examdata2 |> select(Exam, Anxiety, Revise) -> examdata2
cor(examdata2)
cor(examdata2)^2

#
examdata2
ggm::pcor()

  #까다롭다 var()은 문법에 없는데..
(pcor(c("Exam", "Anxiety", "Revise"), var(examdata2)) -> pc)
pc^2

#유의성
examdata2 |> dim()
pcor.test(pc, q = 1, n = 103)

  #참고
cor.test(examdata2$Exam, examdata2$Anxiety)
(cor(examdata2$Exam, examdata2$Anxiety) -> examcor)
examcor^2


cor(examdata2)
