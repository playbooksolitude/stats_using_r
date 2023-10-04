#23-1003 thus 23:08

#
library(tidyverse)
library(car)

#
read_delim("./dataset/06/Exam Anxiety.dat") -> examdata

#
examdata
  #Revise  복습   #연속형-구간
  #Exam    성적   #범주형-순서
  #Anxiety 불안   #연속형-구간

#상관계수
# cor.test()에 켄달 상관관계를 적용하면? 


#Exam ~ Anxiety
examdata$Exam |> hist()
examdata$Anxiety |> hist()

  #
examdata$Exam |>qqPlot()
examdata$Anxiety |> qqPlot()
  #
shapiro.test(examdata$Exam)    #정규성 미준수
shapiro.test(examdata$Anxiety) #정규성 미준수

  #Exam ~ Anxiety 는 비모수 상관계수를 구해야한다?
cor.test(examdata$Exam, examdata$Anxiety, method = "pearson")
cor.test(examdata$Exam, examdata$Anxiety, method = "spearman")
cor.test(examdata$Exam, examdata$Anxiety, method = "kendall")

lm(examdata$Exam ~ examdata$Anxiety)  # -0.665


##Revise ~ Anxiety #비모수검정


examdata$Revise
examdata$Revise |> hist()
shapiro.test(examdata$Revise)
examdata$Revise |> qqPlot()

cor.test(examdata$Revise, examdata$Anxiety, method = "pearson")
cor.test(examdata$Revise, examdata$Anxiety, method = "spearman")
cor.test(examdata$Revise, examdata$Anxiety, method = "kendall")

examdata$Revise |> table()
examdata$Anxiety |> table()


# -------------------------------------------
read_delim("./dataset/06/The Biggest Liar.dat", 
           col_names = T) -> liardata

liardata |> names()

shapiro.test(liardata$Creativity) #정규성 o #연속
liardata$Creativity |> hist()
liardata$Creativity |> qqPlot()

shapiro.test(liardata$Position)   #정규성 x #순서
liardata$Position |> hist()
liardata$Position |> qqPlot()

cor.test(liardata$Creativity, liardata$Position, 
         method = "spearman")

#순서를 바꿀 경우 값은 같다

liardata$Position |> qqPlot()
liardata$Creativity |> qqPlot()
shapiro.test(liardata$Creativity)


#스피어만 --------------------------------------------------
  #양측검정
cor.test(liardata$Position, liardata$Creativity,
         method = "spearman")   #p-value 0.00172

  #단측검정
cor.test(liardata$Position, liardata$Creativity,
         method = "spearman", 
         alternative = "less") #p-value 0.00086

  
#켄달
  #양측검정
cor.test(liardata$Position, liardata$Creativity,
         method = "kendal")

  #단측검정
cor.test(liardata$Position, liardata$Creativity,
         method = "kendal", alternative = "less")

#피어슨
cor.test(liardata$Position, liardata$Creativity, 
         method = "pearson")

#피어슨
cor.test(liardata$Position, liardata$Creativity, 
         method = "pearson", 
         alternative = "greater")


# 둘 중 한개만 정규성을 만족할 경우 어떤 상관을 해야 하는가?
ggplot(data = liardata, aes(x = Position, y = Creativity)) +
  geom_jitter(width = .1) +
  geom_smooth(method = "lm")

lm(liardata$Position ~ liardata$Creativity)
lm(liardata$Creativity ~ liardata$Position)


#답은? 점이연상관과 이연 상관계수
read_csv("./dataset/06/pbcorr.csv") -> catdata

catdata 
  #time 고양이가 집밖에서 보낸 시간 hour
  #gender 수컷 1, 암컷 0
  #recode 성별을 반대로 부호화 수컷은 0, 암컷은 1

shapiro.test(catdata$time) #정규성 x
catdata$time |> qqPlot()

#교재 time, gender 피어슨
  #t 검정과 동일 p-value 0.0028
cor.test(catdata$time, catdata$gender) #cor 0.378
cor.test(catdata$time, catdata$recode) #cor -0.378 

catdata$gender |> table() |> prop.table()



















