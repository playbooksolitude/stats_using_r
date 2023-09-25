#23-0924 sun 21:50

#
library(tidyverse)
library(car)
library(psych)
library(pastecs)
library(gt)
#Rcmdr 
#install.packages("Rcmdr", dependencies = T)
#library(Rcmdr)
#Rcmdr::Commander() #커맨더 재실행

#dat 파일 열기
# read.table("./dataset/chapter_15/Drug.dat", 
#            header = T) -> drug
read_delim("./dataset/chapter_15/Drug.dat") -> drug

#str()
drug |> view()
drug |> glimpse()
drug |> str()
drug |> hist()
drug |> plot()


# book 828p ------------------------------------------------
drug |> 
  pivot_longer(cols = 2:3, 
               names_to = "BDI", 
               values_to = "Value") -> drug2

# EDA
drug2 |> 
  ggplot(aes(x = drug, y = Value)) +
  geom_jitter(width = .1) +
  facet_wrap(.~BDI)


#합계 구하기
c(1,2,3.5,3.5,5,6,7,8,9,14) |> sum()


#
drug |> 
  pivot_wider(names_from = drug, 
              values_from = drug)

#stat.desc()
stat.desc(drug$sundayBDI, basic = T, norm = F)


# Ecstasy #weds
drug |> 
  filter(drug == "Ecstasy") |> 
  with(shapiro.test(wedsBDI))   #수요일 #정규분포 o
  
    #정규 분포 o
  drug |> 
    filter(drug == "Ecstasy") |> #수요일 #정규분포 o
    with(wedsBDI) |> 
    qqPlot()
  
      #
      drug |> 
        filter(drug == "Ecstasy") |> #수요일 #정규분포 o
        with(wedsBDI) |> 
        barplot()


# Ecstasy #sunday
drug |> 
  filter(drug == "Ecstasy") |> 
  with(shapiro.test(sundayBDI)) #일요일 #정규분포 x

  #정규 분포 x
drug |> 
  filter(drug == "Ecstasy") |> with(sundayBDI) |> 
  qqPlot()

      #
      drug |> 
        filter(drug == "Ecstasy") |> with(sundayBDI) |> 
        barplot()


# -------------------------------------------------------- #
# alcohol #일요일
drug |> 
  filter(drug == "Alcohol") |> 
  with(shapiro.test(sundayBDI)) #일요일 #정규분포 O

  #정규 분포 O
  drug |> 
    filter(drug == "Alcohol") |> 
    with(sundayBDI) |> 
    qqPlot() #|> 
    plot()

      #plot
      drug |> 
        filter(drug == "Alcohol") |> 
        with(sundayBDI) |> 
        barplot()
      

# alcohol #수요일
drug |> 
  filter(drug == "Alcohol") |> 
  with(shapiro.test(wedsBDI)) #일요일 #정규분포 x

  #정규 분포 x
  drug |> 
    filter(drug == "Alcohol") |> 
    with(wedsBDI) |> 
    qqPlot()

        drug |> 
          filter(drug == "Alcohol") |> 
          with(wedsBDI) |> 
          barplot()


drug |> 
  filter(drug == "Alcohol") |> 
  with(sundayBDI) |> 
  plot()
  

drug$sundayBDI |> describe()
drug$sundayBDI |> stat.desc()


# 레빈 검정 -----------------------------------------------
leveneTest(drug$sundayBDI, drug$wedsBDI)
leveneTest(drug$sundayBDI, drug$wedsBDI, center = mean)


#윌콕슨 순위 합
wilcox.test(x = drug$sundayBDI)
drugData |> wilcox.test(sundayBDI ~ drug, exact = F)

drug -> drugData
wilcox.test(sundayBDI ~ drug, data = drugData, exact = F)
wilcox.test(wedsBDI ~ drug, data = drugData, exact = F)



