#23-0924 sun 21:50

#
library(tidyverse)
library(car)

#install.packages("Rcmdr", dependencies = T)
library(Rcmdr)
#Rcmdr::Commander() #커맨더 재실행

#
 # read.table("./dataset/chapter_15/Drug.dat", 
 #            header = T) -> drug

read_delim("./dataset/chapter_15/Drug.dat") -> drug

drug |> view()
drug |> glimpse()
drug |> str()
drug |> hist()
drug |> plot()
drug |> ggplot(aes(x = V1)) +
  geom_point(aes(y = V2), stat = "identity") +
  geom_point(aes(y = V3), stat = "identity")

# book 828p
drug |> 
  pivot_longer(cols = 2:3, 
               names_to = "BDI", 
               values_to = "Value") -> drug2

drug2 |> 
  ggplot(aes(x = drug, y = Value)) +
  geom_jitter(width = .1)

drug2 |> 
  ggplot(aes(x = BDI, y = Value)) +
  geom_jitter(width = .1)

#
drug$wedsBDI |> sort() |> sum()
drug[,c(1,3)] #wed
drug[,c(1,2)] #sun
drug
#
drug[,c(1,3)] |> arrange(wedsBDI)
c(1,2,3.5,3.5,5,6,7,8,9,14) |> sum()

#
drug |> summary()
shapiro.test(drug$sundayBDI)
shapiro.test(drug$wedsBDI)

#
drug |> 
  pivot_wider(names_from = drug, 
              values_from = drug)

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
  











