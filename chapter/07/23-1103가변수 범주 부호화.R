#23-1103 fri 14:25

#
library(tidyverse)

#
read_delim("./dataset/07/GlastonburyFestivalRegression.dat") ->
  gfr

gfr

#
gfr$music |> n_distinct()
table(gfr$music) |> data.frame()

# 가변수 부호화
contr.treatment(n = 4, base = 4)
#   1 2 3
# 1 1 0 0
# 2 0 1 0
# 3 0 0 1
# 4 0 0 0

# 가변수 범주 부호화
gfr$music |> factor() -> gfr$music   #교재에 누락됨
contr.treatment(n = 4, base = 4) -> contrasts(gfr$music)
gfr$music
gfr |> tail()
#

#부트스트랩
bootReg <- function(formula, data, indices)
{
  d <- data [indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#
library(boot)
boot(statistic = bootReg, formula = sales ~ adverts +
       airplay + attract, data = album2, R = 2000) -> bootResults

boot(statistic = bootReg, formula = sales ~ adverts +
       airplay + attract, data = album2, R = 2000) |> summary()

boot.ci(bootResults)
boot.ci(bootResults, type = "bca", index = 1)
boot.ci(bootResults, type = "bca", index = 2)
boot.ci(bootResults, type = "bca", index = 3)
boot.ci(bootResults, type = "bca", index = 4)
