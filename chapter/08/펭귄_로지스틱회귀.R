#23-1008 #sun 


#
library(tidyverse)

#
read_delim("./dataset/07/Album Sales 1.dat") -> album
album

#
lm(album$sales ~ album$adverts)
cor.test(album$sales, album$adverts, method = "pearson")

album |> 
  ggplot(aes(x = adverts, y = sales)) +
  geom_point() +
  geom_smooth(se = F, method = "lm", size = 2, 
              color = "black")

lm(sales ~ adverts, album) |> summary()

library(palmerpenguins)
penguins
pairs(penguins)
ggplot(penguins, 
       aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point()

#
penguins$sex |> factor()
penguins$island |> factor()
glm(formula = sex ~ bill_length_mm + 
      bill_depth_mm + flipper_length_mm +
      body_mass_g, family = binomial(), 
    data = penguins) -> temp2

summary(temp2)
temp2$coefficients |> exp()

