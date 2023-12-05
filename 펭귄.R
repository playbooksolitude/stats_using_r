#23-1005

#
library(palmerpenguins)
library(tidyverse)
library(colorspace)


(penguins |> 
  drop_na() |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(.~island) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") -> a)


(penguins |> 
  drop_na() |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(sex~island) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") -> b)

library(patchwork)
a | b
ggsave(path = "./chapter/06/", filename = "penguin.png")


library(nord)
nord_palettes
library(colorspace)
colorspace::hcl_palettes(plot = T)
