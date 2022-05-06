# Preliminary % colonization plot

library(tidyverse)
library(readxl)
library(cowplot)

harvest_data = read_excel("data/Suillus Harvest Data_cleaned.xlsx")
treatments = read_csv("data/Suillus Treatment Inventory.xlsx - Sheet1.csv")

together = left_join(harvest_data, treatments)
together$Colonized_tips = as.numeric(together$Colonized_tips)
together$Total_root_tips = as.numeric(together$Total_root_tips)
together = mutate(together,
                  percent_colonization = (Colonized_tips/Total_root_tips)*100)

ggplot(data = subset(together, Spore == "spore")) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Water, y = percent_colonization,
                   color = N_level)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.50),
              aes(x = Water, y = percent_colonization,
                  color = N_level)) +
  ylab("Percent colonization") +
  xlab("Water level")
