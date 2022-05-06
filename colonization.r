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

# Making a prettier plot
# Excluding plants where colonization FAILED
# because I am not sure how informative those zeros are.
# Also excluding intermediate water level because
# it is not well replicated.

percent_colonization_plot = ggplot(data = subset(together, Spore == "spore" &
                       Water != "intermed")) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = N_level, 
                   y = percent_colonization,
                   # shape = N_level,
                   color = Water)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8,
                                             jitter.width = 0.3),
             aes(x = N_level, 
                 y = percent_colonization,
                 # shape = N_level,
                 color = Water)) +
  ylab("Percent fungal colonization") +
  xlab("Nitrogen level") + 
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

save_plot("plots/percent_colonization_boxplot.pdf", percent_colonization_plot)

perccol_model = lm(percent_colonization ~ Water*N_level, data = subset(together, Spore == "spore" &
                   Water != "intermed"))
summary(perccol_model)
anova(perccol_model)
