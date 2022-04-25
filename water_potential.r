# WATER POTENTIAL

library(tidyverse)
library(cowplot)

water_potentials = read_csv("data/Suillus water potential data - Sheet1.csv")
water_potentials$Water_potential_MPa = as.numeric(water_potentials$Water_potential_MPa)

treatments = read_csv("data/Suillus Treatment Inventory.xlsx - Sheet1.csv")

soil_moisture = read_csv("data/Suillus Soil Data.xlsx - Sheet1.csv", skip = 2)
soil = rename(soil_moisture, Plant_ID = ID)


water_treatments = left_join(treatments, water_potentials)
water_treatments = left_join(water_treatments, soil)
water_treatments$realdate = mdy(water_treatments$Date)

ggplot(data = water_treatments) +
  theme_cowplot() +
  # geom_boxplot(outlier.alpha = 0,
               # aes(x = as.factor(realdate), y = Water_potential_MPa)) +
  geom_jitter(width = 0.20,
              aes(x = realdate, y = Water_potential_MPa)) +
  ylab("Water potential (MPa)") +
  xlab("Harvest date")
