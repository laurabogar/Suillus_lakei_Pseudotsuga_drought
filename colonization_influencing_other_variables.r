# Does the relationship between soil moisture and water potential
# change with fungal colonization?

library(tidyverse)
library(readxl)
library(cowplot)
library(lme4)
library(lmerTest)

colonization = read_csv("data/percent_colonization_only.csv")
biomass = read_csv("data/processed_biomass_data.csv")
harvest = read_csv("data/harvest_data_with_moisture_and_treatments.csv")

merged = left_join(harvest, biomass) %>%
  left_join(colonization)

merged = select(merged, everything(), -Time, -harvesttime)

toplot = 
  filter(merged, Spore == "spore") %>%
  filter(abs(water_potential_corrected) < 3) %>%
  filter(water_potential_corrected < 0) %>%
  filter((is.na(percent_colonization) != TRUE))

water_potential_by_soil_moisture = ggplot(data = toplot) +
  theme_cowplot() +
  geom_point(aes(x = soil_moisture_pct, 
                 y = water_potential_corrected,
                 # alpha = percent_colonization,
                 color = percent_colonization)) +
  xlab("Gravimetric soil moisture (%)") +
  ylab("Shoot water potential (MPa), corrected")

save_plot("plots/water_potential_by_soil_moisture.pdf", water_potential_by_soil_moisture)

# I would like to be able to somehow quantify whether colonization
# made a difference in how shoot water potential relates to gravimetric soil
# moisture. I think I can do this with a model, but could I plot it?

# Model:

moisture_colonization_model = lm(water_potential_corrected ~ soil_moisture_pct * percent_colonization, data = toplot)
summary(moisture_colonization_model)

moisture_colonization_lme = lmer(water_potential_corrected ~ soil_moisture_pct * percent_colonization * N_level + (1|days_since_watering), data = toplot)
summary(moisture_colonization_lme)

# Graphical ideas:
toplot$moistureaxis = toplot$water_potential_corrected/toplot$soil_moisture_pct

ggplot(data = toplot) +
  theme_cowplot() +
  geom_point(aes(x = percent_colonization, 
                 y = moistureaxis,
                 color = Water)) +
  ylab("Shoot water potential (MPa)/Gravimetric soil moisture (%)") +
  xlab("Percent_colonization")

ggplot(data = toplot) +
  theme_cowplot() +
  geom_point(aes(x = percent_colonization, 
                 y = water_potential_corrected,
                 color = Water)) +
  ylab("Shoot water potential (MPa)/Gravimetric soil moisture (%)") +
  xlab("Percent_colonization")
