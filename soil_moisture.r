# Checking % soil moisture data with time.
# Did soils get drier during drydown?

library(tidyverse)
library(cowplot)
library(lubridate)

soil_moisture = read_csv("data/Suillus Soil Data.xlsx - Sheet1.csv", skip = 2)

harvest_data = read_csv("data/Suillus Harvest Data.xlsx - Sheet1.csv")

water_potentials = read_csv("data/Suillus water potential data - Sheet1.csv")
water_potentials$Water_potential_MPa = as.numeric(water_potentials$Water_potential_MPa)

soil = rename(soil_moisture, Plant_ID = ID)

together = left_join(harvest_data, soil)
together = left_join(together, water_potentials)


head(together)
summary(as.factor(together$Date))

# NOTE some of these values are clearly wrong -- clean up visually soon.

together = together %>% mutate(soil_moisture_loss = (soil_wet_g - soil_dry_g))
sum(together$soil_moisture_loss < 0, na.rm = TRUE) # 10 plants have negative moisture levels, doesn't make sense. Remove for now.


together = together %>% mutate(soil_moisture_loss_corrected = 
                                 case_when(soil_moisture_loss < 0 ~ 0, # if negative loss, we can assume it was pretty dry, just set to zero for now
                                           soil_moisture_loss > 0 ~ soil_moisture_loss)) # otherwise assume gravimetric is a reasonable estimate.
together = together %>% mutate(soil_moisture_pct = (soil_moisture_loss/soil_wet_g)*100)

# REMOVE THIS FILTER WHEN READY, THESE NAS SHOULD GO AWAY WHEN DATA ARE COMPLETE
together = subset(together, is.na(together$Date) == FALSE)

ggplot(data = subset(together, soil_moisture_pct >= 0)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(Date), y = soil_moisture_pct)) +
  geom_jitter(width = 0.20,
              aes(x = as.factor(Date), y = soil_moisture_pct)) +
  ylab("Percent soil moisture (gravimetric)") +
  xlab("Harvest date")

# Doesn't work yet, water potential data too dirty.
ggplot(data = subset(together, soil_moisture_pct >= 0)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(Date), y = Water_potential_MPa)) +
  geom_jitter(width = 0.20,
              aes(x = as.factor(Date), y = Water_potential_MPa)) +
  ylab("Water potential (MPa)") +
  xlab("Harvest date")

# Example code to steal from
massplot = ggplot(data = bio_and_col_onlyclean) +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Fungi, y = total_biomass)) +
  geom_jitter(width = 0.20,
              aes(x = Fungi, y = total_biomass)) +
  facet_grid(. ~ N_level, labeller = labeller(N_level = labels)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Total plant biomass (g)") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_text(data = anothertry, aes(x, y, label = labs)) +
  xlab("Fungi on roots at harvest")

