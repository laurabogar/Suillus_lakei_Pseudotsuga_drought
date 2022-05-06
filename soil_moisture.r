# Checking % soil moisture data with time.
# Did soils get drier during drydown?

library(tidyverse)
library(cowplot)
library(lubridate)

WATER_SLOPE = -0.0036985 # calculated with linear model below

soil_moisture = read_csv("data/Suillus Soil Data.xlsx - Sheet1.csv", skip = 2)
treatment_inventory = read_csv("data/Suillus Treatment Inventory.xlsx - Sheet1.csv")
harvest_data = read_excel("data/Suillus Harvest Data_cleaned.xlsx")

water_potentials = read_csv("data/Suillus water potential data - Sheet1.csv")
water_potentials$Water_potential_MPa = as.numeric(water_potentials$Water_potential_MPa)

soil = rename(soil_moisture, Plant_ID = ID)
water_potentials$Date = mdy(water_potentials$Date)
water_potentials$Plant_ID[water_potentials$Plant_ID == "RS116"] = "RS166" #I mistranscribed this on my water potential data sheet.
harvest_data$harvesttime = word(harvest_data$Time, 2)
harvest_data$harvest_datetime = paste(word(harvest_data$Date, 1), harvest_data$harvesttime)
harvest_data$harvest_datetime = parse_date_time2(harvest_data$harvest_datetime,
                                            orders = "%Y-%m-%d %H:%M:%S")

together = left_join(harvest_data, soil)
together = left_join(together, water_potentials)
together = left_join(together, treatment_inventory)

# NOTE some of these values are clearly wrong -- clean up visually soon.

together = together %>% mutate(soil_moisture_loss = (soil_wet_g - soil_dry_g))
sum(together$soil_moisture_loss < 0, na.rm = TRUE) # 10 plants have negative moisture levels, doesn't make sense. Remove for now.


together = together %>% mutate(soil_moisture_loss_corrected = 
                                 case_when(soil_moisture_loss < 0 ~ 0, # if negative loss, we can assume it was pretty dry, just set to zero for now
                                           soil_moisture_loss > 0 ~ soil_moisture_loss)) # otherwise assume gravimetric is a reasonable estimate.
together = together %>% mutate(soil_moisture_pct = (soil_moisture_loss_corrected/soil_wet_g)*100)

together$days_since_watering = as.numeric(word(together$Batch, 3))

write_csv(together, "data/harvest_data_with_moisture_and_treatments.csv")

investigate = together[is.na(together$days_since_watering),]
# hmm, we are missing important data on a handful of plants
# and all the dead seedlings. (Don't know fertilizer or water level.)

# REMOVE THIS FILTER WHEN READY, THESE NAS SHOULD GO AWAY WHEN DATA ARE COMPLETE
# together = subset(together, is.na(together$Date) == FALSE)

ggplot(data = subset(together, soil_moisture_pct >= 0)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(Date), y = soil_moisture_pct)) +
  geom_jitter(width = 0.20,
              aes(x = as.factor(Date), y = soil_moisture_pct)) +
  ylab("Percent soil moisture (gravimetric)") +
  xlab("Harvest date")

# Cool! What if I try to look by treatment?
soil_moisture = ggplot(data = subset(together, 
                     is.na(Water) != TRUE & is.na(days_since_watering) != TRUE & Water != "intermed"),
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(days_since_watering), 
                   y = soil_moisture_pct)) +
  geom_point(position = position_jitterdodge(dodge.width = .5,
                                             jitter.width = 0.2),
              aes(x = as.factor(days_since_watering), 
                  y = soil_moisture_pct,
                  shape = Spore)) +
  ylab("Percent soil moisture (gravimetric)") +
  xlab("Days since last watering") +
  scale_shape_discrete(name = "Fungus added?", labels = c("No", "Yes"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

save_plot("plots/soil_moisture_boxplot.pdf", soil_moisture)

#### Question: Did shoot water potential decline with time since harvest? ####

datetimes = paste(together$Date, together$Time_water_potential)
together$water_potential_formatted_time = parse_date_time2(datetimes,
                        orders = "%Y-%m-%d %H:%M")

water_potential_time_in_cooler = together$water_potential_formatted_time - together$harvest_datetime
together$water_potential_lag = water_potential_time_in_cooler

watermodel = lm(Water_potential_MPa ~ water_potential_lag, data = together)
summary(watermodel)

together$lagnumeric = as.numeric(together$water_potential_lag)

water_potential_regression = ggplot(data = together) +
  theme_cowplot() +
  geom_point(aes(x = lagnumeric, 
                 y = Water_potential_MPa,
                 color = Water)) +
  geom_smooth(aes(x = lagnumeric, 
                y = Water_potential_MPa),
            method = "lm",
            formula = y ~ x,
            color = "black",
            size = 0.5) +
  scale_color_manual(values = c("goldenrod2", "black", "royalblue2")) +
  scale_y_continuous(limits = c(-8, 0)) +
  # annotate("text", paste(expression(beta), " = -0.0036985"), x = 300) +
  ylab("Shoot water potential (MPa)\n(synthetic pre-dawn)") +
  xlab("Minutes since harvest")

water_potential_regression_annotated = ggdraw(water_potential_regression) +
  draw_label("y = -0.0037x - 1.11", x = .75, y = .25) +
  draw_label("Adj. R^2 = 0.079", x = .75, y = .2)

save_plot("plots/water_potential_regression_annotated.pdf", water_potential_regression_annotated)

# Found a weird negative time value while making the plot above.
# Holly says she harvested RS 116 around 2:30PM; my notes
# say I took its water potential around 12:40. This is not possible.
# I think I likely mistranscribed the water potential seedling.
# RS166 should also have been in this batch, but doesn't show up on my
# water potential sheet, and Holly harvested it in the morning.
# Changing manually above.

# Let's correct the water potentials with this factor:

together$water_potential_corrected = together$Water_potential_MPa - WATER_SLOPE*together$lagnumeric


# Preliminary water potential plot
ggplot(data = subset(together, soil_moisture_pct >= 0)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(Date), y = Water_potential_MPa)) +
  geom_jitter(width = 0.20,
              aes(x = as.factor(Date), y = Water_potential_MPa)) +
  ylab("Water potential (MPa)") +
  xlab("Harvest date")

# Water potential -- nicer plot
water_potential_boxplot = ggplot(data = subset(together, 
                     is.na(Water) != TRUE & is.na(days_since_watering) != TRUE & Water != "intermed"),
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = as.factor(days_since_watering), y = water_potential_corrected)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8,
                                             jitter.width = 0.2),
             aes(x = as.factor(days_since_watering), 
                   y = water_potential_corrected,
                 shape = Spore)) +
  ylab("Shoot water potential (MPa)\n(synthetic pre-dawn, corrected)") +
  xlab("Days since last watering") +
  scale_shape_discrete(name = "Fungus added?", labels = c("No", "Yes"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

save_plot("plots/water_potential_boxplot.pdf", water_potential_boxplot)

### How are colonization and soil moisture related? ####

together$percent_col = (as.numeric(together$Colonized_tips)/as.numeric(together$Total_root_tips))*100
together$percent_col[is.na(together$percent_col)] = 0

# You know, I am actually not sure how I would make this work
# would need to control for harvest day
# Maybe interesting but hard to do.