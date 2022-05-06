# Biomass

library(tidyverse)
library(readxl)
library(cowplot)

WET_TO_DRY_CONVERSION = 0.172933

shoot_mass = read_csv("data/Suillus shoot data - Sheet1.csv")
root_mass = read_csv("data/Suillus root data - Sheet1.csv")
harvest_data = read_csv("data/harvest_data_with_moisture_and_treatments.csv")

shoot_mass = rename(shoot_mass, Plant_ID = ID)
root_mass = rename(root_mass, Plant_ID = ID)

everything = left_join(harvest_data, shoot_mass)
everything = left_join(everything, root_mass)

# Build a model to compute dry mass from wet root mass
# First, calculate total wet root mass

everything = mutate(everything, total_wet_roots = Wet_root_mass_env + Wet_root_mass_frozen)

for (i in 1:nrow(everything)) {
  if (is.na(everything$total_wet_roots[i]) == TRUE) {
    everything$total_wet_roots[i] = everything$Wet_root_mass_total[i]
  }
}

# Omit RS248, it was dead and is more than twice as big in dry weigh as any of the others.
# Might have been misweighed?
formodel = everything[everything$Plant_ID != "RS248",]

massmodel = lm(root_dry_wt ~ Wet_root_mass_env, data = formodel)
summary(massmodel)
# Not an awesome model -- R^2 is 0.5289. But, it is significant.
# Let's use this correction factor from the model slope: 0.172933

ggplot(data = formodel) +
  geom_point(aes(x = Wet_root_mass_env, y = root_dry_wt))

everything = mutate(everything, 
                    total_root_dry_cal = total_wet_roots * WET_TO_DRY_CONVERSION) %>%
  mutate(total_dry_biomass = total_root_dry_cal + shoot_dry_wt)

usefuldata = select(everything, Plant_ID, total_root_dry_cal, shoot_dry_wt, total_dry_biomass)
write_csv(usefuldata, "data/processed_biomass_data.csv")

ggplot(data = everything) +
  geom_boxplot(aes(x = N_level,
                 y = total_dry_biomass,
                 color = Water,
                 shape = Spore))

ggplot(data = subset(everything, Water != "intermed")) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = N_level, 
                   y = total_dry_biomass,
               color = Water)) +
  geom_point(position = position_jitterdodge(dodge.width = .05,
                                             jitter.width = 0.05),
             aes(x = N_level, 
                 y = total_dry_biomass,
                 shape = Spore,
                 color = Water)) +
  ylab("Total dry biomass (g)") +
  xlab("Nitrogen level") +
  scale_shape_discrete(name = "Fungus added?", labels = c("No", "Yes"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

