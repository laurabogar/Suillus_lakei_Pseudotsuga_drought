# Biomass

library(tidyverse)
library(readxl)
library(cowplot)

WET_TO_DRY_CONVERSION = 0.213008

shoot_mass = read_csv("data/Suillus shoot data - Sheet1.csv")
root_mass = read_csv("data/Suillus root data - Sheet1.csv")
harvest_data = read_csv("data/harvest_data_with_moisture_and_treatments.csv")

shoot_mass = rename(shoot_mass, Plant_ID = ID)
root_mass = rename(root_mass, Plant_ID = ID)

# IMPORTANT DATA CLEANING CAVEAT:
# We have several plants with multiple entries in shoot mass
# and root mass. For now, I'm just going to remove these
# and hope I get a reasonable idea for the data despite this added
# noise, but in the end I need to figure out why they are there.
# Root mass problems: RS252   RS396   RS??? 
# Shoot mass problems: RS116     RS178     RS311     RS463     RS???
# Harvest data problems:  RS463   RS178   RS413   RS396   RS019   RS026   RS078   RS094   RS103   RS121 
# 24      16      16       3       2       2       2       2       2       2 
# RS124   RS134   RS166   RS203   RS224   RS234   RS242   RS305   RS311   RS316 
# 2       2       2       2       2       2       2       2       2       2 
# RS376   RS382   RS390   RS397   RS404   RS457   RS470   RS503   RS585   RS599 
# 2       2       2       2       2       2       2       2       2       2 

shoot_mass = shoot_mass %>% distinct(Plant_ID, .keep_all = TRUE)
root_mass = root_mass %>% distinct(Plant_ID, .keep_all = TRUE)

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
# Pretty decent -- R^2 is 0.7222. And, it is significant.
# Let's use this correction factor from the model slope: 0.213008

#### Eventually, be sure to calculate conversion factor individually ####
# for each plant, where possible.

ggplot(data = formodel) +
  geom_point(aes(x = Wet_root_mass_env, y = root_dry_wt))

everything = mutate(formodel, 
                    total_root_dry_cal = total_wet_roots * WET_TO_DRY_CONVERSION) %>%
  mutate(total_dry_biomass = total_root_dry_cal + shoot_dry_wt)

usefuldata = select(everything, Plant_ID, total_root_dry_cal, shoot_dry_wt, total_dry_biomass)
write_csv(usefuldata, "data/processed_biomass_data.csv")

ggplot(data = everything) +
  geom_boxplot(aes(x = N_level,
                 y = total_dry_biomass,
                 color = Water,
                 shape = Spore))

toplot = subset(everything, Water != "intermed" & is.na(Spore) == FALSE)

ggplot(data = toplot,
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = N_level, 
                   y = total_dry_biomass)) +
  geom_point(position = position_jitterdodge(),
             aes(x = as.factor(N_level), 
                 y = total_dry_biomass,
                 shape = Spore)) +
  ylab("Total dry biomass (g)") +
  xlab("Nitrogen level") +
  scale_shape_discrete(name = "Fungus added?", labels = c("No", "Yes"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

ggplot(data = toplot,
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Spore, 
                   y = total_dry_biomass)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             aes(x = Spore, 
                 y = total_dry_biomass,
                 shape = N_level,
                 group = Water)) +
  ylab("Total dry biomass (g)") +
  xlab("Spores added?") +
  scale_shape_discrete(name = "Nitrogen level", labels = c("high", "low"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

ggplot(data = toplot,
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Spore, 
                   y = total_root_dry_cal)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             aes(x = Spore, 
                 y = total_root_dry_cal,
                 shape = N_level,
                 group = Water)) +
  ylab("Total root dry biomass (g)") +
  xlab("Spores added?") +
  scale_shape_discrete(name = "Nitrogen level", labels = c("high", "low"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

ggplot(data = toplot,
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Spore, 
                   y = shoot_dry_wt)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             aes(x = Spore, 
                 y = shoot_dry_wt,
                 shape = N_level,
                 group = Water)) +
  ylab("Total root dry biomass (g)") +
  xlab("Spores added?") +
  scale_shape_discrete(name = "Nitrogen level", labels = c("high", "low"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

ggplot(data = toplot,
       aes(color = Water)) +
  theme_cowplot() +
  geom_boxplot(outlier.alpha = 0,
               aes(x = Spore, 
                   y = total_root_dry_cal/shoot_dry_wt)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             aes(x = Spore, 
                 y = total_root_dry_cal/shoot_dry_wt,
                 shape = N_level,
                 group = Water)) +
  ylab("Root to shoot ratio") +
  xlab("Spores added?") +
  scale_shape_discrete(name = "Nitrogen level", labels = c("high", "low"))+
  scale_color_manual(values = c("darkgoldenrod2", "royalblue2"))

toplot = mutate(toplot, root_to_shoot = total_root_dry_cal/shoot_dry_wt)

root_to_shoot_model = lm(root_to_shoot ~ Spore * N_level * Water, data = toplot)
summary(root_to_shoot_model)
