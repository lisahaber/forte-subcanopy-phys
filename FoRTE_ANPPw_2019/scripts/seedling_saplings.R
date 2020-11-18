### This script will reorganize the 0-1cm seedling data and use hieght and base D to
### calculate seedling biomass via geometries. 

#load packages
library(dplyr)
library(plotrix)

#importing csv
seedlings_raw <- read.csv("data/seedling_sapling.csv", na.strings = c("new","NA"))

# convert factor vectors to numeric so we can do numeric calcualtions 
seedlings_raw$height_2019 <- as.numeric(as.character(seedlings_raw$height_2019))

# select the wanted columns, and filter out NA's which were veg plots without any
# seedlings present
seedlings <-  seedlings_raw %>% 
  select(subplot, uniqueID, species, baseD_cm, height_2018, height_2019, date) %>% 
  filter(!is.na(height_2018) , !is.na(height_2019)) %>% 
  filter(!is.na(species)) %>% 
  mutate(D_cm = case_when(
    baseD_cm == "0-1" ~ 0.5,
    baseD_cm == "1-2" ~ 1.5,
    baseD_cm == "2-3" ~ 2.5
  ))

# Use filter to create two seperate df's for each increment: June and August 
# but first need to convert date column into data format, not factor
seedlings$date <- as.Date(as.character(seedlings$date,"%Y-%m-%d"))

june_seedlings <- seedlings %>% 
  filter(date == "2019-06-19" | date == "2019-06-13" | date == "2019-06-14") %>% 
  mutate(volume_2018_cm3 = pi*((0.5*D_cm)^2)*(height_2018/3)) %>% 
  mutate(volume_2019_cm3 = pi*((0.5*D_cm)^2)*(height_2019/3)) %>% 
  mutate(density_g_cm3 = case_when(
    species == "PIRE" ~ 0.41,
    species == "ACPE" ~ 0.44,
    species == "ACRU" ~ 0.49,
    species == "ACSA" ~ 0.56,
    species == "FRPE" ~ 0.53,
    species == "POGR" ~ 0.36,
    species == "QURU" ~ 0.56,
    species == "AMEL" ~ 0.66,
    species == "FAGR" ~ 0.56,
    species == "FRAM" ~ 0.55,
    species == "PIST" ~ 0.34,
    species == "VBAC" ~ 0.72
  )) %>% 
  mutate(biomass_2018_kg = (volume_2018_cm3*density_g_cm3)/1000) %>% 
  mutate(biomass_2019_kg = (volume_2019_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC_total = (biomass_2019_kg - biomass_2018_kg) * 0.48)
  
aug_seedlings <- seedlings %>% 
  filter(date == "2019-08-02" | date == "2019-08-05" | date == "2019-08-06") %>% 
  mutate(volume_2018_cm3 = pi*((0.5*D_cm)^2)*(height_2018/3)) %>% 
  mutate(volume_2019_cm3 = pi*((0.5*D_cm)^2)*(height_2019/3)) %>% 
  mutate(density_g_cm3 = case_when( #VBAC is V. lantana (european viberunum)
    species == "PIRE" ~ 0.41,
    species == "ACPE" ~ 0.44,
    species == "ACRU" ~ 0.49,
    species == "ACSA" ~ 0.56,
    species == "FRPE" ~ 0.53,
    species == "POGR" ~ 0.36,
    species == "QURU" ~ 0.56,
    species == "AMEL" ~ 0.66,
    species == "FAGR" ~ 0.56,
    species == "VBAC" ~ 0.72, # need to subtract aug Vbac from june (mis-ID'ed VBAC)
    species == "FRAM" ~ 0.55, 
    species == "PIST" ~ 0.34
  )) %>% 
  mutate(biomass_2018_kg = (volume_2018_cm3*density_g_cm3)/1000) %>% 
  mutate(biomass_2019_kg = (volume_2019_cm3*density_g_cm3)/1000) %>% 
  mutate(kgC_total = (biomass_2019_kg - biomass_2018_kg) * 0.48) 

# go from kgC to NPP; this is end of season accumulated biomass, does not include 
# june measurements (june could be mid season measuremnt)
NPP_seedlings <- aug_seedlings %>% 
  group_by(subplot, uniqueID) %>% 
  summarise(kgC_m2 = sum(kgC_total)) %>% 
  ungroup() %>% 
  group_by(subplot) %>% 
  summarise(NPP_seedlings = mean(kgC_m2)*10000) # this is kgC/ha/yr = NPP

# create a df with NPP_seedlings grouped by PFT for PFT analysis 
PFT_NPP_seedlings <- aug_seedlings %>% 
  mutate(PFT = case_when( 
  species == "POGR" ~ "early",
  species != "POGR" ~ "late"
  )) %>% 
  group_by(subplot, uniqueID, PFT) %>% 
  summarise(kgC_m2 = sum(kgC_total)) %>% 
  ungroup() %>% 
  group_by(subplot, PFT) %>% 
  summarise(NPP_seedlings = mean(kgC_m2)*10000)

# calculate species comp and stem density for table in ms
# will use august data for species comp because maple viburnum was mis-ID'd in june
# will use June data for seedlings stem density to have as close to predisturbance 
# as possible 

seedling_comp <- aug_seedlings %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>%
  group_by(replicate, species) %>% 
  count(species) %>% 
  group_by(replicate) %>% 
  mutate(total = sum(n)) %>% 
  mutate(percent = n/total) %>% 
  arrange(replicate, percent)

# calcualate stem density per hectare 

seedling_density <- june_seedlings %>% 
  group_by(subplot, uniqueID) %>% 
  count(uniqueID) %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>%
  group_by(replicate) %>% 
  summarise(mean_count_m2 = mean(n), SE = std.error(n)) %>% 
  mutate(mean_count_ha = mean_count_m2*10000) %>% 
  mutate(SE_ha = SE*10000)

