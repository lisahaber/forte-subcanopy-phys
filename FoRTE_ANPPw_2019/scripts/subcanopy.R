###########################################################
## Downloaded from Max Grigri's figshare repository
## DOI: https://doi.org/10.6084/m9.figshare.12442703
## Data and code used for his NPP study in FoRTE, 2019 
###########################################################

# This script brings in subcanopy stem counts (to the species level) that were 
# perfromed in one quadrant (0.025ha) of each subplot, and the subcanopy repeated
# diameter measures from the 2019 growing season. It calculates mean annual
# subplot subcanopy ANPPw from the growth increment between diameter measurements 
# in May 2019 and August 2019

#load necessary packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(plotrix)

# importing tree count data 
tree_counts <- read.csv("C:/github/forte-subcanopy-phys/FoRTE_ANPPw_2019/data/subcanopy_stemcounts.csv")
# select columns of interest
tree_counts <-  select(tree_counts, subplot, species, count)

#importing csv of subcanopy diameter measurements 
subcanopy_data <- read.csv("C:/github/forte-subcanopy-phys/FoRTE_ANPPw_2019/data/subcanopy_D.csv")

# this chunk of code stands alone (not used in ANPPw calcs, but interesting data!)
# It calculates subcanopy stem density per subplot and scaled to the ha. 
# Then summarize stems/ha in each replicate with SE among subplots within a replicate 
subcan_stem_density <- tree_counts %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>%
  group_by(subplot, replicate) %>% 
  summarize(quart_subplot = sum(count)) %>% 
  mutate(stems_per_ha = quart_subplot*40) %>% #counts were done in 0.025 ha
  group_by(replicate) %>% 
  summarize(rep_stem_density = mean(stems_per_ha), SE = std.error(stems_per_ha))

# This chunk also stands alone. It calculate the subcanopy species composition 
# within each replicate.
subcan_comp1 <- tree_counts %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>% 
  select(subplot, replicate, species, count) %>% 
  group_by(replicate, species) %>% 
  summarise(total_stems = sum(count)) #%>% # this is in 0.025 ha plots (1/4 subplots)

subcan_comp <- subcan_comp1 %>%   
  summarise(total_stems_rep = sum(total_stems)) %>% 
  right_join(subcan_comp1) %>% 
  mutate(perc_comp = total_stems/total_stems_rep) 

##################################################################################
# Scale diameter increment to subcanopy ANPPw

# create function for biomass based on allometric parameters (Cooper, 1981) 
# for each species 

biomass_a <- function(species, DBH){
  if (species == "ACRU"){
    biomass <- 0.03117 * (DBH) ^ 2.7780
  } else if (species == "ACPE"){
    biomass <-  0.2040 * DBH ^ 2.2524
  } else if (species == "ACSA"){
    biomass <- 0.1693 * DBH ^ 2.3436
  } else if (species == "AMEL"){
    biomass <- (0.1630 * (DBH * 10) ^ 2.4940)/1000
  } else if (species == "FAGR"){
    biomass <- 0.1892 * DBH ^ 2.3097
  } else if (species == "PIRE"){
    biomass <- 0.0526 * DBH ^ 2.5258
  } else if (species == "PIST"){
    biomass <- 0.0408 * DBH ^ 2.5735
  } else if (species == "POGR"){
    biomass <- 0.1387 * DBH ^ 2.3498
  } else if (species == "QURU"){
    biomass <- 0.0398 * DBH ^ 2.7734
  }
  return(biomass)
}

# vectorize my function becuase it will be used on a vetor 
biomass_a <- Vectorize(biomass_a, vectorize.args = c("species", "DBH"))

# select important columns from subcanopy data. Then remove records where DBH_mm
# was NA (some stems are missing week 1 and 2 of data collection). Next I convert
# DBH in mm to cm, and use my biomass_a function to estimate biomass(kg)
# (DBH allometry) 
subcanopy_select <- subcanopy_data %>% 
  select(subplot, uniqueID, species, tag, DBH_mm, date) %>% 
  filter(!is.na(DBH_mm)) %>% 
  mutate(DBH_cm = DBH_mm/10) %>%
  mutate(biomass_kg = biomass_a(species, DBH_cm)) 

# create a weeks column based on the date 
subcanopy_select$date <- as.Date(subcanopy_select$date,"%Y-%m-%d")
subcanopy_select$week <- as.Date(cut(subcanopy_select$date, breaks = "week", 
                                     start.on.monday = TRUE))

# create a DOY column
subcanopy_select$DOY <- yday(subcanopy_select$date)

# filter subcanopy select for for first and last DBH measurement for every sample
# here is the first measurement of every sampled tree
may_2019 <- subcanopy_select %>% 
  group_by(tag) %>% 
  filter(row_number() == 1) %>% 
  mutate(biomass_kg = biomass_a(species, DBH_cm)) 

# here is the final measurement of every sampled tree
aug_2019 <- subcanopy_select %>% 
  group_by(tag) %>% 
  filter(row_number() == n()) %>% 
  mutate(biomass_kg = biomass_a(species, DBH_cm)) 

##############################################################################
## Rank species by standing biomass in August 2019 and then compare 
## with your sampled trees
#############################################################################

biomass_ranks <- aug_2019 %>%
  group_by(subplot, species) %>%
  summarize(total_biomass = sum(biomass_kg)) %>%
  arrange(desc(total_biomass), .by_group = TRUE) %>% 
  right_join(tree_counts) %>% arrange(subplot, species) %>% 
  group_by(subplot) %>% 
  mutate(sp_biomass_ha = total_biomass * count * 40) %>%
  arrange(desc(sp_biomass_ha), .by_group = TRUE) %>%
  na.omit()

biomass_ranks <- biomass_ranks %>%
  group_by(subplot) %>%
  mutate(total_biomass = sum(sp_biomass_ha))


biomass_ranks <- biomass_ranks %>%
  group_by(subplot) %>%
  mutate(percent_biomass = sp_biomass_ha/total_biomass*100)



write.csv(biomass_ranks, file = "ranked_biomass_by_species_subplots_subcanopy.csv")
 


#############################################################################
#############################################################################
## try by replicate
biomass_ranks_replicate <- aug_2019 %>%
  group_by(subplot, species) %>%
  summarize(total_biomass = sum(biomass_kg)) %>%
  arrange(desc(total_biomass), .by_group = TRUE) %>% 
  right_join(tree_counts) %>% arrange(subplot, species) %>% 
  group_by(subplot) %>% 
  mutate(sp_biomass_ha = total_biomass * count * 40) %>%
  arrange(desc(sp_biomass_ha), .by_group = TRUE) %>%
  na.omit() %>%
  mutate(replicate = substr(subplot, 1, 1))





# now I bind these two dfs together, arrange and group for organization. Next, I do
# some vector math to calculat the biomass increment (biomass_inc). Then, I filter 
# for the end of season measuremnt (no increment for first measurement), and again, 
# select and arrange for organization
sub_annual_inc <- bind_rows(may_2019, aug_2019) %>% 
  arrange(tag) %>% group_by(tag) %>% 
  mutate(biomass_inc = biomass_kg - lag(biomass_kg, default = first(biomass_kg))) %>% 
  filter(row_number() == n()) %>% 
  select(subplot, species, tag, biomass_kg, biomass_inc) %>% 
  arrange(subplot)

# get rid of the negatives in biomass increments. 
# Negatives likely result from inherent error in measuring diameter with 
# digital calipers 
sub_annual_inc$biomass_inc[sub_annual_inc$biomass_inc < 0] <- 0
  
# Now I will move from biomass increment to subcanopy NPP. To scale from my sampled 
# population to the entire population, I first calculate and create a new vector 
# with mean subplot biomass increment. This is then used for subcanopy species that 
# are within the subplot, but that I did not capture in my sample. I then join the 
# tree counts df which creates a new record within subplots for species that were 
# not captures in my sample population. Next, I fill the mean subplot increments, and
# create a new vector that has either the measured increment (sampled species) or the 
# mean increment (unsampled species). Then, I summarize for mean_biomass for each
# subplot, and species, rejoin tree_counts (lost in the summarize), and scale up to 
# the hectare. Lastly, I sum the biomass production of all species within a subplot
# and multiply by a site specific carbon fraction of 0.48 for kgC_ha_yr
NPP_subcanopy <- sub_annual_inc %>% 
  group_by(subplot) %>% 
  summarize(mean_subplot = mean(biomass_inc)) %>% 
  left_join(sub_annual_inc) %>% 
  right_join(tree_counts) %>% arrange(subplot, species) %>% 
  group_by(subplot) %>% fill(mean_subplot, .direction = "updown") %>% 
  group_by(subplot, species) %>% 
  mutate(biomass_inc_obs_est = case_when(
    !is.na(biomass_inc) ~ biomass_inc,
    is.na(biomass_inc) ~ mean_subplot
  )) %>% 
  summarize(mean_biomass = mean(biomass_inc_obs_est)) %>% 
  right_join(tree_counts) %>% 
  mutate(sp_biomass_ha = mean_biomass * count * 40) %>% 
  # * species count * 40 b/c counts were in 0.025ha
  group_by(subplot) %>% 
  summarize(kgC_ha_yr = sum(sp_biomass_ha) * 0.48) %>% 
  rename(NPP_subcan = kgC_ha_yr)

######################################################################
##
##
######################################################################




# create df with a PFT column for PFT analysis; 
# NO POGR (PFT = early) IN THIS DATA SET! so no need to sort by species 
PFT_NPP_subcanopy <- NPP_subcanopy %>% 
  mutate(PFT = case_when(
    !is.na(NPP_subcan) ~ "late",
    is.na(NPP_subcan) ~ "early"
  ))

