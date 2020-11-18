##############################################################################
######## Successional cohort (or PFT) Annual ANPPw. I use PFT in df's for brevity.
# This script continue from df of "NPP_2019.R"

#**************************************#
# IMPORTANT: MUST FIRST RUN "NPP_2019.R"
#**************************************#

# First, filter out week 18 (July 2020), then assign 
# early or late successional to each species. Next, we scale from DBH all the way 
# up through biomass to determine annual ANPPw of each successional cohort within
# each subplot. Through the summarize function, I lose severity and treatment, so
# I reassign them mannually (could be cleaner!)
PFT_NPP_canopy <- df %>% 
  group_by(tag) %>% # NEW WIH inc_cm
  mutate(PFT = case_when(
    species == "ACPE" ~ "late",
    species == "ACRU" ~ "late",
    species == "ACSA" ~ "late",
    species == "AMEL" ~ "late",
    species == "BEPA" ~ "early",
    species == "FAGR" ~ "late",
    species == "PIRE" ~ "late",
    species == "PIST" ~ "late",
    species == "POGR" ~ "early",
    species == "POTR" ~ "late",
    species == "QURU" ~ "late",
    species == "ABBA" ~ "late",
    species == "TSCA" ~ "late"
  )) %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%  # NEW WITH inc_cm  # NEW WITH inc_cm
  mutate(biomass_diff = biomass_new - lag(biomass_new, default = first(biomass_new))) %>% 
  group_by(subplot, PFT) %>% 
  summarise(annual_subplot = sum(biomass_diff)) %>%
  mutate(biomass_per_ha = annual_subplot*10) %>% 
  mutate(kgC_ha_yr = biomass_per_ha* 0.48) %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>% 
  mutate(severity = case_when(
    subplot == "A01E" ~ 0.85, subplot == "A01W" ~ 0.85, subplot == "A02E" ~ 0.45,
    subplot == "A02W" ~ 0.45, subplot == "A03E" ~ 0.65, subplot == "A03W" ~ 0.65,
    subplot == "A04E" ~ 0.00, subplot == "A04W" ~ 0.00, subplot == "B01E" ~ 0.00,
    subplot == "B01W" ~ 0.00, subplot == "B02E" ~ 0.45, subplot == "B02W" ~ 0.45,
    subplot == "B03E" ~ 0.85, subplot == "B03W" ~ 0.85, subplot == "B04E" ~ 0.65,
    subplot == "B04W" ~ 0.65, subplot == "C01E" ~ 0.00, subplot == "C01W" ~ 0.00,
    subplot == "C02E" ~ 0.65, subplot == "C02W" ~ 0.65, subplot == "C03E" ~ 0.85,
    subplot == "C03W" ~ 0.85, subplot == "C04E" ~ 0.45, subplot == "C04W" ~ 0.45, 
    subplot == "D01E" ~ 0.00, subplot == "D01W" ~ 0.00, subplot == "D02E" ~ 0.85,
    subplot == "D02W" ~ 0.85, subplot == "D03E" ~ 0.45, subplot == "D03W" ~ 0.45,
    subplot == "D04E" ~ 0.65, subplot == "D04W" ~ 0.65
  )) %>% 
  mutate(treatment = case_when(
    subplot == "A01E" ~ "bottom", subplot == "A01W" ~ "top", subplot == "A02E" ~ "top",
    subplot == "A02W" ~ "bottom", subplot == "A03E" ~ "bottom", subplot == "A03W" ~ "top",
    subplot == "A04E" ~ "bottom", subplot == "A04W" ~ "top", subplot == "B01E" ~ "bottom",
    subplot == "B01W" ~ "top", subplot == "B02E" ~ "top", subplot == "B02W" ~ "bottom",
    subplot == "B03E" ~ "bottom", subplot == "B03W" ~ "top", subplot == "B04E" ~ "top",
    subplot == "B04W" ~ "bottom", subplot == "C01E" ~ "top", subplot == "C01W" ~ "bottom",
    subplot == "C02E" ~ "bottom", subplot == "C02W" ~ "top", subplot == "C03E" ~ "bottom",
    subplot == "C03W" ~ "top", subplot == "C04E" ~ "top", subplot == "C04W" ~ "bottom", 
    subplot == "D01E" ~ "bottom", subplot == "D01W" ~ "top", subplot == "D02E" ~ "bottom",
    subplot == "D02W" ~ "top", subplot == "D03E" ~ "bottom", subplot == "D03W" ~ "top",
    subplot == "D04E" ~ "top", subplot == "D04W" ~ "bottom"
  )) %>% 
  select(-annual_subplot, -biomass_per_ha, NPP_canopy1 = kgC_ha_yr)
  
#bring in subcan and seedling/sapling anppw by successional cohort / PFT
PFT_aov <- PFT_NPP_canopy %>%
  left_join(PFT_NPP_subcanopy) %>% 
  left_join(PFT_NPP_seedlings)

# quickly replace the NA's produced from the joins to 0's 
PFT_aov[is.na(PFT_aov)] <- 0 

# and vector math for all strata anppw by PFT;
# NOW PFT_aov is ready for analysis 
PFT_aov <- PFT_aov %>% 
  mutate(annual_NPP = NPP_canopy1 + NPP_subcan + NPP_seedlings) %>% 
  ungroup()

# create df for figures of PFT ANPPw
annual_PFT <- PFT_aov %>% 
  group_by(severity, PFT) %>% 
  summarise(NPP_canopy = mean(annual_NPP), SE = std.error(annual_NPP)) %>% 
  group_by(severity) %>% 
  mutate(errorbars = case_when(
    PFT == "late" ~ NPP_canopy,
    PFT == "early" ~ sum(NPP_canopy)
  ))

# make severity %'s for x axis
annual_PFT$severity <- recode(annual_PFT$severity, "0.00" = "0",
                              "0.45" = "45", "0.65" = "65",
                              "0.85" = "85")

