# This script continues from the grow loop part of NPP_2019.R. It splits ANPPw by 
# severity, type, AND fat (live/girdled trees)


#**************************************#
# IMPORTANT: MUST FIRST RUN "NPP_2019.R"
#**************************************#

# This scaling includes fate as a grouping variable to break down canopy ANPPw
# by girdled and healthy trees 
NPP_fate <- df %>%
  group_by(tag) %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>% 
  group_by(subplot, week, fate) %>% 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>% 
  arrange(subplot, fate, week) %>% 
  group_by(subplot, fate) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>%
  summarise(subplot_biomass_kg = sum(biomass_diff)) %>%
  mutate(biomass_per_ha = subplot_biomass_kg*10) %>%
  mutate(kgC_ha_yr = biomass_per_ha*0.48) %>%
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
  group_by(subplot) %>% 
  mutate(total = kgC_ha_yr + lag(kgC_ha_yr, default = last(kgC_ha_yr))) %>% #subplot total kgC_ha_yr
  mutate(perc_total = kgC_ha_yr/total) %>% 
  ungroup()

# make severity %'s for x axis
NPP_fate$severity <- recode(NPP_fate$severity, "0.00" = "0",
                              "0.45" = "45", "0.65" = "65",
                              "0.85" = "85")

# before summarizing MPP_fate, I need to add 8 cases (rows) to fit the 0% severity-
# kill category (no kill trees on control plots). I Will do this by adding 8 rows 
# to NPP_fate and manually entering the subplot, fate, and NPP
# (0 NPP because there are no kill trees) nd then filling severity, treatmnet (type),
# rep
NPP_fate <- NPP_fate %>% 
  add_row(subplot = "A04E", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "A04W", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "B01E", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "B01W", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "C01E", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "C01W", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "D01E", fate = "kill", kgC_ha_yr = 0) %>% 
  add_row(subplot = "D01W", fate = "kill", kgC_ha_yr = 0) %>% 
  group_by(subplot) %>% fill(severity, .direction = "updown") %>% 
  fill(treatment, .direction = "updown") %>% 
  fill(replicate, .direction = "updown") %>% 
  arrange(subplot)
#summarize mean NPP by fate and severity calculate SE across replicates. errorbars 
# are for the figure (becuase it is stacked, I need a column to reference when 
# assigning the error bar aesthetic in ggplot)
NPP_fate_mean <- NPP_fate %>%
  group_by(severity, fate) %>% 
  summarise(NPP_mean = mean(kgC_ha_yr), SE = std.error(kgC_ha_yr)) %>% 
  group_by(severity) %>% 
  mutate(errorbars = case_when(
    fate == "live" ~ NPP_mean,
    fate == "kill" ~ sum(NPP_mean)
  )) %>% 
  mutate(annual_NPP = case_when(
    fate == "live" ~ lag(errorbars, default = last(errorbars)),
    fate == "kill" ~ errorbars
  )) %>% 
  mutate(perc_total = NPP_mean/annual_NPP) %>% 
  ungroup() 

# df for treatment split by live-kill; EXCLUDES CONTROL TREES
# CONTROL PLOTS NEED 0 FOR KILL SPLIT
NPP_fate_tx <- NPP_fate %>%
  group_by(treatment, fate) %>% 
  summarise(NPP_mean = mean(kgC_ha_yr), SE = std.error(kgC_ha_yr)) %>% 
  group_by(treatment) %>% 
  mutate(errorbars = case_when(
    fate == "live" ~ NPP_mean,
    fate == "kill" ~ sum(NPP_mean)
  )) %>% 
  mutate(annual_NPP = case_when(
    fate == "live" ~ lag(errorbars, default = last(errorbars)),
    fate == "kill" ~ errorbars
  )) %>% 
  mutate(perc_total = NPP_mean/annual_NPP) %>% 
  ungroup()

NPP_fate_tx$treatment <- recode(NPP_fate_tx$treatment, bottom = "Bottom-Up", top = "Top-Down")


