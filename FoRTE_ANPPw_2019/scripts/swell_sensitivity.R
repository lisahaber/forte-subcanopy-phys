# This script is essentially a copy of the NPP_2019 script except with the adjustment
# for Acer rubrum bole swelling (detected in the DBLbands.R script). In order to 
# test the sensitivity of our ANPPw estimates to potential A. rubrum bole swelling,
# we we scaled ANPPw without ANY growth from A. rubrum (assuming that all A. rubrum
# growth was due to disproportionate swelling). This was the most conservtive way 
# to test for the impact of bole swelling on our ANPPw scaling. 

# After the scaling, this script runs t-tests on the adjusted and unadjusted ANPPw 
# values, and runs the overall annual ANPPw ANOVA model with the adjusted totals to 
# compare with the unadjusted model 

#**************************************#
# IMPORTANT: MUST FIRST RUN "NPP_2019.R"
#**************************************#

# Create df with an RGR_obs_est column that has the appropriate RGR for every record
# Each record will now have recorded RGR, species mean RGR, plot mean RGR, OR 
# adjusted RGR (for DBH effect on RGR). NOTE: THIS CHUNCK IS WHERE THE A. RUBRUM 
# ADJUSTMENT OCCURS (SEE SPECIES === "ACRU" FILTER BELOW)
RGR_sensitivity <-  RGR_join_fill %>% 
  mutate(RGR_obs_est = case_when(
    !is.na(RGR) ~ RGR, 
    notes == "felled" ~ 0, #manually checked all "dead"/"felled" trees and weeks
    notes == "dead" ~ 0, 
    notes == "typo" ~ 0, #sloppy, but tag 2093, wk 16 had "typo" instead of "dead"
    species == "ACRU" & week != 1 & fate == "kill" ~ 0, #SWELLING ADJUSTMENT!!; removes ACRU production
    is.na(RGR) & !is.na(mean_RGR_sp) & is.na(slope) & 
      !is.na(mean_RGR_plot) ~ mean_RGR_sp,
    is.na(RGR) & is.na(mean_RGR_sp) & is.na(slope) & 
      !is.na(mean_RGR_plot) ~ mean_RGR_plot,
    is.na(RGR) & !is.na(slope) &!is.na(mean_RGR_sp) & 
      !is.na(mean_RGR_plot) ~ DBH_cm*slope+intercept,
    is.na(RGR) & is.na(mean_RGR_sp) & is.na(slope) & 
      is.na(mean_RGR_plot) ~ mean_RGR_wk)) 

#################################################################################
#grow the D of all trees based on appropriate RGR's 
df_swell <- data.frame(RGR_sensitivity)
# this should set the RGR to 0 if negative
df_swell$RGR_obs_est[df_swell$RGR_obs_est < 0] <- 0
#split into list of data frames
df_swell.list <- split(df_swell, df_swell$tag)
df_swell.list <- lapply(df_swell.list, function(x){
  for (i in 1:nrow(x)) {
    for(j in 2:nrow(x)){
      x$DBH_cm_new[j] = (x$DBH_cm_new[j - 1] * x$RGR_obs_est[j]) + x$DBH_cm_new[j-1]
      #x$biomass_a[j] = x$biomass_b_kg_new[j - 1]
      #x$biomass_b_kg_new[j] = (x$biomass_a[j] * x$RGR_obs_est[j]) + x$biomass_a[j]
    }
    return(x)
  }
})
df_swell <- plyr::ldply(df_swell.list, data.frame)

#####################################################################################
#This chunk scales D growth to daily NPP (kgC/ha/day) and annual NPP (kgC/ha/yr)

#create a df that has the # of days (inc_days) between measurements to join with 
#summarized verion of df below
inc_days <- select(df_swell, subplot, week, inc_days) %>% unique()

#scale D growth to biomass, biomass per subplot to ha, biomass/ha to kgC, and 
#kgC/ha to /day or /year. Lastly, manually add severity and treatment because they are 
#lost in the summary function. (may be a more efficient way to do this)
NPP_sensitivity <- df_swell %>% 
  group_by(tag) %>% 
  mutate(biomass_new = case_when(
    species == "AMEL" ~ (a*(DBH_cm_new*10)^b)/1000,
    species != "AMEL" ~ a*DBH_cm_new^b
  )) %>%  
  group_by(subplot, week, date) %>% # can ADD SPECIES in here if i want to look at effect of species 
  summarise(subplot_biomass_kg = sum(biomass_new)) %>% 
  group_by(subplot) %>% 
  mutate(biomass_diff = subplot_biomass_kg - lag(subplot_biomass_kg, 
                                                 default = first(subplot_biomass_kg))) %>% 
  mutate(biomass_per_ha = biomass_diff*10) %>% 
  mutate(kgC_per_ha = biomass_per_ha*0.48) %>% 
  right_join(inc_days) %>% 
  mutate(kgC_per_ha_day = kgC_per_ha/inc_days) %>% # this is kgC/ha/day; (may need to divide by inc_days)
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
  )) 

#create week and month columns to look at different temporal scales 
NPP_sensitivity$month <- as.Date(cut(NPP_sensitivity$date, breaks = "month"))
NPP_sensitivity$week <- as.Date(cut(NPP_sensitivity$date, breaks = "week", start.on.monday = FALSE))

# make severity %'s for x axis
NPP_sensitivity$severity <- recode(NPP_sensitivity$severity, "0.00" = "0",
                             "0.45" = "45", "0.65" = "65",
                             "0.85" = "85")

#####################################################################################
#cumulative 2019 NPP   
NPP_adj_annual <- NPP_sensitivity %>% 
  select(subplot, severity, treatment, biomass_diff) %>% 
  group_by(subplot,severity, treatment) %>% 
  summarise(annual_subplot = sum(biomass_diff)) %>% 
  mutate(biomass_per_ha = annual_subplot*10) %>% 
  mutate(kgC_ha_yr = biomass_per_ha* 0.48) %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>% 
  group_by(subplot, severity, treatment, replicate) %>% 
  summarise(NPP_canopy = mean(kgC_ha_yr))

#summarize by severity 
NPP_sev_adj <- NPP_adj_annual %>% 
  select(subplot, severity, treatment, NPP_canopy) %>% 
  group_by(severity) %>% 
  summarise(kgC_ha = mean(NPP_canopy), SE = std.error(NPP_canopy))

# join w/ ACRU and w/o ACRU annual NPP df's into one and transform to prepare for a 
# t-test examining sensitivity to ACRU adjustments 

# first rename columns of each as NPP_acru and NPP_noacru
NPP_acru <- rename(NPP_2019, NPP_acru = NPP_canopy) 
NPP_noacru <- rename(NPP_adj_annual, NPP_noacru = NPP_canopy) 

# now join the acru and no acru for adj and unadjust NPP
sens <- right_join(NPP_acru, NPP_noacru)

#################################################################################
####this chunk runs stats (t.test) on the unpaired adjusted and unadjusted annual
####ANPPw 

# transform data so there's an acru/noacru column
sense_ttest <- gather(sens, adj, NPP, NPP_acru, NPP_noacru)

# run a t.test of NPP at each severity between acru and no acru
try_45 <- filter(sense_ttest, severity == 45)
t.test(NPP ~ adj, data = try_45, paired = FALSE)

try_65 <- filter(sense_ttest, severity == 65)
t.test(NPP ~ adj, data = try_65, paired = FALSE)

try_85 <- filter(sense_ttest, severity == 85)
t.test(NPP ~ adj, data = try_85, paired = FALSE)

# do the same for disturbance type
# first summarize the data for type
NPP_trt_adj <- NPP_adj_annual %>% 
  select(subplot, severity, treatment, NPP_canopy) %>% 
  group_by(treatment) %>% 
  summarise(kgC_ha = mean(NPP_canopy), SE = std.error(NPP_canopy))

try_bot <- filter(sense_ttest, treatment == "bottom")
t.test(NPP ~ adj, data = try_bot, paired = FALSE)

try_top <- filter(sense_ttest, treatment == "top")
t.test(NPP ~ adj, data = try_top, paired = FALSE)

###############################################################################
###### Now run the whole model on the adjusted NPP 


# clean up my NPP_fate df for model run. Need to run all variables as factors 
NPP_adj_annual$severity <- as.factor(NPP_adj_annual$severity)
NPP_adj_annual$replicate <- as.factor(NPP_adj_annual$replicate)
NPP_adj_annual$treatment <- as.factor(NPP_adj_annual$treatment)
#NPP_final$species <- as.factor(NPP_final$species)

#aov for all canopy strata NPP
aov.adj.annual<- aov(NPP_canopy ~ replicate + severity + treatment +
                      Error(replicate:treatment),
                    data = NPP_adj_annual)
summary(aov.adj.annual)

# run LSD post hoc for adjusted canopy NPP by severity  (error df's and error MSE)
LSD_ouput <- with(NPP_adj_annual, LSD.test(NPP_canopy, severity, 21, 564615, 
                                       console = TRUE, alpha = 0.1))

#run LSD post hoc for adj canopy NPP by dist. type
LSD_ouput <- with(NPP_adj_annual, LSD.test(NPP_canopy, treatment, 3, 1501851, 
                                           console = TRUE, alpha = 0.1))
