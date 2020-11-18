# This script is the pipeline from dendrometer bands measurements of ~700 trees
# across the entire FoRTE experiment to ANPPw. First, it runs the subcanopy, and 
# seedling and sapling scripts using the source function. Then it begins the pipeline
# After ANPPw is calculated, it summarize data and prepares df's for ggplot2 in the 
# "figures.R" script, as well as, the "stats.R" script. 

# load packages i use 
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(gridExtra)
library(grid)
library(ggpubr)
library(lubridate)

# use source to run subcanopy scripts 
source("scripts/subcanopy.R")
source("scripts/seedling_saplings.R")

#remove clutter from global environmnet from the subcanopy and seedling scripts
remove(seedling_comp, seedling_density, seedlings, seedlings_raw, subcan_comp, 
       subcan_stem_density, subcanopy_data, subcanopy_select, tree_counts, 
       aug_seedlings, june_seedlings, sub_annual_inc, subcan_comp1, may_2019, 
       aug_2019)

#importing csv
dendro_2019 <- read.csv("data/canopy_dendrobands.csv", na.strings = c("", "NA"))

#arrange dendro data by tag a date 
dendro_data <- arrange(dendro_2019, tag, week)
#bring in all trees tagged experiment wide (all trees within experimental subplots)
all_trees <- read.csv("data/FoRTE_all_trees.csv") %>% arrange(Tag)

#selected only for columns that I will use 
dendro_data <- select(dendro_data, week, subplot, tag, 
                   fate, species, DBH_cm, bands_in, DOY, date, notes) %>%
  mutate(bands_cm= bands_in*2.54) %>% 
  filter(!is.na(bands_cm))

#create uniqueID for dendrodata 
dendro_data$uniqueID <- paste(dendro_data$subplot, dendro_data$week,
                                  dendro_data$species, dendro_data$fate, sep = "_")

#select columns
all_trees <- all_trees %>% 
  select(SubplotID, Species, Tag, fate, dbh)

#rename column names to match dendro_data
names(all_trees) <- c("subplot", "species", "tag", "fate", "DBH_cm")


#creating 18 of each tag to build a week column and prep for join; then join and fill in DOY 
# THIS COULD BE CLEANER: Needs to be changed everytime new data is collected 
# need to add a and b to this df because POTR, TSCA, AMEL are not in my sampled population
# and therefore will not have a or b's when I join this table with sample pop. 
# (COULD BE CLEANER!!)
all_trees_weeks <- all_trees %>% 
  #dplyr::setdiff(all_reps)
  slice(rep(1:n(), each = 17)) %>% 
  # this number must match # of data points (i.e 17 weeks from Nov. 2018-Nov. 2019) 
  group_by(tag) %>% 
  mutate(week = row_number()) %>% 
  mutate(a = case_when(
    species == "ACPE" ~ 0.03117,
    species == "ACRU" ~ 0.03177,
    species == "ACSA" ~ 0.1693,
    species == "AMEL" ~ 0.1630,
    species == "BEPA" ~ 0.0301,
    species == "FAGR" ~ 0.1892,
    species == "PIRE" ~ 0.0526,
    species == "PIST" ~ 0.0408,
    species == "POGR" ~ 0.1387,
    species == "POTR" ~ 0.0589,
    species == "QURU" ~ 0.0398,
    species == "ABBA" ~ 0.0705,
    species == "TSCA" ~ 0.1617
  )) %>% 
  mutate(b = case_when(
    species == "ACPE" ~ 2.7780,
    species == "ACRU" ~ 2.7780,
    species == "ACSA" ~ 2.3436,
    species == "AMEL" ~ 2.4940,
    species == "BEPA" ~ 2.8387,
    species == "FAGR" ~ 2.3097,
    species == "PIRE" ~ 2.5258,
    species == "PIST" ~ 2.5735,
    species == "POGR" ~ 2.3498,
    species == "POTR" ~ 2.6235,
    species == "QURU" ~ 2.7734,
    species == "ABBA" ~ 2.4970,
    species == "TSCA" ~ 2.1536
  ))

# create week_tag unique ID
all_trees_weeks$wk_tag <- paste(all_trees_weeks$week, all_trees_weeks$tag, sep = "_")

#create subplot_week_species, fate unique_ID for application of DBH - RGR adjustment
#further down the pipe
all_trees_weeks$uniqueID <- paste(all_trees_weeks$subplot, all_trees_weeks$week,
                              all_trees_weeks$species, all_trees_weeks$fate, sep = "_")
#building df with all trees sampled and unsampled
#moving from raw increment to both delta biomass AND RGR 
#manually enter the appropriate a and b coefficients from (Cooper et al.,)
# POTENTIAL CLEAN: not sure I need to add a & b's here if I add them to all_trees_weeks
# above
df1 <- dendro_data %>% #keeping this only measured trees and weeks, will add in unsampled trees later 
  group_by(tag) %>%
  arrange(tag, week) %>% 
  select(-bands_in, -date)
  
# create a unique name for each tag and week in order to filter out certain weeks 
df1$wk_tag <- paste(df1$week, df1$tag, sep = "_")

#calculate increment in circumference cm's and divide by pi for radial inc
#create new column DBH_cm_new for the first band read of all sampled trees 
#(the first record of each grou of "tag"); this new column will be populated 
#with the new DBH for each sampled tree (based on measured growth)
df1 <- df1 %>%   
  mutate(inc_cm = (bands_cm - lag(bands_cm, default = first(bands_cm)))/pi) %>% 
  group_by(tag) %>% 
  mutate(DBH_cm_new = case_when(
    row_number()==1 ~ DBH_cm))

#zero out negative growth (assuming trees can't shrink)
df1$inc_cm[df1$inc_cm < 0] <- 0

##################################################################################
# time to radially grow my sampled trees based on measured radial growth 
# further down the pipeline I will use this measured growth to calculate RGR's 
# for each species, week, subplot, and fate 

# split df1 into tags to apply the increment growth functions and loops
df1 <- data.frame(df1)
df1.list <- split(df1, df1$tag)
# Create a function that grows measured trees based on previous DBH and DBH increment
increment <- function(x) {
  for (i in 1:nrow(x)) {
    for(j in 2:nrow(x)){
      x$DBH_cm_new[j] = x$DBH_cm_new[j - 1] + (x$inc_cm[j])
    }
    return(x)
  }
}
# apply this function to the split list of tags (trees)
df1.list_grow <- lapply(df1.list, increment)
# paste lists back together as a df 
df1 <- plyr::ldply(df1.list_grow, data.frame)

# remove clutter from global environment
remove(df1.list, df1.list_grow)

##################################################################################
# Use DOY to create dates 
# also correct the Nov. 2018 date (they were interpretted as all the same year)
df1$DOY <- as.numeric(df1$DOY)
dates <- data.frame(as.factor(as.Date(df1$DOY, "2019-01-01")))
names(dates) <- c("date")
dates <- data.frame(recode_factor(dates$date, '2019-11-03' = "2018-11-15"))
names(dates) <- c("date")
dates$date <- as.Date(dates$date,"%Y-%m-%d")

# join dates column back with df1 
df1 <- bind_cols(df1, dates)
#remove clutter
remove(dates)

###############################################################################
#calculate RGR and number of days between dates (inc_days)
RGR_plot <- df1 %>% 
  group_by(tag) %>% 
  mutate(BETWEEN0=as.numeric(difftime(date,lag(date,1))), 
         inc_days=ifelse(is.na(BETWEEN0),0,BETWEEN0),
         total_inc=cumsum(as.numeric(inc_days)))%>%
  select(-BETWEEN0)  %>%
  mutate(RGR = inc_cm / lag(DBH_cm_new, default = first(DBH_cm_new))) 

# generate mean RGR for each subplot, wk, species, fate to use as RGR for unsampled trees
RGR_wk_sp_fate <- RGR_plot %>%
  group_by(tag) %>% 
  filter(row_number()!=1) %>% ungroup() %>% # this filters out the first record for each tag (RGR is NaN)
  group_by(subplot, week, date, species, fate) %>% 
  summarise(mean_RGR_sp= mean(RGR))

# generate mean RGR for each subplot, week, fate to use as RGR for unsampled trees for which
# there is no sampled species in the subplot (i.e. i did not sample oaks in A03E, but 
# there ARE oaks in A03E)
RGR_wk_fate <- RGR_plot %>%
  group_by(tag) %>% 
  filter(row_number()!=1) %>% ungroup() %>% 
  group_by(subplot, week, date, fate) %>% 
  summarise(mean_RGR_plot= mean(RGR))

# generate mean RGR for each subplot, week (excluding fate for the 7-8 trees/weeks 
# that no species, week, fate was measured)
RGR_wk <- RGR_plot %>%
  group_by(tag) %>% 
  filter(row_number()!=1) %>% ungroup() %>% 
  group_by(subplot, week, date) %>% 
  summarise(mean_RGR_wk= mean(RGR))

# join above df's with RGR_plot so that each tree has either mean species RGR, mean_plot RGR,
# or measured RGR
RGR_mean_wk_sp <- right_join(RGR_wk_sp_fate, RGR_plot)
RGR_mean <- right_join(RGR_wk_fate, RGR_mean_wk_sp)
RGR_plot_mean <- right_join(RGR_wk, RGR_mean) %>% arrange(tag, week)

# remove some clutter from global environment
remove(all_trees, dendro_2019, dendro_data, df1)

##################################################################################
#This chunk takes sampled trees  and uses regression equations to measure in which
#subplot, week, fate, species DBH had a significant relationship with growth. THis 
#information is then used to adjust RGR's that were significantly (p<0.05) related to
#DBH

#select for RGR, tag, date, subplot, species, DBH
RGR_plot_regression <- RGR_plot_mean %>%
  filter(!is.na(RGR)) %>% 
  select(subplot, week, tag, species, 
         fate, DBH_cm_new, DOY, RGR) %>% 
  group_by(tag) 

#scale to NPP by subplot, week, species, AND DBH (where necessary)
#runs regression for each subplot, week, and species AND returns object with 
#coefficients; SHOULD INCLUDE FATE HERE~~!!
regressions <- RGR_plot_regression %>% 
  group_by(subplot, week, species, fate) %>% 
  do(mod = summary(lm(RGR ~ DBH_cm_new, data = .))) #%>% #-1 removes the intercept

regressions$uniqueID <- paste(regressions$subplot, regressions$week, 
                              regressions$species, regressions$fate, sep = "_")

final_output <- matrix(nrow= 3064, ncol=4, dimnames = list(c(), 
                                                           c("uniqueID", "pvalue", 
                                                             "slope", "intercept")))

for(i in 1:nrow(regressions)) {
  
  final_output[i, 4] <- regressions$mod[[i]][4]$coefficients[1]
  final_output[i, 3] <- regressions$mod[[i]][4]$coefficients[2]
  final_output[i, 2] <- regressions$mod[[i]][4]$coefficients[8]
  final_output[i, 1] <- regressions$uniqueID[[i]] #may need [,i] or [i,]
  
} 

#convert matrix to data frame 
final_output_df <- as.data.frame(final_output) 

#remove NA's: NEED TO FIGURE OUT WHY NA'S VS NaN'S 
final_output_df_NAomit <-   filter(final_output_df, pvalue != "NaN", !is.na(pvalue)) 

#coerce pvalue, slope, and intercept into character and then numeric
final_output_df_NAomit$pvalue <-  as.numeric(as.character(final_output_df_NAomit$pvalue))
final_output_df_NAomit$slope <-  as.numeric(as.character(final_output_df_NAomit$slope))
final_output_df_NAomit$intercept <-  as.numeric(as.character(final_output_df_NAomit$intercept))

#filter for pvalue's < 0.05; subplot, weeks, and species where DBH is having a sig effect 
sig_DBH_effect <- filter(final_output_df_NAomit, pvalue < 0.05)

#####################################################################################
#create new RGR_obs_est_df for calculated RGR (sampled trees), avg_RGR (unsampled trees), 
#adj_RGR (for unique id's where DBH had significant effect on RGR)
#first bring in ALL unsampled trees and/or missing weeks and assign a RGR using
#group_by and fill functions
# DISCLAIMER: total_inc is off on all "new bands" 
RGR_join_fill <- RGR_plot_mean %>% 
  right_join(all_trees_weeks) %>% arrange(tag, week) %>% 
  group_by(subplot, week) %>% 
  fill(c(DOY, inc_days, total_inc, date, mean_RGR_wk), .direction = "updown") %>% 
  group_by(subplot, week, fate) %>% 
  fill(c(mean_RGR_plot), .direction = "updown") %>% 
  group_by(subplot, week, species, fate) %>% 
  fill(mean_RGR_sp, .direction = "updown") %>% 
  #this is a quick fix for total_inc and DBH_cm_new; take them out and redo
  #so that newly joined records from all_trees_weeks have them
  select(-inc_days, -total_inc, -DBH_cm_new) %>% 
  group_by(tag) %>% 
  fill(notes, .direction = "down") %>% #ensures "dead" and "felled" trees are not grown
  mutate(BETWEEN0=as.numeric(difftime(date,lag(date,1))), 
         inc_days=ifelse(is.na(BETWEEN0),0,BETWEEN0),
         total_inc=cumsum(as.numeric(inc_days))) %>%
  select(-BETWEEN0) %>% 
  left_join(sig_DBH_effect) %>% 
  mutate(DBH_cm_new = case_when(
    row_number()==1 ~ DBH_cm)) 

# Create df with an RGR_obs_est column that has the appropriate RGR for every record
# Each record will now have recorded RGR, species mean RGR, plot mean RGR, OR 
# adjusted RGR (for DBH effect on RGR)
RGR_obs_est_df <-  RGR_join_fill %>% 
  mutate(RGR_obs_est = case_when(
    !is.na(RGR) ~ RGR, 
    notes == "felled" ~ 0, #manually checked all "dead"/"felled" trees and weeks
    notes == "dead" ~ 0, 
    notes == "typo" ~ 0, #sloppy, but tag 2093, wk 16 had "typo" instead of "dead"
    #species == "ACRU" & week != 1 & fate == "kill" ~ 0, #SWELLING ADJUSTMENT!!; removes ACRU production
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
df <- data.frame(RGR_obs_est_df)
# this should set the RGR to 0 if negative
df$RGR_obs_est[df$RGR_obs_est < 0] <- 0
#split into list of data frames
df.list <- split(df, df$tag)
df.list <- lapply(df.list, function(x){
  for (i in 1:nrow(x)) {
    for(j in 2:nrow(x)){
      x$DBH_cm_new[j] = (x$DBH_cm_new[j - 1] * x$RGR_obs_est[j]) + x$DBH_cm_new[j-1]
      #x$biomass_a[j] = x$biomass_b_kg_new[j - 1]
      #x$biomass_b_kg_new[j] = (x$biomass_a[j] * x$RGR_obs_est[j]) + x$biomass_a[j]
    }
    return(x)
  }
})
df <- plyr::ldply(df.list, data.frame)

#remove clutter from global environment
remove(df.list, final_output,final_output_df,
       final_output_df_NAomit, regressions, RGR_obs_est_df, RGR_plot, 
       RGR_plot_regression, RGR_wk, RGR_mean_wk_sp, RGR_wk_fate, RGR_wk_sp_fate,
       sig_DBH_effect, RGR_mean, RGR_plot_mean, all_trees_weeks)

#####################################################################################
#This chunk scales D growth to daily NPP (kgC/ha/day) and annual NPP (kgC/ha/yr)

#create a df that has the # of days (inc_days) between measurements to join with 
#summarized verion of df below
inc_days <- select(df, subplot, week, inc_days) %>% unique()

#scale D growth to biomass, biomass per subplot to ha, biomass/ha to kgC, and 
#kgC/ha to /day or /year. Lastly, manually add severity and treatment because they are 
#lost in the summary function. (may be a more efficient way to do this)
NPP_final <- df %>% 
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
  filter(!is.na(kgC_per_ha_day)) %>% 
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
NPP_final$month <- as.Date(cut(NPP_final$date, breaks = "month"))
NPP_final$week <- as.Date(cut(NPP_final$date, breaks = "week", start.on.monday = FALSE))

# make severity %'s for x axis
NPP_final$severity <- recode(NPP_final$severity, "0.00" = "0",
                                 "0.45" = "45", "0.65" = "65",
                                 "0.85" = "85")

# filter for 2020 and summarize by severity as of July 26, 2020
NPP_july_2020 <- NPP_final %>% 
  select(subplot, severity, treatment, kgC_per_ha) %>% 
  group_by(severity) %>% 
  summarise(kgC_ha = mean(kgC_per_ha), SE = std.error(kgC_per_ha))

#####################################################################################
#cumulative 2019 NPP   
NPP_2019 <- NPP_final %>% 
  select(subplot, severity, treatment, biomass_diff) %>% 
  group_by(subplot,severity, treatment) %>% 
  summarise(annual_subplot = sum(biomass_diff)) %>% 
  mutate(biomass_per_ha = annual_subplot*10) %>% 
  mutate(kgC_ha_yr = biomass_per_ha* 0.48) %>% 
  mutate(replicate = substr(subplot, 1, 1)) %>% 
  group_by(subplot, severity, treatment, replicate) %>% 
  summarise(NPP_canopy = mean(kgC_ha_yr))

#summarize by severity 
NPP_2019_sev <- NPP_2019 %>% 
  select(subplot, severity, treatment, NPP_canopy) %>% 
  group_by(severity) %>% 
  summarise(kgC_ha = mean(NPP_canopy), SE = std.error(NPP_canopy))

#summarize by disturbance type
NPP_2019_tx <- NPP_2019 %>% 
  select(subplot, severity, treatment, NPP_canopy) %>% 
  group_by(treatment) %>% 
  summarise(kgC_ha = mean(NPP_canopy), SE = std.error(NPP_canopy))

# severity time series df 
severity_time <- NPP_final %>% 
  group_by(week, severity) %>% 
  summarize(NPP = mean(kgC_per_ha_day), SE = std.error(kgC_per_ha_day)) %>% 
  mutate(DOY = yday(week))

# treatment (type) time series df
treatment <- NPP_final %>% 
  group_by(week, treatment) %>% 
  summarize(NPP = mean(kgC_per_ha_day), SE = std.error(kgC_per_ha_day)) %>% 
  mutate(DOY = yday(week))

####################################################################################
# join NPP subcanopy and seedlings data, create strata column and ID NPP by canopy,
# subcanopy, and seedling
all_strata <- NPP_2019 %>%
  left_join(NPP_subcanopy) %>% 
  left_join(NPP_seedlings) %>% 
  mutate(annual_NPP = NPP_canopy + NPP_subcan + NPP_seedlings) %>% 
  ungroup()

all_strata$treatment <- recode(all_strata$treatment, bottom = "zbottom")

#annual NPP by treatments
annual_NPP_tx <- all_strata %>% 
  group_by(severity, treatment) %>% 
  summarise(NPP = mean(annual_NPP), SE = std.error(annual_NPP)) %>% 
  group_by(severity) %>% 
  mutate(errorbars = case_when(
    treatment == "zbottom" ~ NPP,
    treatment == "top" ~ sum(NPP)
  ))

# Create df for annual NPP figure 
annual_NPP_fig <- all_strata %>% 
  group_by(severity) %>% 
  summarise(NPP_canopys = mean(NPP_canopy), SE1 = std.error(NPP_canopy),
            NPP_subcans = mean(NPP_subcan), SE2 = std.error(NPP_subcan),
            NPP_zseedlings = mean(NPP_seedlings), SE3 = std.error(NPP_seedlings)) %>% 
  gather(canopy_strata, annual_NPP, NPP_canopys, NPP_subcans, NPP_zseedlings) %>% 
  mutate(SE = case_when(
    canopy_strata == "NPP_zseedlings" ~ SE3,
    canopy_strata == "NPP_subcans" ~ SE2,
    canopy_strata == "NPP_canopys" ~ SE1
  )) %>% 
  group_by(severity) %>% 
  arrange(severity) %>% 
  mutate(errorbars = case_when(
    canopy_strata == "NPP_zseedlings" ~ annual_NPP,
    canopy_strata == "NPP_subcans" ~ sum(annual_NPP) - lag(annual_NPP, default = first(annual_NPP)),
    canopy_strata == "NPP_canopys" ~ sum(annual_NPP)
  ))
