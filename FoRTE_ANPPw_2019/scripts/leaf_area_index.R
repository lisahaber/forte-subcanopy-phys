# this script takes LAI data collected with hemispherical images, plots the mean 
# end-of-season LAI by disturbance severity and type, and runs an ANOVA to test for
# significant differences. It also includes a summary of LAI in 

# reformat LAI data and make a histogram of end of season LAI among severities and types 

#load packages 
library(dplyr)
library(ggplot2)
library(agricolae)
library(plotrix)
library(gridExtra)
library(grid)

#import data
camera_data <- read.csv("data/FORTE_LAI.csv")

#select columns of interest and rename columns to match other format
LAI <- select(camera_data, SubplotID, severity, treatment, Date, Day, LAI_cam)
names(LAI) <- c("subplot", "severity", "treatment", "date", "DOY", "LAI")
LAI <- arrange(LAI, subplot)

# filter for end of season measurements and summarize for mean subplot LAI. This df 
# is used for the statistical analysis
LAI_end <- LAI %>% 
  filter(DOY == 214 | DOY == 213 | DOY == 215) %>% 
  group_by(subplot, severity, treatment) %>% 
  summarise(mean_LAI = mean(LAI)) %>% 
  mutate(replicate = substr(subplot, 1, 1)) 

# create a couple df's to use for ggplotting 
# create df for summarized LAI by severity 
LAI_severity <- LAI_end %>% 
  group_by(severity) %>% 
  summarize(LAI_mean = mean(mean_LAI), SE = std.error(mean_LAI))

#create df for summarized LAI by treatment (dist. type)
LAI_type <- LAI_end %>% 
  group_by(treatment) %>% 
  summarise(LAI_mean = mean(mean_LAI), SE = std.error(mean_LAI))

# recode type to read bottom up and top down for figures
LAI_type$treatment <- recode(LAI_type$treatment, B = "Bottum-Up")
LAI_type$treatment <- recode(LAI_type$treatment, T = "Top-Down")

