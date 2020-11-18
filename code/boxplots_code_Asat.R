##############################
## Cleaned up code for Max
## L. Haber
## 02/12/2020
## FoRTE Subcanopy Leaf Phys Visualiztion
##############################

## load packages
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)

## load data
## sorry, Max, you'll need to correct the file path...
all_data <- read.csv("C:/github/forte-subcanopy-phys/Mean_Asat_values_subplots_2018_2019_Corrected.csv")
head(all_data)

## create factor levels for boxplots
all_data$Severity <- as.factor(all_data$Severity)

## color scheme
forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")

## boxplots
boxplots <- all_data %>%
  na.omit() %>%
  group_by(Severity) %>%
  ggplot(aes(x = Severity, y = mean_Asat, group_by(Severity), fill = Severity)) + 
  theme_classic(base_size = 15) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Severity (% LAI Lost)") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) + 
  scale_fill_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  facet_wrap(~Year)

boxplots
