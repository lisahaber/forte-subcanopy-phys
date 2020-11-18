#############################
## LMA in FoRTE 2018, 2019, 2020
## Visualization for Gough Lab
## 08/26/2020
###############################

## load dependencies
library(dplyr)
library(ggplot2)
library(tidyverse)

## bring in data
data2018 <- read.csv("data/subcanopy_tree_Asat_forte_2019_ForAnalysis.csv")
data2019 <- read.csv("data/subcanopy_physiology_sampling_2019_LMA.csv")
data2020 <- read.csv("data/subcanopy_physiology_sampling_2020.csv")

head(data2018)
head(data2019)
head(data2020)

data2018 <- as_tibble(data2018)
data2019 <- as_tibble(data2019)
data2020 <- as_tibble(data2020)

LMA_2018 <- data2018 %>%
  select(Subplot, Severity, Species, ID_tag_2019, LMA_gm.2_2018) %>%
  na.omit() %>% 
  group_by(Subplot, Severity)%>%
  summarize(mean_LMA_subplot_2018 = mean(LMA_gm.2_2018))

LMA_2019 <- data2019 %>%
  select(subplot, tag_number_2019, severity, LMA_gm.2_2019) %>%
  na.omit() %>%
  group_by(subplot, severity) %>%
  summarize(mean_LMA_subplot_2019 = mean(LMA_gm.2_2019))

LMA_2020 <- data2020 %>%
  select(subplot_id, tag_number, severity, LMA_gm.2_2020) %>%
  na.omit() %>%
  group_by(subplot_id, severity) %>%
  summarize(mean_LMA_subplot_2020 = mean(LMA_gm.2_2020))

LMA_all <- cbind(LMA_2018, LMA_2019, LMA_2020)

write.csv(LMA_all, "LMA_for_visualization.csv", row.names = FALSE)

###########
## note that I manually changed the .csv to appear how I wanted it to because I'm in a rush and dumb, also

## boxplots
LMA_data <- read.csv("LMA_for_visualization.csv")

LMA_data$Severity <- as.factor(LMA_data$Severity)

boxplots <- LMA_data %>%
  na.omit() %>%
  group_by(Severity) %>%
  ggplot(aes(x = Severity, y = mean_LMA, group_by(Severity), fill = Severity)) + 
  theme_classic(base_size = 15) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Severity (% LAI Lost)") +
  ylab("Subplot Means for Subcanopy LMA (g/m2)") + 
  scale_fill_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  facet_wrap(~Year)
