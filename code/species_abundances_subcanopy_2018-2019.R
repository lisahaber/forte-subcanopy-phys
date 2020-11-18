###########################################################################
## Calculating relative abundances for species in subcanopy data set      #
## using 2018 data and comparing to Max Grigri's counts from 2019         #
## Fall 2020                                                              #
###########################################################################

# load dependencies
library(tidyverse)

# load data
data2018 <- read.csv("C:/github/forte-subcanopy-phys/data/LMA_NDVI_Asat_forte_leaves_OnlyIncludeRedosForC03D03_2018.csv")
head(data2018)

# maxdata <- read.csv("C:/github/forte-subcanopy-phys/data/subcan_comp.csv")
# head(maxdata)

# add plot column to 2018 data
data2018$Plot <- 
   ifelse(grepl("A...", data2018$Subplot),
         "A",
         ifelse(grepl("B...", data2018$Subplot),
                "B",
                ifelse(grepl("C...", data2018$Subplot),
                       "C", 
                       ifelse(grepl("D...", data2018$Subplot),
                              "D", "Other"
                       ))))
  
abundance_2018 <- data2018 %>%
  group_by(Subplot, Species) %>%
  count(Species) %>%
  group_by(Subplot) %>%
  mutate(total_count = sum(n))

abundance_2018 <- abundance_2018 %>%
  group_by(Subplot) %>%
  mutate(percent = n/total_count*100) %>%
  arrange(desc(percent), .by_group = TRUE)

write.csv(abundance_2018, "abundance_2018_groups.csv")
# 
# data_2018_abund <- read.csv("abundance_2018_groups.csv")
# 
# abund_2018 <- data_2018_abund %>%
#     arrange(Plot, desc(percent_comp))
# 
# abundance_comparison <- read.csv("C:/github/forte-subcanopy-phys/data/abundances_max_lisa_comparison.csv")
# 
# 
# ## graph abundances
# 
# abundance_comparison$Year <- as.factor(abundance_comparison$Year)
# 
# g <- abundance_comparison %>%
#   ggplot(aes(x = Species, y = Percent_comp, color = Year)) +
#   geom_point() +
#   facet_wrap(~Plot)
# 
