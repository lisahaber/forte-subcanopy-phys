#############################################
## Subcanopy Asat vs. NPP
## Fall 2020
#############################################

# load dependencies
library(tidyverse)

# load data
npp <- read.csv("C:/github/forte-subcanopy-phys/data/NPP_subcan.csv")
head(npp)

asat <- read.csv("Mean_Asat_values_subplots_2019.csv")
head(asat)

# combine data tables for analysis
asat <- as_tibble(asat)
npp <- as_tibble(npp)
npp <- npp %>%
  rename(Subplot = subplot)

combined <- left_join(asat, npp)

head(combined)

combined$Replicate <- 
  ifelse(grepl("A...", combined$Subplot),
         "A",
         ifelse(grepl("B...", combined$Subplot),
                "B",
                ifelse(grepl("C...", combined$Subplot),
                       "C", 
                       ifelse(grepl("D...", combined$Subplot),
                              "D", "Other"
                       ))))

##Another way to do this, from Max:
# npp <- npp %>%
#   mutate(Replicate = substr(subplot, 1, 1)) %>%

## add column for CV
combined <- combined %>%
  mutate(CV = sd_Asat_2019 / mean_Asat_2019)

# plot for examination
g1 <- combined %>%
  ggplot(aes(x = mean_Asat_2019, y = NPP_subcan)) +
  geom_point(size=3.5) +
  xlab("Subcanopy mean Asat") +
  ylab("ANPP Subcanopy") +
  geom_smooth(method=lm)

fit1 <- lm(NPP_subcan ~ mean_Asat_2019, data = combined)
summary(fit1)

g2 <- combined %>%
  ggplot(aes(x = mean_Asat_2019, y = NPP_subcan, color=Replicate )) +
  geom_point(size=3.5) +
  xlab("Subcanopy mean Asat") +
  ylab("ANPP Subcanopy") +
  geom_smooth(method=lm, aes(group=1)) +
  theme_classic(base_size = 18)

g3 <- combined %>%
  ggplot(aes(x = CV, y = NPP_subcan, color=Replicate )) +
  geom_point(size=3.5) +
  xlab("Subcanopy CV Asat") +
  ylab("ANPP Subcanopy")+
  theme_classic(base_size = 18)

fit2 <- lm(NPP_subcan ~ CV, data = combined)
summary(fit2)

devtools::install_github("atkinsjeff/fortedata")
require(fortedata)
