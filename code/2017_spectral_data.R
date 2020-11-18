#### examining 2017 spectral data
### subcanopy leaves at UMBS

## load dependencies
library(dplyr)
library(ggplot2)

## bring in data
data <- read.csv("Leaf_n_correlation_data_summer_2017.csv")
head(data)

## group by species, light conditions
data <- data %>%
  group_by(species_code, leaf_type)

## graph overall trends
plot1 <- data %>%
  ggplot(aes(x = n_percent, y = reNDVI)) +
  geom_point()

overall_model <- lm(reNDVI ~ n_percent, data=data)
summary(overall_model)

##acru
acru <- data %>%
  filter(species_code == 'acru') 

plot2 <- acru
  ggplot(aes(x = n_percent, y = reNDVI)) +
  geom_point()

acru_model <- lm(reNDVI ~ n_percent, data=acru)
summary(acru_model)

##quru
quru <- data %>%
  filter(species_code == 'quru') 

plot3 <- quru %>%
ggplot(aes(x = n_percent, y = reNDVI)) +
  geom_point()

quru_model <- lm(reNDVI ~ n_percent, data=quru)
summary(quru_model)

##pogr
pogr <- data %>%
  filter(species_code == 'pogr') 

plot4 <- pogr %>%
  ggplot(aes(x = n_percent, y = reNDVI)) +
  geom_point()

pogr_model <- lm(reNDVI ~ n_percent, data=pogr)
summary(pogr_model)

##fagr
fagr <- data %>%
  filter(species_code == 'fagr') 

plot5 <- fagr %>%
  ggplot(aes(x = n_percent, y = reNDVI)) +
  geom_point()

fagr_model <- lm(reNDVI ~ n_percent, data=fagr)
summary(fagr_model)
