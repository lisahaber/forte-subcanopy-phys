# This script take the data from the double or dual bands that were put up in June 
# 2019 and compares the growth increments between top and bottom bands across species.
# It runs t-tests to compare top and bottom bands within species, and the compares 
# the differences in the taper between live and dead tress of each species

# load packages 
library(dplyr)
library(tidyr)

#importing csv
DBLband <- read.csv("data/DBL_dendrobands.csv")

# select wanted columns 
DBLband_data <- select(DBLband, tag, fate, species, 
                       DBH_cm, bottom..in., top, date)

# filter out NA's (missed reads), and calculate growth inrements for top and bottom
# bands. Then mutate for difference between top and bottom band increments for 
# each tree
DBLband_incs <- DBLband_data %>%
  group_by(tag) %>%
  filter(!is.na(top)) %>% 
  filter(!is.na(bottom..in.)) %>% 
  mutate(bot_increment= bottom..in.-lag(bottom..in., 
                                    default = first(bottom..in.))) %>%
  mutate(top_increment= top-lag(top, default = first(top))) %>% 
  mutate(t_b_diff = bot_increment - top_increment)
  
  
#all increments < 0 = 0; typos, typos and misreads lead to some negative increments 
DBLband_incs$top_increment[DBLband_incs$top_increment < 0] <- 0
DBLband_incs$bot_increment[DBLband_incs$bot_increment < 0] <- 0
# need two-sample t-test for each species on each week 

# but first I need to convert date from factor to date and create a week column 
DBLband_incs$date <- as.Date(DBLband_incs$date, "%Y-%m-%d")
DBLband_incs$week <- as.Date(cut(DBLband_incs$date, breaks = "week", start.on.monday = FALSE))

# reformat data to fit a t-test
DBLband_transform <- DBLband_incs %>% 
  gather(bot_or_top, increment, bot_increment, top_increment) %>% 
  ungroup()

#make a t test funcion to compare top and bottom differences. The function filters 
#out the first two weeks of double band measurements to give a couple weeks for 
#bands to settle 

t_test_diff <- function(SPECIES){
  filtered <- filter(DBLband_incs, species == SPECIES & 
                       week != "2019-07-07" & week != "2019-07-14")
  output <- t.test(t_b_diff ~ fate, data = filtered, paired = FALSE, var.equal = TRUE)
  return(output)
}

# t-tests asking if the difference in top and bottom bands is different in the live 
# and dead trees 
t_test_diff("ACRU")
t_test_diff("ACSA")
t_test_diff("PIST")
t_test_diff("POGR")
t_test_diff("QURU")
 