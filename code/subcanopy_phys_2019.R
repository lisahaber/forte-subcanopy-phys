#######################################################################
## FoRTE 2019 Subcanopy Physiology data extraction & compilation
## Fall 2019
## Lisa Haber
## Reusing/modifying code from 2018
#######################################################################

## This year, our LICOR file names for subcanopy individuals are different (changed to save time in the field). Instead of the 2018 format including species code, subplot, and date, we only have the four digit unique subcanopy tree ID in the file name. These IDs are the tag numbers added to trees by field tech team in 2019 (did not exist in 2018) so a lookup table will be needed to match this year's measurements with last year's. 

#1. Extract and utilize subcanopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)

# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology_2019"
# as_id("https://drive.google.com/drive/folders/1zSisjX5fHLc6o8bFkq6tD0HiTykfWQmJ") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data2019/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with four digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{4}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line

  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>%
      gsub('\"', "", .)

    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) ->  # change to a POSIXct object
  # separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) ->
  licordata 

##############################
# get means of 5 observations per leaf to use in analysis

licordata_means_2019 <- licordata %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo))
licordata_means_2019  

# write.table(licordata_means_2019,"Mean_Photo_Cond_2019.txt",sep="\t",row.names=FALSE)  
# write.csv(licordata_means_2019, "Mean_Photo_Cond_2019.csv", row.names = FALSE)

#################################
# summarize data for Chris G., get by-subplot means for Amax pre and post
# first, 2019 (simpler because there were no "redo's" to ignore)
# getwd()
# data2019 <- read.csv("C:/github/forte-subcanopy-phys/data/subcanopy_tree_Asat_forte_2019_ForAnalysis.csv")
# head(data2019)
# 
# subplot_mean_Asat_2019_counts <- data2019 %>%
#   select(Subplot, Species, ID_tag_2019, mean_Asat_2019_area_corrected) %>%
#   na.omit() %>%
#   filter(mean_Asat_2019_area_corrected > 0) %>%
#   group_by(Subplot) %>%
#   count() 

# write.csv(subplot_mean_Asat_2019_counts, "Samples_count_subplots_2019.csv",row.names=FALSE)

# subplot_mean_Asat_2019 <- data2019 %>%
#   select(Subplot, Species, ID_tag_2019, mean_Asat_2019_area_corrected) %>%
#   na.omit() %>%
#   filter(mean_Asat_2019_area_corrected > 0) %>%
#   group_by(Subplot) %>%
#   summarise(mean_Asat_2019 = mean(mean_Asat_2019_area_corrected), sd_Asat_2019 = sd(mean_Asat_2019_area_corrected))

# write.csv(subplot_mean_Asat_2019, "Mean_Asat_values_subplots_2019.csv", row.names = FALSE)


#second, 2018
data2018 <- read.csv("C:/github/forte-subcanopy-phys/data/LMA_NDVI_Asat_forte_leaves_OnlyIncludeRedosForC03D03_2018.csv")
head(data2018)

subplot_mean_Asat_2018_counts <- data2018 %>%
  select(Leaf_unique_ID, Subplot, Species, mean_Asat_2018_area_corrected) %>%
  na.omit() %>%
  filter(mean_Asat_2018_area_corrected > 0) %>%
  group_by(Subplot) %>%
  count()

write.csv(subplot_mean_Asat_2018_counts, "Samples_count_subplots_2018_Corrected.csv", row.names=FALSE)

subplot_mean_Asat_2018_Corrected <- data2018 %>%
select(Leaf_unique_ID, Subplot, Species, mean_Asat_2018_area_corrected) %>%
na.omit() %>%
filter(mean_Asat_2018_area_corrected > 0) %>%
group_by(Subplot) %>%
summarise(mean_Asat_2018 = mean(mean_Asat_2018_area_corrected), sd_Asat_2018 = sd(mean_Asat_2018_area_corrected))

write.csv(subplot_mean_Asat_2018_Corrected, "Mean_Asat_values_subplots_2018_Corrected.csv", row.names = FALSE)


#now, visualize & analyze
boxplots2018 <- data2018 %>%
  select(Leaf_unique_ID, Subplot, Species, mean_Asat_2018_area_corrected) %>%
  na.omit() %>%
  filter(mean_Asat_2018_area_corrected > 0) %>%
  group_by(Subplot) %>%
  ggplot(aes(x = Subplot, y = mean_Asat_2018_area_corrected)) + geom_boxplot()

# boxplots2019 <- data2019 %>%
#   select(Subplot, Species, ID_tag_2019, mean_Asat_2019_area_corrected) %>%
#   na.omit() %>%
#   filter(mean_Asat_2019_area_corrected > 0) %>%
#   group_by(Subplot) %>%
#   ggplot(aes(x = Subplot, y = mean_Asat_2019_area_corrected)) + geom_boxplot()

  
all_data <- read.csv("C:/github/forte-subcanopy-phys/Mean_Asat_values_subplots_2018_2019_Corrected.csv")
head(all_data)

all_data$Severity <- as.factor(all_data$Severity)

forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")

boxplots <- all_data %>%
  na.omit() %>%
  group_by(Severity) %>%
  ggplot(aes(x = Severity, y = mean_Asat, group_by(Severity), fill = Severity)) + 
  theme_classic(base_size = 13) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Severity (% LAI Lost)") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) + 
    facet_grid(~Year) +
  scale_fill_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00"))
  
boxplots

###########################
## species only graphs
data2019 <- read.csv("C:/github/forte-subcanopy-phys/data/subcanopy_tree_Asat_forte_2019_ForAnalysis.csv")

head(data2019)

data2019 <- data2019 %>%
  select(Subplot, Replicate, Severity, Species, ID_tag_2019, mean_Asat_2019_area_corrected) %>% 
  filter(mean_Asat_2019_area_corrected >= 0) %>%
  group_by(Subplot, Species, Severity, Replicate) %>%
  summarize(mean_Asat = mean(mean_Asat_2019_area_corrected))

head(data2019)

data2019$Severity <- as.factor(all_data$Severity)
data2019$Species <-as.factor(data2019$Species)

forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")

boxplots <- data2019 %>%
  na.omit() %>%
  group_by(Severity, Species) %>%
  ggplot(aes(x = Severity, y = mean_Asat, group_by(Species), fill = Species)) + 
  theme_classic(base_size = 13) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Species Code") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) + 
  # facet_grid(~Year) +
  scale_fill_viridis_d()

boxplots




#### Model Run
## using code from Max Grigri; same model run by him and also by Kayla Mathes on their summer 2019 data sets
library(agricolae)
# clean up my all_data df for model run
all_data$Severity <- as.factor(all_data$Severity)
all_data$Treatment <- as.factor(all_data$Treatment)
all_data$Replicate <- as.factor(all_data$Replicate)
all_data$Year <- as.factor(all_data$Year)
# recode 0 to 0.00 to help me when with the post-hoc output
all_data$Severity <- recode_factor(all_data$Severity, "0" = "0.00")
model_run <- all_data %>%
  ungroup() %>%
  select(Replicate, Year, Severity, Treatment, mean_Asat) %>%
  filter(!is.nan(mean_Asat))
# overall anova split-split plot model run
Asatmodel <- with(model_run, ssp.plot(Replicate, Treatment, Year, Severity,
                                     mean_Asat))
# post hoc analsis
# creating an object for each part of the model
gla <- Asatmodel$gl.a
glb <- Asatmodel$gl.b
glc <- Asatmodel$gl.c
# doing the same as above for each error term
Ea <- Asatmodel$Ea
Eb <- Asatmodel$Eb
Ec <- Asatmodel$Ec
# running an LSD post hoc test: Year
LSD_output <- with(model_run, LSD.test(mean_Asat, Year, glb, Eb,
                                      console = TRUE))
# LSD post hoc: Severity-Year
LSD_output <- with(model_run, LSD.test(mean_Asat, Severity:Year, glc, Ec,
                                      console = TRUE))
# # get post-hoc output into a df
# output_df <- data.frame(tibble::rownames_to_column(LSD_output$groups))
# # seperate into severity and date columns
# output_df <- transform(output_df, Severity = substr(rowname, 1, 4),
#                        date = substr(rowname, 6, 15))
# # sort by date and severity so that we can look which severities differ within a week
# output_df <- output_df %>%
#   arrange(date, severity)
# 

#### Model Run for SPECIES
## using code from Max Grigri; same model run by him and also by Kayla Mathes on their summer 2019 data sets
library(agricolae)
# clean up my all_data df for model run
data2019$Severity <- as.factor(data2019$Severity)
data2019$Species <- as.factor(data2019$Species)
data2019$Replicate <- as.factor(data2019$Replicate)
data2019$Severity <- recode_factor(data2019$Severity, "0" = "0.00")
# model_run <- all_data %>%
#   ungroup() %>%
#   select(Year, Severity, Species, mean_Asat) %>%
#   filter(!is.nan(mean_Asat))
# overall anova split-split plot model run
Asatmodel <- aov(mean_Asat ~ Severity*Species*Replicate, data=data2019)
summary(Asatmodel)
# post hoc analsis
# creating an object for each part of the model
# gla <- Asatmodel$gl.a
# glb <- Asatmodel$gl.b
# glc <- Asatmodel$gl.c
# # doing the same as above for each error term
# Ea <- Asatmodel$Ea
# Eb <- Asatmodel$Eb
# Ec <- Asatmodel$Ec
# running an LSD post hoc test: Species
LSD_output <- LSD.test(CanAsatmodel, "Species", console = TRUE)
