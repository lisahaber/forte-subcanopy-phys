###############################################################################
## Sample LICOR file ingest & cleaning code
## FoRTE
## Lisa T. Haber (built from code produced by Ben Bond-Lamberty & Jeff Atkins)
## May 2021
###############################################################################

## load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

#########################################

## Step 1: Use package "googledrive" to extract raw data files from shared
## FoRTE data drive folder, and aggregate their contents into single dataframe
## for cleaning and analysis.
## URL will depend on your folder, make sure sharing permissions are allowed.

# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology"
as_id("https://drive.google.com/drive/folders/1Q2k5eSuk0Gr6d-lPECksonbP6FbNwmjz") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# Download data
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with eight digits and no file extension

# NOTE: Your filenames will likely be different and this code chunk will need 
# modification to fit your file name structure. Very likely, your LICOR files
# will also have no file extension (e.g. .csv or other), and will also likely
# have data beginning after the $STARTOFDATA$ line in the file.

files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()

# for loop that will read n files in your list and generate a single dataframe
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    # NOTE: This hopefully won't apply to you if you
    # did not make unfortunate comments inside your LICOR files.
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

##########################################################

## Step 2: Data cleaning and preparation for "fd" Level 1 data product export.
# Combine data into a single data frame for analysis
# exclude the unwanted dates: "07052018" & "07102018"
# NOTE: I excluded files collected on these dates (which are part of my 2018 raw
# data backup) because even though those files are valid/useful, I had to resample 
# the subplots where these files came from on a later date that summer. So, they
# are part of the FoRTE raw data backup but not part of the fortedata package
# because they were/are not included in any subsequent analysis or publication.

# Additional notes about the cleaning here:
## To clean the 2018 data set, I did the following:

#1. used only the last five logged values from each Licor file, as a small subset of measurements from 2018 and 2019 included more than the standard 5 logs due to error

#2. excluded data collected on 7/5/2018 and 7/10/2018, as these measurements were from plots that were resampled at a later date

## Note that some tree IDs changed from 2018 to 2019, as many of the small trees sampled in 2018 were not tagged that summer and were subsequently tagged in 2019

## Also note that Licor file naming scheme changed from 2018 to 2019 to streamline, reduce redundancy and error. This necessitates a lookup table to link the observations from 2018 to 2019

filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) %>%
  filter(!Filename_date %in% c("07052018", "07102018"))-> # this part excludes duplicate files from 2018
  licordata

unique(licordata$Filename_date)

## Second code chunk: exclude excessive observations
# figure out which dates/files have more than 5 observations
excessobs <- licordata %>%
  subset(Obs >= 6)

dates <- excessobs$Filename

view(excessobs)
print(unique(dates))

# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand at this point)
# these are observations ("logged" measurements) on the LICOR that I accidentally made before a leaf had fully stabilized
view(licordata)

# now, exclude them. Note that this means the affected files will have observations beginning at "2" or "6", not "1"
drop <- c(191, 567:571, 587:591, 662:666, 1157:1161, 1897:1901)

licordata1 <- licordata[-drop, ] #this subsets the original dataframe to exclude unwanted measurements

view(licordata1)  

nrow(licordata1)  

# now write cleaned, fd-ready data to a .csv file
write.csv(licordata1, "Subcanopy_LI6400_Phys_Data_Clean_2018.csv", row.names = FALSE)

### check to see what overall non-negative sample size is
licordata1_means <- licordata1 %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0)

## we exclude negative photosynthetic values because they are not usable measurements

nrow(licordata1_means)


## NOTE: Since species and sample information were included in my actual file name
## in 2018, those pieces of information are included in the output data frame.
## For 2019 and 2020 data uploads, I will need to use a lookup table to append
## species and sample/tree ID information since I stopped using that information
## in the file name itself (because it introduces error and there's a better way).


