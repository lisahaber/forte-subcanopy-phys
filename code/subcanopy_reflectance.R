####################################
## Lisa T. Haber                  ##
## 2018.11.18                     ##
## FoRTE Subcanopy Physiology     ##
## Leaf reflectance data          ##
####################################

# attempt to adapt Ben Bond-Lamberty's code for pulling the leaf reflectance data

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_reflectance"
# as_id("https://drive.google.com/drive/folders/1Q2k5eSuk0Gr6d-lPECksonbP6FbNwmjz") %>% 
  # drive_ls ->
  # gdfiles

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
files <- list.files(data_dir, pattern = "[0-9]{18}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line