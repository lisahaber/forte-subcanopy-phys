####################################
## Lisa T. Haber                  ##
## 2018.10.25                     ##
## FoRTE Subcanopy Physiology     ##
####################################

# This code is my first attempt to extract and utilize Amax files from the shared FoRTE data drive.
# I am building this off Jeff Atkins' code from the FoRTE Canopy repo.

# load required packages
library(plyr)
library(dplyr)
library(readr)
require(tidyverse)
require(googledrive)
require(ggplot2)

# check drives
drive_find(pattern = "subcanopy_leaf_physiology", n_max = 50)

#Direct Google Drive link to "FoRTE/data"
x <- as_id("https://drive.google.com/drive/folders/1YULT4fx50b1MXZNOywgEeosW0GkrUe9c?usp=sharing")

# Uses x to "get" drive
drive_get(as_id(x))

# lists what is in drive
drive_ls(x)

# id of drive with subcanopy phys data
subcanopy.files <- as_id("https://drive.google.com/open?id=1Q2k5eSuk0Gr6d-lPECksonbP6FbNwmjz")
drive_ls(subcanopy.files)
files <- drive_ls(subcanopy.files)

# create a data directory
# dir.create("data/subcanopy_phys", showWarnings = FALSE)
# 
# for(f in files$name) {
#   cat("Downloading", f, "...\n")
#   drive_download(f, overwrite = TRUE)
#   print(f)
# }
# 
# 



##################################################################
## working with LICOR 6400 XT .txt files                        ##
## used advice from:                                            ##
## http://www.ericrscott.com/2018/01/17/li-cor-wrangling/       ##
##################################################################
text.raw <- read_file("licor.txt")
header_pattern <- "\"OPEN \\d\\.\\d\\.\\d"
data_pattern <- "\\$STARTOFDATA\\$"

#splits into individual bouts
raw_split <- str_split(text.raw, header_pattern, simplify = TRUE)

#splits further to separate headers from actual data
raw_split2 <- str_split(raw_split, data_pattern, simplify = FALSE)

str(raw_split2)

#extract just the second element, the actual data
raw_split3 <- raw_split2 %>%
  map(`[`, 2) %>% #equivalent to doing raw_split2[[i]][2] for every element "i"
  flatten_chr() #converts to a vector

#remove empty elements
raw_split3 <- raw_split3[!is.na(raw_split3)]

# use map() from package "purrr" to apply read_tsv() to every string in raw text vector
# "skip = 1" gets rid of the unnecessary line "ins" and "outs"
input <- raw_split3 %>%
  map(read_tsv, skip = 1)

input.all <- bind_rows(input)
head(input.all, 10)

#create a "safe" version of as.integer() that returns a list of a result and error
safe_as.int <- safely(as.integer)

#returns error for text remarks, returns value for integer observation numbers

input.all <- input.all %>% 
  mutate(#create a comment column to indicate if an "Obs" is actually a remark
    comment = is.na(safe_as.int(Obs)$result), 
    #copy those remarks to the remark column
    remark = ifelse(comment == TRUE, Obs, NA),
    #remove remarks from Obs column
    Obs = ifelse(comment == FALSE, Obs, NA)) %>% 
  #move the remark column the the begining
  select(remark, everything()) %>% 
  #remove the comment column.  We're done with it
  select(-comment)
head(input.all, 10)

#you must replace NA with the literal string "NA" so str_* functions from stringr can deal with it
input.all <- input.all %>% mutate(remark = str_replace_na(remark))

IDpattern <- "[:lower:][:blank:]\\d+[:blank:][:lower:]"
str_view(input.all$remark[1:10], IDpattern)

#Now that I’ve figure out a pattern that matches the ID’s I can use str_extract() to move them to a new sampleID column.
input.all <- input.all %>%
  mutate(sampleID = str_extract(remark, IDpattern)) %>% 
  select(sampleID, everything())
head(input.all, 10)

#get rid of other remarks and fill down the sample ID column
output <- input.all %>% 
  filter(!xor(remark == "NA" , is.na(sampleID))) %>%
  fill(sampleID) %>% 
  #get rid of the rest of the remark rows
  filter(complete.cases(.)) %>% 
  #get rid of the remark column
  select(-remark)
head(output, 10)

#And finally, we have a cleaned data frame ready for use in analyses! You could go on to separate plot ID, plant ID and leaf ID using separate() from tidyr, and then do any necessary calculations, visualizations, and modeling with the resulting data frame.