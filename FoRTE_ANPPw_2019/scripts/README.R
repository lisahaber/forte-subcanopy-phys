# Maxim S. Grigri
# Virginia Commonwealth University (VCU) 
# National Science Foudnation funded research
# Forest Resilience and Threshold Experiment (FoRTE)
# Associated Manuscript: Grigri et al., (unpublished)

# This set of scripts are associated with diameter increment data of a subsample of 
# trees in FoRTE, The data include stems of all canopy strata, and were collected 
# to quantify aboveground wood net primary production (ANPPw) in the first year 
# after an experimentally administered disturbance. Data were collected from Nov. 
# 2018 to Nov. 2019. 

# SCRIPT OVERVIEW:
# "subcanopy.R" and "seedling_saplings.R" take increment growth data and scale to 
# ANPPw for their respective canopy strata.

# "NPP_2019.R" will first run the "subcanopy.R" and the 
# "seedling_saplings.R" scripts, and then proceed to calculate annual and daily ANPPw
# from raw weekly dendrometer band reads of a sub-sample of canopy trees from 
# Nov. 2018 to Nov. 2019. It then adds in the ANPPw from the other canopy strata. 

# "fate.R" continues from "NPP_2019.R", but uses 
# fate (healthy vs. girdled) as a grouping variable and thus allows for analysis 
# of healthy vs. senescent trees. 

# "cohort.R" continues from "NPP_2019.R", but uses succesional cohort
# (or plant funcitonal type (PFT)) as a grouping variable and thus allows for analysis 
# of early vs. middle/late successional trees.

# "DBL_dendrobands.R" uses data collected from multiple dendrometer bands set at
# multiple heights along a sub-sample of tree stems. This was an effort to detect 
# stem swelling above the injury (see Grigri et al., unpublished). This script 
# analyses differences between upper and lower dendrometer bands in girdled and 
# healthy trees among species 

# "swell_sensitivity.R" calculates annual ANPPw without Acer rubrum conributions 
# because of the detected swelling in "DBL_dendrobands.R" script. It then runs stats
# comparing adjusted (without A. rubrum) and unadjusted (with A. rubrum) annual 
# ANPPw

# "leaf_area_index.R" uses leaf area index (LAI) calculated from hemispherical images
# to analyze end of season (August 2019) LAI. 

# "stats.R" runs statistics for daily and annual ANPPw among disturbance severities,
# types, healthy/girdled trees, canopy strata, succesional cohort (or PFT), leaf area
# index (LAI)

# Here is a list of all the packages needed to run these scripts 
install.packages("dlpyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggpubr")
install.packages("lubridate")
install.packages("agricolae")


