# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL EDA: POLICE & PERCEPTIONS
# Module: CEGE0097
# Student no.: 20164326 [TOMMY]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c("pp", "pp_wab", "borough", "ward", "proj")])