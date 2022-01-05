# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL EDA: CRIME
# Module: CEGE0097
# Student no.: [PRATIBHA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c("crime", "borough", "ward", "proj")])