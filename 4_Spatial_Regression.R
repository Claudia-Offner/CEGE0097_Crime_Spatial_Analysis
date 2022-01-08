# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL REGRESSION
# Module: CEGE0097
# Student no.: 20198829 [CLAUDIA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')

# CLAUDIA COMENTS:
# What is fairSS for PP_borough race datasets?
# (NEED i.e.: ss_ward2_black, ss_ward2_asian, ss_ward2_white, ss_ward2_male, ss_ward2_female)
# (NEED i.e.:  crime_ward_antiBeh; crime_ward_offense; crime_ward_allTheft)

#### 4.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c('crime_ward', 
                           'ss_ward2',  
                           'pp_borough', 'pp_black_borough', 'pp_asian_borough', 'pp_white_borough', 
                           'borough', 'ward', 'proj')])

# (NEED i.e.: ss_ward2_black, ss_ward2_asian, ss_ward2_white, ss_ward2_male, ss_ward2_female)
# (NEED i.e.:  crime_ward_antiBeh; crime_ward_offense; crime_ward_allTheft)

# Make PP lists Spatial objects
pp_borough <- pp_borough[[1]]
pp_black_borough <- pp_black_borough[[1]]
pp_asian_borough <- pp_asian_borough[[1]]
pp_white_borough <- pp_white_borough[[1]]
pp_borough <- merge(borough, pp_borough[ , c(1,6:ncol(pp_borough))], by='DISTRICT')
pp_black_borough <- merge(borough, pp_black_borough[ , c(1,6:ncol(pp_black_borough))], by='DISTRICT')
pp_asian_borough <- merge(borough, pp_asian_borough[ , c(1,6:ncol(pp_asian_borough))], by='DISTRICT')
pp_white_borough <- merge(borough, pp_white_borough[ , c(1,6:ncol(pp_white_borough))], by='DISTRICT')



#### 4.2 MERGE DATASETS ####

# POLICE PERCEPTION: Merge variables of Interest (4) 
### goodjob, fair, listens, mean by ALL & race (black, asian, white)
### What is fairSS for PP_borough race datasets?

pp_df <- merge(pp_borough, pp_black_borough[ , c(1,6:ncol(pp_black_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_asian_borough[ , c(1,6:ncol(pp_asian_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_white_borough[ , c(1,6:ncol(pp_white_borough))], by='DISTRICT')


# STOP & SEARCH: Merge variables of Interest (6)
### ss_occurance by ALL, race (black, asian, white) and gender (male, female)

ss_df <- ss_ward2

# CRIME: Variables of Interest (4)
### crime_occurance by ALL, type (anti-social behaviour, violence & sexual offense, all theft)

crime_df <- crime_ward

# COMBINE ALL DATASETS: For regression analysis
reg_df <- merge(ss_df, crime_df, by='NAME')
reg_df <- merge(reg_df, pp_df[ , c(1,6:ncol(pp_df))], by='DISTRICT')




