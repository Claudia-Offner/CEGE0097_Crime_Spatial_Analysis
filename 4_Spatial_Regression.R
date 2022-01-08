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




#### 4.3 LINEAR REGRESSION ####

# Dependent variable: The variable we are trying to predict.
# Independent variables: The variables we are using to predict the dependent variable. They are called independent variables because they are required to independent of one another. Strong correlation between two or more independent variables is referred to as multicollinearity and can lead to unstable parameter estimates, which limits the explanatory power of the models.
# Parameters: Parameters or coefficients are the weights of the regression model. Each independent variable has a parameter that is multiplied by its value to make a prediction.

# STOP AND SEARCH (outcome/dependent) ~ CRIME (independent)
ssc_lreg <- lm(ss_occurance~crime_occurance, data=reg_df) 
summary(ssc_lreg)
# For every crime occurrence, there is 0.12 more stop and search (sig)
# Model accounts for 50% of data variance

# POLICE PERCEPTIONS (outcome/dependent) ~ CRIME (independent)
ppc_lreg <- lm(PP_ALL_MEAN~crime_occurance, data=reg_df) 
summary(ppc_lreg)
# For every crime occurrence, there is -0.003 lower police perception (insig)
# Model accounts for 0.002 of data variance

# POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent)
ppss_lreg <- lm(PP_ALL_MEAN~ss_occurance, data=reg_df) 
summary(ppss_lreg)
# For every stop & search occurrence, there is -0.002 lower stop and search (very insig)
# Model accounts for -0.002 of data variance

#### 4.4 TESTING ERRORS FOR SPATIAL AUTOCORRELATION ####

# STOP AND SEARCH (outcome/dependent) ~ CRIME (independent)

boston.shp$lm.res <- residuals(bos.lm)
tm_shape(boston.shp)+tm_polygons("lm.res", palette="-RdBu", style="quantile")

