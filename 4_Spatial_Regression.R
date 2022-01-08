# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL REGRESSION
# Module: CEGE0097
# Student no.: 20198829 [CLAUDIA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
# rm(list=ls()[! ls() %in% c("borough", "ward", "proj")])


#### Combine Data files ####

### combine pp from all ethnicities 
pp_per_borough <- merge(borough, pp_ag, by='DISTRICT')
pp_per_borough <- merge(pp_per_borough, pp_white_ag, by='DISTRICT')
pp_per_borough <- merge(pp_per_borough, pp_asian_ag, by='DISTRICT')
pp_per_borough <- merge(pp_per_borough, pp_black_ag, by='DISTRICT')
pp_per_borough <- pp_per_borough[order(pp_per_borough$DISTRICT),]
view(pp_per_borough)

### combine data (TT: not sure what this df is for)
df <- merge(crime_ward, ss_ward, by='NAME')
df <- merge(df, pp_mean, by='DISTRICT')
df <- merge(df, pp_white_mean, by='DISTRICT')
df <- merge(df, pp_asian_mean, by='DISTRICT')
df <- merge(df, pp_black_mean, by='DISTRICT')

### Add pp_mean to ss dataset
crime <- merge(crime, pp_mean, by='DISTRICT')
crime <- merge(crime, pp_white_mean, by='DISTRICT')
crime <- merge(crime, pp_asian_mean, by='DISTRICT')
crime <- merge(crime, pp_black_mean, by='DISTRICT')
view(crime)

### Add pp_mean to ss dataset
ss <- merge(ss, pp_mean, by='DISTRICT')
ss <- merge(ss, pp_white_mean, by='DISTRICT')
ss <- merge(ss, pp_asian_mean, by='DISTRICT')
ss <- merge(ss, pp_black_mean, by='DISTRICT')
view(ss)

### drop ss/crime points where no pp data
sapply(crime@data, function(x) sum(is.na(x)))
sapply(ss@data, function(x) sum(is.na(x)))
sapply(pp_per_borough@data, function(x) sum(is.na(x)))
crime@data <- na.omit(crime@data)
ss@data <- na.omit(ss@data)
pp_per_borough@data <- na.omit(pp_per_borough@data)
