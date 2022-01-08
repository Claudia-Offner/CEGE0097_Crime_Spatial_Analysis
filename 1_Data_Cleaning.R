# Project: CRIME SPATIAL ANALYSIS
# File: DATA CLEANING
# Module: CEGE0097
# Student no.: 20198829, 20164326 ...
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided data.
setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')

# Load Packages
library(tmap)
library(ggplot2)
library(sp)
library(rgdal)
library(tidyverse)
library(leaflet)
library(spatialEco)
library(dplyr)

# NOTE: Make sure all of your necessary pre-processing is done here (not in 2_Non_Spatial_EDA)

#### 1.1 LOAD DATA ####
# Open csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv', fileEncoding="UTF-8-BOM")
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv', fileEncoding="UTF-8-BOM")
pp <- read.csv(file='Data/2016-police-perceptions.csv', fileEncoding="UTF-8-BOM")
pp_wab <- read.csv(file='Data/2015-16 _to_2020-21_inclusive_neighbourhood_indicators_final_221221.csv') # wab = white, asian, black

# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" #set projections
ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp")
borough <- spTransform(borough, CRS(proj))
names(borough@data)[1] <- "DISTRICT" # change name of spatial delineation (col1) to match ward

#### 1.2 GENERAL DATA CLEANING & AGGREGATION ####

### Check for missing values
sapply(crime, function(x) sum(is.na(x)))
sapply(ss, function(x) sum(is.na(x)))
sapply(pp, function(x) sum(is.na(x)))
sapply(pp_wab, function(x) sum(is.na(x)))
sapply(borough@data, function(x) sum(is.na(x)))
sapply(ward@data, function(x) sum(is.na(x)))

### create subsets and drop missing/irrelevant fields
crime <- subset (crime, select = -c(Crime.ID, Context, Reported.by, Falls.within, Location, LSOA.code, LSOA.name, Last.outcome.category))
ss <- subset(ss, select = -c(Part.of.a.policing.operation, Type, Legislation, Policing.operation, Removal.of.more.than.just.outer.clothing, Outcome.linked.to.object.of.search))
borough@data <- subset(borough@data, select = -c(SUB_2006, SUB_2009))
pp <- subset(pp, select = -c(2:3,5:6,9:22)) # selects only good-job, fair and listens indicators
pp <- pp[ , c(1, 3, 4, 2)] # reorder to neighbour, fair, listens, good-job

### Drop Values with NA (831 in lat/long)
crime <- na.omit(crime)
ss <- na.omit(ss)
pp <- na.omit(pp)
pp_wab <- na.omit(pp_wab)

### Add unique id's
ss$ID <- seq_along(ss[,1])
crime$ID <- seq_along(crime[,1])




################### TOMMY: Add your PRE-PROCESSING bits below ###################
# (CLAUDIA) I have added what looks like pre-processing here so I can start my script, but 
# double check that your section contain all of pre-processing required for 
# visuals/non-spatialEDA/ESDA. Add more drop down sections for specific 
# variables (like in Danni's section) if need be.

#### 1.2a Police Perceptions #### 

### shorten indicator names
names(pp) <- c('neighbourhood','ALL_goodjob', 'ALL_fair', 'ALL_listens')
names(pp_wab) <- c('neighbourhood', 'WHITE_fair', 'WHITE_listens', 'WHITE_goodjob', 'WHITE_fairSS', 'ASIAN_fair', 'ASIAN_listens', 'ASIAN_goodjob', 'ASIAN_fairSS', 'BLACK_fair', 'BLACK_listens', 'BLACK_goodjob', 'BLACK_fairSS')

### remove subheading row(s)
pp <- pp[-1,]# remove row2 containing "% Strongly Agree or Tend to agree"
pp_wab <- pp_wab[-c(1, 2), ]

### Change data types where applicable
names <- colnames(pp)[-1]
pp[ ,names] <- apply(pp[ , names], 2, function(x) as.numeric(as.character(x)))# Change characters to numeric
names <- colnames(pp_wab)[-1]
pp_wab[ ,names] <- apply(pp_wab[ , names], 2, function(x) as.numeric(as.character(x)))

### foo: select borough name within neighbourhood name
replace_neighbourhood_names <- function(pp) {
  names <- pp$neighbourhood
  names <- sub(" -.*", "", names) # remove text after borough name
  pp$neighbourhood <- c(names)
  names(pp)[1] <- "DISTRICT"      # rename neighbourhoods as DISTRICTS
  return(pp)
}

pp <- replace_neighbourhood_names(pp)         # call1
pp_wab <- replace_neighbourhood_names(pp_wab) # call2

### omit districts where a high proportion of its neighbourhoods are NA (TBC)
# count number of districts - as.data.frame(table(pp_wab$DISTRICT)) 
# count number of NAs per district
# merge 'district count' with 'NA count' by district name.
# if more than 25% are NA, FALSE.
# export list of districts to remove from master datasets

### in the meantime, just delete all rows that are NA. BUT, risk of few neighbourhoods representing a whole borough.
pp_wab <- pp_wab[!is.na(pp_wab[2]), ]                 
list <- setdiff(pp[1], pp_wab[1])           # identify neighbourhoods removed from pp_wab
pp <- pp[!pp$DISTRICT %in% list$DISTRICT, ] # remove rows from pp that were deleted in pp_wab

### prepare pre-aggregated datasets for use in exploring variation (eda)
pp$ALL_mean <- round(rowMeans(pp[-1]),1)  # add mean to row for use in eda exploring variation
pp_white <- pp_wab[, c(1:5)]
pp_white$WHITE_mean <- round(rowMeans(pp_white[-1]),1)
pp_asian <- pp_wab[, c(1, 6:9)]
pp_asian$ASIAN_mean <- round(rowMeans(pp_asian[-1]),1)
pp_black <- pp_wab[, c(1,10:13)]
pp_black$BLACK_mean <- round(rowMeans(pp_black[-1]),1)

### foo: aggregate all data to a single row for each borough
aggregate_to_borough <- function(pp, borough, colname) {
  names <- colnames(pp)[-1]
  pp_ag <- pp %>%
    group_by(DISTRICT) %>% 
    summarise_at(vars(names), mean)
  pp_ag$mean_pp <- round(rowMeans(pp_ag[-1]),1)                    # create 'mean_pp' col to pp_ag table, populated with mean of row.
  pp_ag <- rapply(pp_ag, f = round, classes = "numeric", how = "replace", digits = 1) # round all vals to 1 d.p.
  pp_borough <- merge(borough, pp_ag, by='DISTRICT')      # merge pp_ag table to the borough shapefile
  pp_mean <- subset(pp_ag, select = c(DISTRICT, mean_pp)) # create subset table containing only districts and mean_pp
  # change colname based on input ethnicity
  pp_ag <- data.frame(pp_ag)
  pp_mean <- data.frame(pp_mean)
  pp_borough <- data.frame(pp_borough)
  colnames(pp_ag)[colnames(pp_ag) == 'mean_pp'] <- colname
  colnames(pp_mean)[colnames(pp_mean) == 'mean_pp'] <- colname
  colnames(pp_borough)[colnames(pp_borough) == 'mean_pp'] <- colname
  out <- list(pp_ag, pp_mean, pp_borough)                 # aggregate results into list, as only one returnable var
  return(out)
}

### call1: get mean of pp indicators for 'all' respondents at borough level
pp_lst <- aggregate_to_borough(pp, borough,'PP_ALL_MEAN')
pp_ag <- pp_lst[1]        # district, 3 indicator scores, mean score
pp_mean <- pp_lst[2]      # district and mean score
pp_borough <- pp_lst[3]   # district, 3 indicator scores, mean score WITH borough boundary

### call2: get mean of pp indicators for 'white' respondents at borough level
pp_white_lst <- aggregate_to_borough(pp_white, borough, 'PP_WHITE_MEAN')
pp_white_ag <- pp_white_lst[1]
pp_white_mean <- pp_white_lst[2]
pp_white_borough <- pp_white_lst[3]

### call3: get mean of pp indicators for 'asian' respondents at borough level
pp_asian_lst <- aggregate_to_borough(pp_asian, borough,'PP_ASIAN_MEAN')
pp_asian_ag <- pp_asian_lst[1]
pp_asian_mean <- pp_asian_lst[2]
pp_asian_borough <- pp_asian_lst[3]

### call4: get mean of pp indicators for 'black' respondents at borough level
pp_black_lst <- aggregate_to_borough(pp_black, borough, 'PP_BLACK_MEAN')
pp_black_ag <- pp_black_lst[1]
pp_black_mean <- pp_black_lst[2]
pp_black_borough <- pp_black_lst[3]



################### DANNI: Add your PRE-PROCESSING bits below ###################
# (CLAUDIA) I have added what looked like pre-processing for the overall SS dataset and 
# specific variables here so I can start my script. I commented out any visuals (barplots & ggplots) since 
# those will need to be put in script 2. Double check that your sections contain 
# all of pre-processing required for visuals/non-spatialEDA/ESDA scripts.

### (CLAUDIA) COMMENT: You created a MEAN_PP variable - Is it needed for your final analyses? 
# If so, either add to both your scrip 2 section & ESDA script, 
# or check with tommy's section cause it may be useful for all of our scripts 

#### 1.2b Stop and Search #### 

# Create Point data from coordinate datasets
xy <- ss[,c('Longitude', 'Latitude')] 
ss <- SpatialPointsDataFrame(coords= xy, data = ss, proj4string = CRS(proj))


# Join Polygon & Point information
ss <- point.in.poly(ss, ward)
# Remove point outside of polygon (NaN GSS_Code)
sapply(ss@data, function(x) sum(is.na(x)))
ss@data <- na.omit(ss@data)

#_______________________________________________________________________________
# Aggregate point data by ward NAME (COUNT points): Specific demographics will 
# need to be counted and examined separately, and this will be determined by 
# non spatial EDA
#_______________________________________________________________________________\

# install point.in.polygon -> package associated 
ss_ag <- aggregate(ss@data$NAME, list(ss@data$NAME), length)
names(ss_ag) <- c('NAME', 'ss_occurance')

# Merging and ss_ward2 

# Create polygon data from point count
# ***** DONT TOUCH THIS: 
ss_ward <- merge(ward, ss_ag, by='NAME')  # ss_ag -> 595 rows // ward has 657 rows 

# ***** 

# original data:
nrow(ss_ag) # 595 (with no duplicates)

# merge to wards shp
ss_ward2 <- merge(x = ward, y = ss_ag, all.x = FALSE)

nrow(ss_ward2) # 614
length(which(table(ss_ward2$NAME)>1)) # 19 rows are duplicated 
# 614 rows subtract the 19 duplicates = 595 

(which(table(ss_ward2$NAME)>1))

ss_ward2@data

# Abbey // barnhill -> examples of districts whose names are repeated

ordered_ss_ward2 <- ss_ward2[order(ss_ward2$NAME),]
sub <- subset(ss_ward2@data, ss_ward2@data$NAME == "Abbey") #ignore


ss_ward2_data <- ss_ward2@data
count(is.na(ss_ward2@data$ss_occurance))

# SS Gender -------------------------------------------------------------


# SS: Gender (>80% men) - COMBINE EMPTY VALUES WITH 'OTHER' (or not?) !!

table(ss@data$Gender) # -> gives count per gender 
#       Female   Male  Other 
#  100    568   8383      3 

table2 <- table(ss@data$Gender)
prop.table(table2) # gives proportions per gender 
round(prop.table(table2) ,3)  # proportions rounded to 3 dp 


# # frequency table per gender including empties 
# x <- prop.table(table(ss@data$Gender))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# # plot mean_pp per gender (F / M / O) -> excluding empties
# ggplot(data=ss@data[ss@data$Gender!="",], aes(x=Gender, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# make new dataframe getting rid of gender empties
updated_genders <- data.frame(ss@data[ss@data$Gender != "",])
typeof(updated_genders)


# # plot updated df 
# ggplot(data=updated_genders, aes(x=Gender, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))



# SS Age -------------------------------------------------------------


# **** SS AGE DATA *** 
# SS: Age (>35% are 18-24) (<25% are 25-34) - LABEL EMPTY VALUES WITH 'OTHER' (done) !!

table(ss@data$Age.range) # original data with empty category
#        10-17    18-24    25-34  over 34 under 10 
# 398     1764     3389     2030     1472        1 

# attempt 1 
levels(ss@data$Age.range)[levels(ss@data$Age.range)==""] <-"OTHER"
table(ss@data$Age.range)


# x <- prop.table(table(ss@data$Age.range))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

# ggplot(data=ss@data, aes(x=Age.range, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




# SS Self-defined Ethnicity ------------------------------------------------------------


# *** SS ETHNICITY DATA ***
# SS: Self.defined.ethnicity - NEEDS CATEGORICAL AGGREGATION (done)!!

# ethnicity data aggregated into "", white, black, mixed, chinese or other, asian, not stated
ss@data$Self.defined.ethnicity<- fct_collapse(ss@data$Self.defined.ethnicity, 
                                              "White" = grep("White - ", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Black or Black British" = grep("Black or Black British -", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Mixed" = grep("Mixed -", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Chinese or other ethnic group" = grep("Chinese or ", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Asian or Asian British" = grep("Asian or Asian British -", ss@data$Self.defined.ethnicity, value = TRUE))




table4 <- table(ss@data$Self.defined.ethnicity) # We still have the empty category 
round(prop.table(table4) , 3) 

#                Asian or Asian British        Black or Black British Chinese or other ethnic group 
#0.010                         0.117                         0.332                         0.021 

#Mixed               Not Stated (NS)                         White 
#0.047                         0.095                         0.379 



# Change empty ethnicity fields to "Not Stated (NS)"
levels(ss@data$Self.defined.ethnicity)[levels(ss@data$Self.defined.ethnicity)==""] <-"Not Stated (NS)" 



table5 <- table(ss@data$Self.defined.ethnicity) # Updated table -> merges empty field with NS 
round(prop.table(table5) , 3) 

# Not Stated (NS)        Asian or Asian British        Black or Black British Chinese or other ethnic group 
# 0.105                         0.117                         0.332                         0.021 

#Mixed                         White 
#0.047                         0.379 


# x <- prop.table(table(ss@data$Self.defined.ethnicity))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# ggplot(data=ss@data, aes(x=Self.defined.ethnicity, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))



# SS Officer Defined Ethnicity  ----------------------------------------------

# *** The officer-defined ethnicity of the person stopped ***

# SS: officer.defined.ethnicity (>40% white) (40% black) - COMBINE EMPTY VALUES WITH 'OTHER' !!

ss@data$Officer.defined.ethnicity

# x <- prop.table(table(ss@data$Officer.defined.ethnicity))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# REPLACE EMPTY WITH "NOT STATED"

levels(ss@data$Officer.defined.ethnicity)[levels(ss@data$Officer.defined.ethnicity)=="CHECK"] <-"NOT STATED" 
# THIS ONE WORKS^

table(ss@data$Officer.defined.ethnicity)


# ggplot(data=ss@data, aes(x=Officer.defined.ethnicity, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




# SS Object of Search --------------------------------------------------------

# SS: object.of.search (>60% Controlled drugs) (~35% articles for use in criminal damage)
# x <- prop.table(table(ss@data$Object.of.search))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

# ggplot(data=ss@data, aes(x=Object.of.search, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))

# SS: outcome (>60% nothing found - no further action) (~20% suspect arrested)  - NEEDS CATEGORICAL AGGREGATION!!

table(ss@data$Outcome)

# Local resolution          Nothing found - no further action 
# 94                                    6111 
# Offender cautioned        Offender given drugs possession warning 
# 14                                     740 
#Offender given penalty notice        Suspect arrested 
# 153                                    1873 
# Suspect summonsed to court 
# 69 

# x <- prop.table(table(ss@data$Outcome))
# par(fig=c(0,1,0.3,1), new=FALSE)
# barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

# ggplot(data=ss@data, aes(x=Outcome, y=mean_pp)) + 
#   geom_boxplot()+
#   coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




################### PRATIBHA: Add your PRE-PROCESSING bits below ###################

# (CLAUDIA) I have added some pre-processing here so I can start my script.
# Add your pre-processing required for visuals/non-spatialEDA/ESDA here when you 
# can and make sure the visuals/non-spatialEDA is in script 2.

### (CLAUDIA) COMMENT: when you install new pacakges, make sure you leave the 
# install.package() code in for us, or have a section that lets us identify what 
# we need to install easily

#### 1.2c Crime #### 

#_______________________________________________________________________________
# NOTE: Re-code all relevant categorical variables prior to aggregation
#_______________________________________________________________________________

# Create Point data from coordinate datasets
xy <- crime[,c('Longitude', 'Latitude')]
crime <- SpatialPointsDataFrame(coords= xy, data = crime, proj4string = CRS(proj))

# Join Polygon & Point information
crime <- point.in.poly(crime, ward)
# Remove point outside of polygon (NaN GSS_Code)
sapply(crime@data, function(x) sum(is.na(x)))
crime@data <- na.omit(crime@data)

#_______________________________________________________________________________
# Aggregate point data by ward NAME (COUNT points): Specific crime type will 
# need to be counted and examined separately, and this will be determined by 
# non spatial EDA
#_______________________________________________________________________________
crime_ag <- aggregate(crime@data$NAME, list(crime@data$NAME), length)
names(crime_ag) <- c('NAME', 'crime_occurance')

# Create polygon data from point count
crime_ward <- merge(ward, crime_ag, by='NAME') 
