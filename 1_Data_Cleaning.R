# Project: CRIME SPATIAL ANALYSIS
# File: DATA CLEANING
# Module: CEGE0097
# Student no.: 20198829, 20164326, 20174920 
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided data.
#setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')
setwd('C:\\Users\\Tommy\\OneDrive - University College London\\Modules-Notebooks\\CEGE0097_Geocomputation\\Assignment')

# Load Packages
library(tmap)
library(ggplot2)
library(sp)
library(rgdal)
library(tidyverse)
library(leaflet)
library(spatialEco)
library(dplyr)
library(plyr) # This must follow dplyr for 'count' method to work!
library(spdep) # for poly2nb & nb2listw
# NOTE: Make sure all of your necessary pre-processing is done here (not in 2_Non_Spatial_EDA)
tmap_mode("view")

# 1.1 LOAD DATA ####
# Open CRIME csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv', fileEncoding="UTF-8-BOM")
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv', fileEncoding="UTF-8-BOM")
pp <- read.csv(file='Data/2016-police-perceptions.csv', fileEncoding="UTF-8-BOM")
pp_wab <- read.csv(file='Data/2015-16 _to_2020-21_inclusive_neighbourhood_indicators_final_221221.csv') # wab = white, asian, black
ethnicity_by_borough <- read.csv(file='Data/ethnic-groups-by-borough.csv') # https://data.london.gov.uk/dataset/ethnic-groups-borough
ldn_oa <- readOGR(dsn="Data/London_Shapefiles/OA_2011_London_gen_MHW.shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
ldn_b <- readOGR(dsn = "Data/London_Shapefiles/London_Borough_Excluding_MHW.shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
ldneth <- read.csv(file='Data/bulk.csv', stringsAsFactors = F, check.names = F) %>% select(geography, "Ethnic Group: White; measures: Value", "Ethnic Group: Asian/Asian British; measures: Value", "Ethnic Group: Black/African/Caribbean/Black British; measures: Value")

# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" #set projections
# ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
ward <- readOGR(dsn="Data/London_Shapefiles/London-wards-2014/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp")
borough <- spTransform(borough, CRS(proj))
names(borough@data)[1] <- "DISTRICT" # change name of spatial delineation (col1) to match ward

# Open OTHER csv files (controlled variables for regression - both with data from 2013)
stations <- read.csv(file='Data/2013_police_counters.csv', fileEncoding="UTF-8-BOM")
pop <- read.csv(file='Data/2013_ward_atlas_data.csv')

# 1.2 GENERAL DATA CLEANING & AGGREGATION ####

### Check for missing values
sapply(crime, function(x) sum(is.na(x)))
sapply(ss, function(x) sum(is.na(x)))
sapply(pp, function(x) sum(is.na(x)))
sapply(pp_wab, function(x) sum(is.na(x)))
sapply(ethnicity_by_borough, function(x) sum(is.na(x)))
sapply(borough@data, function(x) sum(is.na(x)))
sapply(ward@data, function(x) sum(is.na(x)))
sapply(pop, function(x) sum(is.na(x)))
sapply(stations, function(x) sum(is.na(x)))

### create subsets and drop missing/irrelevant fields
crime <- subset (crime, select = -c(Crime.ID, Context, Reported.by, Falls.within, Location, LSOA.code, LSOA.name, Last.outcome.category))
ss <- subset(ss, select = -c(Part.of.a.policing.operation, Type, Legislation, Policing.operation, Removal.of.more.than.just.outer.clothing, Outcome.linked.to.object.of.search))
borough@data <- subset(borough@data, select = -c(SUB_2006, SUB_2009))
pop <- subset(pop, select = c(New_Code, Borough, Names, Population_and_Age_Mean_age_2013, 
                              Area_and_Density_Population_density_.persons_per_sq_km._2013, 
                              Diversity_Ethnic_Group_5_groups__2011_Census_White, 
                              Diversity_Ethnic_Group_5_groups__2011_Census_Asian_or_Asian_British, 
                              Diversity_Ethnic_Group_5_groups__2011_Census_Black_or_Black_British, 
                              Births_and_deaths_Standardised_Mortality_Ratio_.SMR._2013,
                              Life_Expectancy_Life_expectancy_at_age_65_.all_persons._20092013,
                              House_Prices_Median_House_Price_2013,
                              Household_Income_Mean_Modelled_Household_income_..._2012.13,
                              Crime_Total_crime_rate_2013.14))


### Drop Values with NA (831 in lat/long)
crime <- na.omit(crime)
ss <- na.omit(ss)
pp <- na.omit(pp)
pp_wab <- na.omit(pp_wab)
ethnicity_by_borough <- na.omit(ethnicity_by_borough)

### Add unique id's
ss$ID <- seq_along(ss[,1])
crime$ID <- seq_along(crime[,1])




# 1.2a Police Perceptions #### 

### drop missing/irrelevant fields
pp <- pp[-1,]                 # subheading row
pp_wab <- pp_wab[-c(1, 2), ]  # subheading row

### remove characters from 'grouping'
pp$Group<-gsub("Group","",as.character(pp$Group))

### select subsets
pp <- subset(pp, select = c(1,3,4,7,8)) # selects only good-job, fair and listens indicators
pp <- pp[ , c(1,2,4,5,3)]               # reorder cols to neighbour, fair, listens, good-job

### summarise indicator names (e.g. listens <- "The police in your area listen to the concerns of local people")
names(pp) <- c('neighbourhood', 'grouping', 
               'fair_all', 'listens_all', 'goodjob_all')
names(pp_wab) <- c('neighbourhood', 
                   'fair_w', 'listens_w', 'goodjob_w', 'fairSS_w', 
                   'fair_a', 'listens_a', 'goodjob_a', 'fairSS_a', 
                   'fair_b', 'listens_b', 'goodjob_b', 'fairSS_b')

### amalgamate pp and pp_wab datasets
pp <- merge(pp, pp_wab, by='neighbourhood')
pp <- pp[ , c('neighbourhood','grouping',
              'goodjob_w','goodjob_a','goodjob_b', 'goodjob_all',
              'listens_w', 'listens_a', 'listens_b', 'listens_all',
              'fair_w', 'fair_a', 'fair_b', 'fair_all',
              'fairSS_w', 'fairSS_a', 'fairSS_b')] 

### change fields to numeric where necessary
view(pp)
names <- colnames(pp)[-1]
pp[ ,names] <- apply(pp[ , names], 2, function(x) as.numeric(as.character(x)))  # characters to numeric

### foo: select borough name within neighbourhood name
names <- pp$neighbourhood                         # get neighbourhood names
names <- sub(" -.*", "", names)                   # get district name from neighbourhood name
pp <- add_column(pp, DISTRICT = '', .before = 1)  # create placeholder
pp$DISTRICT <- c(names)                           # populate placeholder

### identify boroughs where 25% of neighbourhoods are NA
na_per_borough <- setNames(aggregate(goodjob_w ~ DISTRICT, data=pp, function(x) {sum(is.na(x))}, na.action = NULL), c("DISTRICT", "na_count"))
nbr_per_borough <- setNames(aggregate(pp$DISTRICT, by=list(pp$DISTRICT), FUN=length), c("DISTRICT", "nbr_count"))
na_nbr <- merge(nbr_per_borough, na_per_borough, by='DISTRICT')
na_nbr$perc_na <- with(na_nbr, na_count / nbr_count * 100)
na_nbr <- na_nbr[!(na_nbr$perc_na<25.1),]
omit_boroughs <- list(na_nbr$DISTRICT)  # res: Bexley, Kingston upon Thames, Merton, Richmond upon Thames

### delete rows that contain NA
pp <- na.omit(pp) 

### get mean of all indicator scores per borough (for exploring variation in eda)
pp$mean_w <- round(rowMeans(pp[, c('goodjob_w', 'listens_w', 'fair_w', 'fairSS_w')]), 1)
pp$mean_a <- round(rowMeans(pp[, c('goodjob_a', 'listens_a', 'fair_a', 'fairSS_a')]), 1)
pp$mean_b <- round(rowMeans(pp[, c('goodjob_b', 'listens_b', 'fair_b', 'fairSS_b')]), 1)
pp$mean_all <- round(rowMeans(pp[, c('goodjob_all', 'listens_all', 'fair_all')]), 1)

### aggregate and merge pp to borough shapefile
names <- colnames(pp)[-c(1,2,3)]
pp_ag_borough <- pp %>%
  group_by(DISTRICT) %>% 
  summarise_at(vars(names), mean)
pp_ag_borough <- rapply(pp_ag_borough, f = round, classes = "numeric", how = "replace", digits = 1) # round all vals to 1 d.p.
pp_ag_borough <- pp_ag_borough[!pp_ag_borough$DISTRICT %in% omit_boroughs[[1]], ]                   # omit boroughs where over 25% it's neighbourhoods were NA

### calculate and append ethnicity population rank per borough
names(ethnicity_by_borough)[names(ethnicity_by_borough) == "ï..DISTRICT"] <- "DISTRICT"         # remove special characters
ethnicity_by_borough <- ethnicity_by_borough[-c(6)]                                             # remove 'other' category
ethnicity_by_borough$White <- as.numeric(gsub(",","",ethnicity_by_borough$White))               # remove commas and convert to numeric field
ethnicity_by_borough$Asian <- as.numeric(gsub(",","",ethnicity_by_borough$Asian))
ethnicity_by_borough$Black <- as.numeric(gsub(",","",ethnicity_by_borough$Black))
ethnicity_by_borough$Total <- as.numeric(gsub(",","",ethnicity_by_borough$Total))
ethnicity_by_borough$perc_w <- with(ethnicity_by_borough, White / Total * 100)                  # calculate percentage of overall population
ethnicity_by_borough$perc_a <- with(ethnicity_by_borough, Asian / Total * 100)
ethnicity_by_borough$perc_b <- with(ethnicity_by_borough, Black / Total * 100)
ethnicity_by_borough$rank_w[order(-ethnicity_by_borough$White)] <- 1:nrow(ethnicity_by_borough) # rank percentage
ethnicity_by_borough$rank_a[order(-ethnicity_by_borough$Asian)] <- 1:nrow(ethnicity_by_borough)
ethnicity_by_borough$rank_b[order(-ethnicity_by_borough$Black)] <- 1:nrow(ethnicity_by_borough)
ethnicity_by_borough <- ethnicity_by_borough[c('DISTRICT', 'perc_w','perc_a', 'perc_b', 'rank_w','rank_a', 'rank_b')]        # retain only rank

# merge datasets
pp_ag_borough <- merge(pp_ag_borough, ethnicity_by_borough, by='DISTRICT') # merge ethnicity rankings to borough-level dataset
pp_ag_borough <- pp_ag_borough[sample(nrow(pp_ag_borough)),]               # shuffle
pp_ag_borough_shp <- merge(borough, pp_ag_borough, by='DISTRICT')          # merge borough-level dataset to borough shapefile                                   
pp_ag_borough <- pp_ag_borough_shp[sample(nrow(pp_ag_borough_shp)),]

# pp datasets to use further along the project pipeline
view(pp_ag_borough)     # for df version
view(pp_ag_borough_shp) # for sdf version


# 1.2b Stop and Search #### 

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
#_______________________________________________________________________________

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

# Rename column
names(ss_ward2@data)[8] <- "ss_occurance_ALL" 

# Abbey // barnhill -> examples of districts whose names are repeated

ordered_ss_ward2 <- ss_ward2[order(ss_ward2$NAME),]
sub <- subset(ss_ward2@data, ss_ward2@data$NAME == "Abbey") #ignore


ss_ward2_data <- ss_ward2@data
count(is.na(ss_ward2@data$ss_occurance))

#### Creating SS_WARD per ethnicity  ------------------------------

# THIS CODE IS COPIED FROM "SS -> EDA : SS (self defined) ETHNICITY DATA EDA"

# ethnicity data aggregated into "", white, black, mixed, chinese or other, asian, not stated
ss@data$Self.defined.ethnicity<- fct_collapse(ss@data$Self.defined.ethnicity, 
                                              "White" = grep("White - ", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Black or Black British" = grep("Black or Black British -", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Mixed" = grep("Mixed -", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Chinese or other ethnic group" = grep("Chinese or ", ss@data$Self.defined.ethnicity, value = TRUE),
                                              "Asian or Asian British" = grep("Asian or Asian British -", ss@data$Self.defined.ethnicity, value = TRUE))

table4 <- table(ss@data$Self.defined.ethnicity) # We still have the empty category 
round(prop.table(table4) , 3) 

# Change empty ethnicity fields to "Not Stated (NS)"
levels(ss@data$Self.defined.ethnicity)[levels(ss@data$Self.defined.ethnicity)==""] <-"Not Stated (NS)" 

# subset ss@data -> then aggregate to ss_ag -> then merge to ward 


# CREATING SS_WARD_WHITE
ss_white <- subset(ss@data, ss@data$Self.defined.ethnicity == "White")
head(ss_white)

ss_ag_white <- aggregate(ss_white$NAME, list(ss_white$NAME), length)
names(ss_ag_white) <- c('NAME', 'ss_occurance')

ss_ward2_white <- merge(x = ward, y = ss_ag_white, all.x = FALSE)
names(ss_ward2_white)[8] <- "ss_occurance_white" 


# CREATING SS_WARD_BLACK
ss_black <- subset(ss@data, ss@data$Self.defined.ethnicity == "Black or Black British")
head(ss_black)

ss_ag_black <- aggregate(ss_black$NAME, list(ss_black$NAME), length)
names(ss_ag_black) <- c('NAME', 'ss_occurance')

ss_ward2_black <- merge(x = ward, y = ss_ag_black, all.x = FALSE)

names(ss_ward2_black)[8] <- "ss_occurance_black" 


# CREATING SS_WARD_ASIAN
ss_asian <- subset(ss@data, ss@data$Self.defined.ethnicity == "Asian or Asian British")
head(ss_asian)

ss_ag_asian <- aggregate(ss_asian$NAME, list(ss_asian$NAME), length)
names(ss_ag_asian) <- c('NAME', 'ss_occurance')

ss_ward2_asian <- merge(x = ward, y = ss_ag_asian, all.x = FALSE)

names(ss_ward2_asian)[8] <- "ss_occurance_asian" 



# CREATING SS_WARD_mixed
ss_mixed <- subset(ss@data, ss@data$Self.defined.ethnicity == "Mixed")
head(ss_mixed)

ss_ag_mixed <- aggregate(ss_mixed$NAME, list(ss_mixed$NAME), length)
names(ss_ag_mixed) <- c('NAME', 'ss_occurance')

ss_ward2_mixed <- merge(x = ward, y = ss_ag_mixed, all.x = FALSE)

names(ss_ward2_mixed)[8] <- "ss_occurance_mixed" 



# CREATING SS_WARD_chinese_other
ss_chinese_other <- subset(ss@data, ss@data$Self.defined.ethnicity == "Chinese or other ethnic group")
head(ss_chinese_other)

ss_ag_chinese_other <- aggregate(ss_chinese_other$NAME, list(ss_chinese_other$NAME), length)
names(ss_ag_chinese_other) <- c('NAME', 'ss_occurance')

ss_ward2_chinese_other <- merge(x = ward, y = ss_ag_chinese_other, all.x = FALSE)

names(ss_ward2_chinese_other)[8] <- "ss_occurance_chinese_other" 





#### Creating SS_WARD per gender ---------------------------------------------


# SS FEMALE 
ss_female <- subset(ss@data, ss@data$Gender == "Female")
head(ss_female)

ss_ag_female <- aggregate(ss_female$NAME, list(ss_female$NAME), length)
names(ss_ag_female) <- c('NAME', 'ss_occurance')

ss_ward2_female <- merge(x = ward, y = ss_ag_female, all.x = FALSE)
# tm_shape(ss_ward2_female)+tm_polygons("ss_occurance") 


# SS MALE
ss_male <- subset(ss@data, ss@data$Gender == "Male")
head(ss_male)

ss_ag_male <- aggregate(ss_male$NAME, list(ss_male$NAME), length)
names(ss_ag_male) <- c('NAME', 'ss_occurance')

ss_ward2_male <- merge(x = ward, y = ss_ag_male, all.x = FALSE)
#tm_shape(ss_ward2_male)+tm_polygons("ss_occurance") 





# 1.2c Crime #### 

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
names(crime_ward)[8] <- "crime_occurance_ALL" 


# Add pp_mean to ss and crime datasets
names(crime)[11] <- "DISTRICT" 
crime <- merge(crime, pp_mean, by='DISTRICT') 

# There is no police perception data for city of London, so drop these points :(
sapply(crime@data, function(x) sum(is.na(x)))
crime@data <- na.omit(crime@data)

#merge to wards shp
crime_ward_2 <-merge(x=ward, y=crime_ag, all.x = FALSE)
nrow(crime_ward_2) # 650
length(which(table(crime_ward_2$NAME)>1)) # 19 rows were duplicated
# 650 row minus 19 duplicates= 631
(which(table(crime_ward_2$NAME)>1))
crime_ward_2@data

crime_ward_2_data <- crime_ward_2@data
count(is.na(crime_ward_2@data$crime_occurance))

#----------------------------------------------------------------------------

table(crime@data$Crime.type)
# 2 major crimes :
# Anti-social behaviour : 21825, Violence and sexual offences : 17621, Other theft: 8891, Vehicle crime :7490 

table2 <- table(crime@data$Crime.type)
prop.table(table2) # gives proportions per crimes
round(prop.table(table2) ,3)  # proportions rounded to 3 dp 

# make new dataframe getting rid of crime empties
updated_crime <- data.frame(crime@data[crime@data$Crime.type != "",])
typeof(updated_crime)
#----------------------------------------------------------------------------
# Anti-social behaviour
asb <- subset(crime@data, crime@data$Crime.type == 'Anti-social behaviour')
crime_asb_ag <- aggregate(asb$NAME, list(asb$NAME), length)
names(crime_asb_ag) <- c('NAME', 'crime_occurance_asb')

# Create polygon data from point count
crime_ward_asb <- merge(ward, crime_asb_ag, by='NAME') 

# Violence and sexual offences

crime_vso <- subset(crime@data, crime@data$Crime.type == 'Violence and sexual offences')
crime_ag_vso <- aggregate(crime_vso$NAME, list(crime_vso$NAME), length)
names(crime_ag_vso) <- c('NAME', 'crime_vso_occurance')

#  Create polygon data from point count
crime_ward_vso <- merge(x=ward, y = crime_ag_vso, by='NAME')

#----------------------------------------------------------------------------

# 1.2d Stations & Population ####
#### Police Stations ####
# Create Point data from coordinate datasets
xy <- stations[,c('longitude', 'latitude')]
stations <- SpatialPointsDataFrame(coords= xy, data = stations, proj4string = CRS(proj))

# Join Polygon & Point information
stations <- point.in.poly(stations, ward)
# Remove point outside of polygon (NaN GSS_Code)
sapply(stations@data, function(x) sum(is.na(x)))
stations@data <- na.omit(stations@data)

stations_ag <- aggregate(stations@data$NAME, list(stations@data$NAME), length)
names(stations_ag) <- c('NAME', 'police_station_occurance')

# Create polygon data from point count
station_ward <- merge(ward, stations_ag, by='NAME') 
# Set NAs to 0
station_ward@data[is.na(station_ward@data)] <- 0

## CREATE K NEAREST NEIGHBOURS FOR NEAREST POLICE STATION

#### Population ####

# Rename columns
names(pop) <- c('GSS_CODE', 'DISTRICT', 'NAME', 'Mean_Popuation_Age_2013', 'Population_Density_km2_2013', 
                'Ethnic_Group_White_2013', 'Ethnic_Group_Asian_2013', 'Ethnic_Group_Black_2013', 
                'Mortality_Ratio_2013', 'Life_Expectancy_2013', 'Median_House_Prices_2013', 
                'Mean_Household_Income_2013', 'Total_crime_rate_2013')

pop_ward <- merge(ward, pop, by='GSS_CODE')
# Organise data
pop_ward@data <- pop_ward@data[order(pop_ward@data$BOROUGH), ]
rownames(pop_ward@data) <- seq(length=nrow(pop_ward@data))
sapply(pop_ward@data, function(x) sum(is.na(x)))



# Merge missing data on NAME 
missing <- pop_ward[rowSums(is.na(pop_ward@data)) > 0, drop = FALSE]
missing <- missing[ , c(1:7), drop = FALSE]
names(missing@data)[2] <- "NAME" # change name of spatial delineation (col1) to match ward
missing@data$NAME <- sub("&", "and", missing@data$NAME) # replace & with and
replace <- strtoi(rownames(missing@data))
missing <- merge(missing, pop, by='NAME', drop = FALSE)

# Fill in City of London NA's
for(i in 1:nrow(missing@data)) {       # for-loop over rows
  if(missing@data[i, 6] == 'City of London') {
    missing@data[i, c(8:ncol(missing@data))] <- data.frame('E09000001', 'City of London', 41.3, 2406.25, 5799, 940, 193, 45.7, 24.93011, 615000, 99390, 680.7206)
  }
}

sapply(missing@data, function(x) sum(is.na(x)))

# Reorganise dataframes
pop_ward@data <- subset(pop_ward@data, select = -c(NAME.y))
names(pop_ward@data)[2] <- "NAME" # change name of spatial delineation (col1) to match ward
missing@data <- subset(missing@data, select = -c(GSS_CODE.y))
names(missing@data)[2] <- "GSS_CODE" # change name of spatial delineation (col1) to match ward
pop_ward@data <- pop_ward@data[c(colnames(missing@data))]
# Remove rownames that are in missing df
pop_ward <- pop_ward[!(rownames(pop_ward@data) %in% replace),]
# Append missing df
pop_ward <- rbind(pop_ward, missing)

# tm_shape(pop_ward)+tm_polygons("Population_Density_km2_2013", palette="-RdBu", style="quantile")
