# CRIME SPATIAL ANALYSIS
# Module: CEGE0097
# Student no.: 20198829, 20164326, ... , ...
# Date: 22 January 2022

# ______________________________________________________________________________
# PROJECT WORKFLOW/PSEUDOCODE
# ______________________________________________________________________________

# WORKFLOW:
# 0. SETUP
      ## setting environment
      ## importing libraries
# 1. LOADING DATA
      ## loading csv
      ## loading shapefiles
# 2. PRE-PROCESSING DATA:
      ## cleaning data
      ## preparing pp
      ## preparing crime
      ## preparing s&s
      ## combining datasets
# 3. NON-SPATIAL EDA
      ## exploring distribution
      ## exploring variation
# 4. SPATIAL EDA
      ## exploring geographic distribution
      ## exploring geographic variation
# 5. PP ANALYSIS
# 6. ...
# 7. ...

#_______________________________________________________________________________
# 0. SETUP
#_______________________________________________________________________________

# set environment
# setwd('C:\\Users\\Tommy\\OneDrive - University College London\\Modules-Notebooks\\CEGE0097_Geocomputation\\Assignment')
# setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')

# install packages("tmap")
# install.packages("ggplot2")
# install.packages('tidyverse')
# install.packages('spatialEco')

# load packages
library(tmap)       # for creating thematic maps
library(ggplot2)    # for creating plots from a dataframe 
library(sp)         # for accessing classes/methods that handle spatial data 
library(rgdal)      # for reading spatial data in various formats via GDAL 
library(tidyverse)  # collection of packages that work in harmony to model, transform & visualise data
library(leaflet)    # for creating interactive 'web' maps in the console
library(spatialEco) # for spatial data manipulation, querying, sampling & modelling.
library(dplyr)      # for efficient manipulation of dataframe like objects
library(plyr)
library(gridExtra)
library(forcats)

#_______________________________________________________________________________
#### 1. LOADING DATA #### 
#_______________________________________________________________________________

# load csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv')  # read csv and assign to var
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv')
pp <- read.csv(file='Data/2016-police-perceptions.csv')
pp_wab <- read.csv(file='Data/2015-16 _to_2020-21_inclusive_neighbourhood_indicators_final_221221.csv') # wab = white, asian, black

# load shapefiles
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # sets projection var using proj4string of params
ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp") # reads OGR data into vector object
borough <- spTransform(borough, CRS(proj)) # transforms 'borough' to WGS84 proj
names(borough@data)[1] <- "DISTRICT"  # change name of spatial delineation (col1) to match ward
#neighbourhood <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
#neighbourhood <- spTransform(ward, CRS(proj))

#_______________________________________________________________________________
#### 2. PREPROCESSING DATA #### 
#_______________________________________________________________________________

####2.1 GENERAL CLEANING ####---------------------------------------------------

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


#### 2.1. PREPROCESSING POLICE PERCEPTION (PP)  #### ---------------------------

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


#### 2.2. PREPROCESSING STOP & SEARCH (S&S) ####  ------------------------------

### Create points from coordinate datasets
xy <- ss[,c('Longitude', 'Latitude')]
ss <- SpatialPointsDataFrame(coords= xy, data = ss, proj4string = CRS(proj))

### Join Polygon & Point information
ss <- point.in.poly(ss, ward)

### Remove point outside of polygon (NaN GSS_Code)
sapply(ss@data, function(x) sum(is.na(x)))
ss@data <- na.omit(ss@data)

### *****
### Aggregate point data by ward NAME (COUNT points): Specific demographics will 
### need to be counted and examined separately, and this will be determined by 
### non spatial EDA
### *****
ss_ag <- aggregate(ss@data$NAME, list(ss@data$NAME), length)
names(ss_ag) <- c('NAME', 'ss_occurance')

### Create polygon data from point count
ss_ward <- merge(ward, ss_ag, by='NAME') 

#### 2.3. PREPROCESSING CRIME  #### --------------------------------------------
### Create Point data from coordinate datasets
xy <- crime[,c('Longitude', 'Latitude')]
crime <- SpatialPointsDataFrame(coords= xy, data = crime, proj4string = CRS(proj))

### Join Polygon & Point information
crime <- point.in.poly(crime, ward)
### Remove point outside of polygon (NaN GSS_Code)
sapply(crime@data, function(x) sum(is.na(x)))
crime@data <- na.omit(crime@data)

### *****
### Aggregate point data by ward NAME (COUNT points): Specific demographics will 
### need to be counted and examined separately, and this will be determined by 
### non spatial EDA
### *****
crime_ag <- aggregate(crime@data$NAME, list(crime@data$NAME), length)
names(crime_ag) <- c('NAME', 'crime_occurance')

### Create polygon data from point count
crime_ward <- merge(ward, crime_ag, by='NAME') 

#### 2.4 COMBINING DATASETS #### -----------------------------------------------

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

#_______________________________________________________________________________
#### 3. NON-SPATIAL EDA #### 
#_______________________________________________________________________________

#### 3.1. EXPLORING DESCRIPTIVE STATISTICS #### --------------------------------

### pp
head(pp_per_borough)                        # print first rows
nrow(pp_per_borough)                        # count observations (rows)
ncol(pp_per_borough)                        # count attributes (cols)
colnames(pp_per_borough)                    # get attribute names
summary(pp_per_borough)                     # get attribute stats e.g. min, max, mean, median, quartile(s)
for (i in colnames(pp_per_borough)){        # get standard deviation per attribute
  print(paste(i, sd(pp_per_borough[[i]])))
}

### crime
colnames(crime@data)
head(crime@data)
summary(crime@data)
for (i in colnames(crime@data)){
  print(paste(i, sd(crime@data[[i]])))
}

### ss
colnames(ss@data)
head(ss@data)
summary(ss@data)
for (i in colnames(ss@data)){
  print(paste(i, sd(ss@data[[i]])))
}

#### 3.2. EXPLORING DISTRIBUTION #### ------------------------------------------

### pp distribution
mean_distribution <- function(pp, title) {
  pp_dist <- ggplot(pp, aes(rowMeans(pp[-1]))) + 
    geom_histogram(aes(y =..density..), breaks = seq(floor(min(rowMeans(pp[-1]))), ceiling(max(rowMeans(pp[-1]))), by = 1), ) + 
    geom_vline(aes(xintercept = mean(rowMeans(pp[-1]))),col='red',size=1) +         # add line for mean (red)
    geom_vline(aes(xintercept = median(rowMeans(pp[-1]))),col='blue',size=1)+       # Q2 = median-q
    geom_vline(aes(xintercept = quantile(rowMeans(pp[-1]))[2]),col='blue',size=1) + # Q1 = lower-q
    geom_vline(aes(xintercept = quantile(rowMeans(pp[-1]))[4]),col='blue',size=1) + # Q3 = upper-q
    ggtitle(title) +
    stat_function(fun = dnorm, args = list(mean = mean(rowMeans(pp[-1])), sd = sd(rowMeans(pp[-1]))))
  return(pp_dist)
}

pp_dist <- mean_distribution(pp,"PP Dist.: All Respondents")               # all respondents
pp_white_dist <- mean_distribution(pp_white,"PP Dist.: White Respondents") # white respondents
pp_asian_dist <- mean_distribution(pp_asian,"PP Dist.: Asian Respondents") # asian respondents
pp_black_dist <- mean_distribution(pp_black,"PP Dist.: Black Respondents") # black respondents
require(gridExtra)
grid.arrange(pp_dist, pp_white_dist, pp_asian_dist, pp_black_dist, ncol=2)

### crime distribution

### s&s distribution


#### 3.3. EXPLORING VARIATION #### --------------------------------------------

### foo: barcharts showing variation by indicator
barchart <- function(pp_per_borough,  indicator_col, title_indicator, respondent) {
  pp_bar <- ggplot(data=pp_per_borough@data, aes_string(x="DISTRICT", y = indicator_col)) +
    geom_col(fill="#0099f9", width=0.85) +
    geom_text(aes_string(label = indicator_col)) + 
    coord_flip() +
    labs(title = paste(title_indicator,"Rating by", respondent, sep=" "), caption = "Source data: MPS/MOPAC", y = 'SCORE (%)') +
    theme_minimal()
  return(pp_bar)
}

### call: barchart by 'good job' pp indicator
br_white_goodjob <- barchart(pp_per_borough, "WHITE_goodjob", "'Good Job'", "White Respondents")
br_asian_goodjob <- barchart(pp_per_borough, "ASIAN_goodjob", "'Good Job'", "Asian Respondents")
br_black_goodjob <- barchart(pp_per_borough, "BLACK_goodjob", "'Good Job'", "Black Respondents")
br_all_goodjob <- barchart(pp_per_borough, "ALL_goodjob", "'Good Job'", "All Respondents")
grid.arrange(br_white_goodjob, br_asian_goodjob, br_black_goodjob, br_all_goodjob, ncol=2)

### call: barchart by 'fair' pp indicator
br_white_fair <- barchart(pp_per_borough, "WHITE_fair", "'Fair'", "White Respondents")
br_asian_fair <- barchart(pp_per_borough, "ASIAN_fair", "'Fair'", "Asian Respondents")
br_black_fair <- barchart(pp_per_borough, "BLACK_fair", "'Fair'", "Black Respondents")
br_all_fair <- barchart(pp_per_borough, "ALL_fair", "'Fair'", "All Respondents")
grid.arrange(br_white_fair, br_asian_fair, br_black_fair, br_all_fair, ncol=2)

### call: barchart by 'fair' pp indicator
br_white_fairss <- barchart(pp_per_borough, "WHITE_fairSS", "'Fair S&S'", "White Respondents")
br_asian_fairss <- barchart(pp_per_borough, "ASIAN_fairSS", "'Fair S&S'", "Asian Respondents")
br_black_fairss <- barchart(pp_per_borough, "BLACK_fairSS", "'Fair S&S'", "Black Respondents")
grid.arrange(br_white_fairss, br_asian_fairss, br_black_fairss, ncol=2)

### call: barchart by 'listens' pp indicator
br_white_listens <- barchart(pp_per_borough, "WHITE_listens", "'Listens'", "White Respondents")
br_asian_listens <- barchart(pp_per_borough, "ASIAN_listens", "'Listens'", "Asian Respondents")
br_black_listens <- barchart(pp_per_borough, "BLACK_listens", "'Listens'", "Black Respondents")
br_all_listens <- barchart(pp_per_borough, "ALL_listens", "'Listens'", "All Respondents")
grid.arrange(br_white_listens, br_asian_listens, br_black_listens, br_all_listens, ncol=2)

### call: barchart by calculated mean pp indicator
br_white_mean <- barchart(pp_per_borough, "PP_WHITE_MEAN", "Mean PP", "White Respondents")
br_asian_mean <- barchart(pp_per_borough, "PP_ASIAN_MEAN", "Mean PP", "Asian Respondents")
br_black_mean <- barchart(pp_per_borough, "PP_BLACK_MEAN", "Mean PP", "Black Respondents")
br_all_mean <- barchart(pp_per_borough, "PP_ALL_MEAN", "Mean PP", "All Respondents")
grid.arrange(br_white_mean, br_asian_mean, br_black_mean, br_all_mean, ncol=2)
  
### foo: boxplots showing variation by indicator
boxplots <- function(pp,  indicator_col, title_indicator, respondent) {
  bx_pp <- ggplot(data=pp, aes_string(x="DISTRICT", y=indicator_col)) + 
    geom_boxplot(fill="#0099f9") +
    coord_flip() +
    labs(title = paste(title_indicator,"Rating by", respondent, sep=" "), caption = "Source data: MPS/MOPAC", y = 'SCORE (%)') +
    scale_y_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0))
  return(bx_pp)
}

### call: boxpplot by 'good job' indicator
bx_white_goodjob <- boxplots(pp_white,'WHITE_goodjob', "'Good Job'", 'White Respondents')
bx_asian_goodjob <- boxplots(pp_asian,'ASIAN_goodjob', "'Good Job'", 'Asian Respondents')
bx_black_goodjob <- boxplots(pp_black,'BLACK_goodjob', "'Good Job'", 'Black Respondents')
bx_all_goodjob <- boxplots(pp,'ALL_goodjob', "'Good Job'", 'All Respondents')
grid.arrange(bx_white_goodjob, bx_asian_goodjob, bx_black_goodjob, bx_all_goodjob, ncol=2)

### call: boxpplot by 'fair' indicator
bx_white_fair <- boxplots(pp_white,'WHITE_fair', "'Fair'", 'White Respondents')
bx_asian_fair <- boxplots(pp_asian,'ASIAN_fair', "'Fair'", 'Asian Respondents')
bx_black_fair <- boxplots(pp_black,'BLACK_fair', "'Fair'", 'Black Respondents')
bx_all_fair <- boxplots(pp,'ALL_fair', "'Fair'", 'All Respondents')
grid.arrange(bx_white_fair, bx_asian_fair, bx_black_fair, bx_all_fair, ncol=2)

### call: boxpplot by 'fair s&S' indicator
bx_white_fairss <- boxplots(pp_white,'WHITE_fairSS', "'Fair S&S'", 'White Respondents')
bx_asian_fairss <- boxplots(pp_asian,'ASIAN_fairSS', "'Fair S&S'", 'Asian Respondents')
bx_black_fairss <- boxplots(pp_black,'BLACK_fairSS', "'Fair S&S'", 'Black Respondents')
grid.arrange(bx_white_fairss, bx_asian_fairss, bx_black_fairss, ncol=2)

### call: boxpplot by 'listens' indicator
bx_white_listens <- boxplots(pp_white,'WHITE_listens', "'Listens'", 'White Respondents')
bx_asian_listens <- boxplots(pp_asian,'ASIAN_listens', "'Listens'", 'Asian Respondents')
bx_black_listens <- boxplots(pp_black,'BLACK_listens', "'Listens'", 'Black Respondents')
bx_all_listens <- boxplots(pp,'ALL_listens', "'Listens'", 'All Respondents')
grid.arrange(bx_white_fair, bx_asian_fair, bx_black_fair, bx_all_fair, ncol=2)

### call: boxpplot by mean pp attribute
bx_white_mean <- boxplots(pp_white,'WHITE_listens', "Mean PP", 'White Respondents')
bx_asian_mean <- boxplots(pp_asian,'ASIAN_listens', "Mean PP", 'Asian Respondents')
bx_black_mean <- boxplots(pp_black,'BLACK_listens', "Mean PP'", 'Black Respondents')
bx_all_mean <- boxplots(pp,'ALL_listens', "Mean PP", 'All Respondents')
grid.arrange(bx_white_mean, bx_asian_mean, bx_black_mean, bx_all_mean, ncol=2)

### foo: combined boxplots for each indicator
combined_boxplot <- function(pp_white, pp_asian, pp_black, indicator) {
  # 'y' var must have same name across dataframes
  names(pp_white)[names(pp_white) == paste("WHITE",indicator,sep="_")] <- "TEMP"
  names(pp_asian)[names(pp_asian) == paste("ASIAN",indicator,sep="_")] <- "TEMP"
  names(pp_black)[names(pp_black) == paste("BLACK",indicator,sep="_")] <- "TEMP"
  # create boxplot
  bx_pp <- ggplot(NULL, aes_string(x="DISTRICT", y="TEMP")) + 
    geom_boxplot(data=pp_white, alpha=.4, width=0.6, color="#0057e7", fill="#001eff", position = position_nudge(x = -0.05, y = 0)) +
    geom_boxplot(data=pp_asian, alpha=.4, width=0.6, color="#1fc700", fill="#1fc700", position = position_nudge(x = 0, y = 0)) +
    geom_boxplot(data=pp_black, alpha=.4, width=0.6, color="#d62d20", fill="#d62d20", position = position_nudge(x = 0.05, y = 0)) +
    coord_flip() +
    labs(title = toupper(indicator), caption = "Source data: MPS/MOPAC", y = 'SCORE (%)') +
    scale_y_continuous(limits=c(40,100), breaks=seq(40,100,10), expand = c(0, 0))
  return(bx_pp)
}

### call: combined boxplots
bx_goodjob <- combined_boxplot(pp_white, pp_asian, pp_black, "goodjob")
bx_fair <- combined_boxplot(pp_white, pp_asian, pp_black, "fair")
bx_fairss <- combined_boxplot(pp_white, pp_asian, pp_black, "fairSS")
bx_listens <- combined_boxplot(pp_white, pp_asian, pp_black, "listens")
bx_goodjob
bx_fair
bx_fairss
bx_listens


### crime by 'type' attribute (res: 25% Anti-social behaviour, 20% Violence and sexual offense)
x <- prop.table(table(crime@data$Crime.type))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=crime@data, aes(x=Crime.type, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(crime@data$PP_ALL_MEAN, c(0, 0.97)))

### ss by 'gender' (res: Gender (>80% men) - COMBINE EMPTY VALUES WITH 'OTHER' !!)
x <- prop.table(table(ss@data$Gender))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Gender, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

### ss by 'Age' (res: (>35% are 18-24) (<25% are 25-34) - LABEL EMPTY VALUES WITH 'OTHER' !!)
x <- prop.table(table(ss@data$Age.range))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Age.range, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

### SS by self-defined ethnicity - NEEDS CATEGORICAL AGGREGATION!!
x <- prop.table(table(ss@data$Self.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Self.defined.ethnicity, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

## SS by officer-defined ethnicity (>40% white) (40% black) - COMBINE EMPTY VALUES WITH 'OTHER' !!
x <- prop.table(table(ss@data$Officer.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Officer.defined.ethnicity, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

## SS by object.of.search (>60% Controlled drugs) (~35% articles for use in criminal damage)
x <- prop.table(table(ss@data$Object.of.search))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Object.of.search, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

## SS by outcome (>60% nothing found - no further action) (~20% suspect arrested)  - NEEDS CATEGORICAL AGGREGATION!!
x <- prop.table(table(ss@data$Outcome))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frequency (%)", las=2)

ggplot(data=ss@data, aes(x=Outcome, y=PP_ALL_MEAN)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$PP_ALL_MEAN, c(0, 0.97)))

#_______________________________________________________________________________
#### 4. SPATIAL EDA #### 
#_______________________________________________________________________________

#### 4.1. EXPLORING GEOGRAPHIC DISTRIBUTION #### -------------------------------

### crime: point plot
crimemap <- leaflet(crime) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-0.1257400, 51.5085300, zoom = 10) %>% 
  addPolygons(data=ward,
              col = 'dodgerblue',
              stroke = FALSE, 
              fillOpacity = 0.3, 
              smoothFactor = 0.5) %>% 
  addCircleMarkers(~Longitude, ~Latitude,
                   weight = 3,
                   radius=1,
                   color="#ffa500")
crimemap

### ss: point plot
ssmap <- leaflet(ss) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-0.1257400, 51.5085300, zoom = 10) %>% 
  addPolygons(data=ward,
              col = 'dodgerblue',
              stroke = FALSE, 
              fillOpacity = 0.3, 
              smoothFactor = 0.5) %>% 
  addCircleMarkers(~Longitude, ~Latitude,
                   weight = 3,
                   radius=1,
                   color="#08A218")
ssmap

### crime: choropleth map
### https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html
crime_choro <- leaflet(crime_ward) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", crime_occurance)(crime_occurance) )
crime_choro

### ss: choropleth map
ss_choro <- leaflet(ss_ward) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ss_occurance)(ss_occurance) ) 
ss_choro

### pp: choropleth maps
pp_choro <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", PP_ALL_MEAN)(PP_ALL_MEAN) )
pp_choro


#### 4.1. EXPLORING GEOGRAPHIC VARIATION #### ----------------------------------

## pp: choropleth maps by 'fair' indicator
choro_white_fair <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", WHITE_fair)(WHITE_fair) )
choro_white_fair

choro_asian_fair <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ASIAN_fair)(ASIAN_fair) )
choro_asian_fair

choro_black_fair <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", BLACK_fair)(BLACK_fair) )
choro_black_fair

## pp: choropleth maps by 'fair S&S' indicator
choro_white_fairss <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", WHITE_fairSS)(WHITE_fairSS) )
choro_white_fairss

choro_asian_fairss <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ASIAN_fairSS)(ASIAN_fairSS) )
choro_asian_fairss

choro_black_fairss <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", BLACK_fairSS)(BLACK_fairSS) )
choro_black_fairss

## pp: choropleth maps by 'listens' indicator
choro_white_listens <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", WHITE_listens)(WHITE_listens) )
choro_white_listens

choro_asian_listens <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ASIAN_listens)(ASIAN_listens) )
choro_asian_listens

choro_black_listens <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", BLACK_listens)(BLACK_listens) )
choro_black_listens

### pp: choropleth maps by mean pp (calculated var)
choro_white_mean <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", PP_WHITE_MEAN)(PP_WHITE_MEAN) )
choro_white_mean

choro_asian_mean <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", PP_ASIAN_MEAN)(PP_ASIAN_MEAN) )
choro_asian_mean

choro_black_mean <- leaflet(pp_per_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", PP_BLACK_MEAN)(PP_BLACK_MEAN) )
choro_black_mean
