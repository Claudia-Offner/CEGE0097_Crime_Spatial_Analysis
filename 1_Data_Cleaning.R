# Project: CRIME SPATIAL ANALYSIS
# File: DATA CLEANING
# Module: CEGE0097
# Student no.: 20198829, 20164326 ...
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided data.
setwd('C:/Users/offne/OneDrive - University College London/CEGE0097 Spatial Analysis and Geocomputation/Coursework')

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



#### 1.2a Police Perceptions #### 

# TOMMY: Add your PRE-PROCESSING bits here

#### 1.2b Stop and Search #### 

# DANNI: Add your PRE-PROCESSING bits here

#### 1.2c Crime #### 

# PRATIBHA: Add your PRE-PROCESSING bits here

