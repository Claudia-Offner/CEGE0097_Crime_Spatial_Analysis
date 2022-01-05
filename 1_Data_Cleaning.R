# CRIME SPATIAL ANALYSIS
# Module: CEGE0097
# Student no.: 20198829 ...
# Date: 22 January 2022

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

#### 1. LOAD DATA ####
# Open csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv', fileEncoding="UTF-8-BOM")
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv', fileEncoding="UTF-8-BOM")
pp <- read.csv(file='Data/2016-police-perceptions.csv', fileEncoding="UTF-8-BOM")

# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp")
borough <- spTransform(borough, CRS(proj))
names(borough@data)[1] <- "DISTRICT"
ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))


#### 2. CLEAN & AGGREGATE DATA ####
# Check missing values
sapply(crime, function(x) sum(is.na(x)))
sapply(ss, function(x) sum(is.na(x)))
sapply(pp, function(x) sum(is.na(x)))
sapply(borough@data, function(x) sum(is.na(x)))
sapply(ward@data, function(x) sum(is.na(x)))

# Drop Columns where all instances are miSsing OR irrelevant 
crime <- subset (crime, select = -c(Crime.ID, Context, Reported.by, Falls.within, Location, LSOA.code, LSOA.name, Last.outcome.category))
ss <- subset (ss, select = -c(Part.of.a.policing.operation, Type, Legislation, Policing.operation, Removal.of.more.than.just.outer.clothing, Outcome.linked.to.object.of.search))
pp <- subset (pp, select = -c(Code, Group, Total.Notifiable.Offences.rate))
borough@data <- subset (borough@data, select = -c(SUB_2006, SUB_2009))

# Drop Values with NA (831 in lat/long)
crime <- na.omit(crime)
ss <- na.omit(ss)

# Add unique id's
ss$ID <- seq_along(ss[,1])
crime$ID <- seq_along(crime[,1])

# Change data types where applicable
pp <- pp[-1,]
names <- colnames(pp)[-1]
pp[ ,names] <- apply(pp[ , names], 2, function(x) as.numeric(as.character(x))) # Change characters to numeric



#### 2.a #### 
#### 2.b #### 
#### 2.c #### 
