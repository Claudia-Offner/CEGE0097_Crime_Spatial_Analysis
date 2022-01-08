# To run this code, set the work directory to folder containing the provided data.
setwd('C:/Users/Lenovo/Documents/Report')



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
ss <- read.csv(file='2016-06-metropolitan-stop-and-search.csv')
crime <- read.csv(file='2016-06-metropolitan-street.csv')
pp <- read.csv(file='2016-police-perceptions.csv')

# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
borough <- readOGR(dsn="London_Shapefiles/London_Borough_Excluding_MHW.shp")
borough <- spTransform(borough, CRS(proj))
names(borough@data)[1] <- "DISTRICT"
ward <- readOGR(dsn="London_Shapefiles/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))



#### 2. CLEAN & AGGREGATE DATA ####
# Check missing values
sapply(crime, function(x) sum(is.na(x)))
sapply(ss, function(x) sum(is.na(x)))
sapply(pp, function(x) sum(is.na(x)))
sapply(borough@data, function(x) sum(is.na(x)))
sapply(ward@data, function(x) sum(is.na(x)))

# Drop Columns where all instances are missing OR irrelevant 
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


#### 2a. Police Perceptions ####

# Assign borough to pp
names <- pp$ï..Neighbourhood
names <- sub(" -.*", "", names)
pp$ï..Neighbourhood <- c(names)
names(pp)[1] <- "DISTRICT"

# Take mean of values based on borough & then take row mean
names <- colnames(pp)[-1]
pp_ag <- pp %>%
  group_by(DISTRICT) %>% 
  summarise_at(vars(names), mean)

pp_ag$mean_pp <- rowMeans(pp_ag[-1])
pp_mean <- subset (pp_ag, select = c(DISTRICT, mean_pp))

# Assign pp to borough shapefile
pp_borough <- merge(borough, pp_ag, by='DISTRICT') 


#### 2b. Stop and Search ####
#_______________________________________________________________________________
# NOTE: Re-code all relevant categorical variables prior to aggregation
#_______________________________________________________________________________

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
ss_ag <- aggregate(ss@data$NAME, list(ss@data$NAME), length)
names(ss_ag) <- c('NAME', 'ss_occurance')

# Create polygon data from point count
ss_ward <- merge(ward, ss_ag, by='NAME') 


#### 2c. Crime ####
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


#### 2d. Combine All Data ####

df <- merge(crime_ward, ss_ward, by='NAME')
df <- merge(df, pp_mean, by='DISTRICT')

# Add pp_mean to ss and crime datasets
crime <- merge(crime, pp_mean, by='DISTRICT')
ss <- merge(ss, pp_mean, by='DISTRICT')

# There is no police perception data for city of London, so drop these points :(
sapply(crime@data, function(x) sum(is.na(x)))
sapply(ss@data, function(x) sum(is.na(x)))
crime@data <- na.omit(crime@data)
ss@data <- na.omit(ss@data)

#### 3. Visualize Data ####

# POINT DATA
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


# POLYGON DATA
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html

# Choropleth maps
crime_choro <- leaflet(crime_ward) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", crime_occurance)(crime_occurance) )
crime_choro

ss_choro <- leaflet(ss_ward) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ss_occurance)(ss_occurance) )
ss_choro

pp_choro <- leaflet(pp_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", mean_pp)(mean_pp) )
pp_choro



#### 4. NON-SPATIAL EDA ####

# Look at Crime data
colnames(crime@data)
head(crime@data)
summary(crime@data)
for (i in colnames(crime@data)){
  print(paste(i, sd(crime@data[[i]])))
}

# Look at SS data
colnames(ss@data)
head(ss@data)
summary(ss@data)
for (i in colnames(ss@data)){
  print(paste(i, sd(ss@data[[i]])))
}

# Look at PP data
colnames(pp)
head(pp)
summary(pp)
for (i in colnames(pp)){
  print(paste(i, sd(pp[[i]])))
}

##### 4a. Continuous Variables ####

# Mean police perceptions - normaly distributed 
ggplot(pp, aes(rowMeans(pp[-1]))) + 
  geom_histogram(aes(y =..density..), breaks = seq(60, 82, by = 1), ) + 
  stat_function(fun = dnorm, args = list(mean = mean(rowMeans(pp[-1])), sd = sd(rowMeans(pp[-1]))))

#### 4b. Categorical Variables ####


# Examine categorical variables - NEEDS CATEGORICAL AGGREGATION!!
# Crime type: (25% Anti-social behaviour) (20% Violence and sexual offenSe)
x <- prop.table(table(crime@data$Crime.type))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=crime@data, aes(x=Crime.type, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(crime@data$mean_pp, c(0, 0.97)))

# SS: Gender (>80% men) - COMBINE EMPTY VALUES WITH 'OTHER' !!
x <- prop.table(table(ss@data$Gender))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Gender, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# SS: Age (>35% are 18-24) (<25% are 25-34) - LABEL EMPTY VALUES WITH 'OTHER' !!
x <- prop.table(table(ss@data$Age.range))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Age.range, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# SS: Self.defined.ethnicity - NEEDS CATEGORICAL AGGREGATION!!
x <- prop.table(table(ss@data$Self.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Self.defined.ethnicity, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))

# SS: officer.defined.ethnicity (>40% white) (40% black) - COMBINE EMPTY VALUES WITH 'OTHER' !!
x <- prop.table(table(ss@data$Officer.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Officer.defined.ethnicity, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))

# SS: object.of.search (>60% Controlled drugs) (~35% articles for use in criminal damage)
x <- prop.table(table(ss@data$Object.of.search))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Object.of.search, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))

# SS: outcome (>60% nothing found - no further action) (~20% suspect arrested)  - NEEDS CATEGORICAL AGGREGATION!!
x <- prop.table(table(ss@data$Outcome))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Outcome, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# SPPA
library(spatstat)
library(ggplot2)
library(knitr)

# splitting the data according to crime type-a. anti social beh

library(sf)
library(readxl)
library(janitor)
library(tmap)
library(dplyr)
library(sp)
library(spdep)

crime_type <- split(crime@data, crime@data$Crime.type)
crime_type[[1]]

anti_beh <- crime_type[[1]]
anti_beh   # has district
plot(borough)  # has district


# Create Point data from coordinate datasets
ab <- anti_beh[,c('Longitude', 'Latitude')]
anti_social <- SpatialPointsDataFrame(coords= ab, data = anti_beh, proj4string = CRS(proj))
plot(anti_social)


# Join Polygon & Point information
anti_social_behaviour <- point.in.poly(anti_social, borough)

#Aggregate point data by ward NAME (COUNT points): Specific crime type will 
# need to be counted and examined separately, and this will be determined by 
# non spatial EDA

asb_ag <- aggregate(anti_social@data$DISTRICT, list(anti_social@data$DISTRICT), length)
names(asb_ag) <- c('DISTRICT', 'crime_occurance')
#names(asb_ag)
# Create polygon data from point count
asb_ward <- merge(borough, asb_ag, by='DISTRICT') 
asb_ward

# POINT DATA
asbmap <- leaflet(anti_social) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-0.1257400, 51.5085300, zoom = 10) %>% 
  addPolygons(data=borough,
              col = 'dodgerblue',
              stroke = FALSE, 
              fillOpacity = 0.3, 
              smoothFactor = 0.5) %>% 
  addCircleMarkers(~Longitude, ~Latitude,
                   weight = 3,
                   radius=1,
                   color="#ffa500")
asbmap

# POLYGON DATA
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html

# Choropleth maps
asb_choro <- leaflet(asb_ward) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", crime_occurance)(crime_occurance) )
asb_choro

# from tutorial
tm_shape(asb_ward) + tm_polygons(col="crime_occurance", palette="YlOrRd", style="jenks") +
  tm_shape(anti_social_behaviour) + tm_dots( style="jenks") + tm_layout(legend.outside = TRUE, legend.text.size = 1)

library(spdep)
library(knitr)
nb <- poly2nb(asb_ward)
W <- nb2mat(nb, style="W")
colnames(W) <- rownames(W)
kable(W[1:10,1:10], digits=3, caption="First 10 rows and columns of W for London wards", booktabs=T)

# Add the row IDs as a column in the data matrix to plot using tmap
asb_ward$rowID <- rownames(asb_ward@data)

# plot the first 10 polygons in medianhpward and label with ID
tm_shape(asb_ward[1:33,])+tm_polygons()+tm_text(text="rowID")


nbrs <- which(W["4",]>0) # Find column indices of neighbours of ward 4 (result of which(W[,"4"]>0) would be identical)
tm_shape(asb_ward[nbrs,])+tm_polygons()+tm_text(text="rowID")

Wl <- nb2listw(nb) # a listw object is a weights list for use in autocorrelation measures.
moran(asb_ward$crime_occurance, Wl, n=length(Wl$neighbours), S0=Szero(Wl))
asb_ward$crime_occurance
#a <- point.in.poly(anti_spatial, borough)
#counts_anti <- table(anti_beh$DISTRICT)
#counts_anti
#plot(counts_anti)







