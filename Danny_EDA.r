# CRIME SPATIAL ANALYSIS
# Module: CEGE0097
# Student no.: 20198829 ...
# Date: 22 January 2022

# To run this code, set the work directory to folder containing the provided data.
setwd('C:/Users/offne/OneDrive - University College London/CEGE0097 Spatial Analysis and Geocomputation/Coursework')

#install.packages('tidyverse')
#install.packages('spatialEco')


# Load Packages
library(tmap)
library(ggplot2)
library(sp)
library(rgdal)
library(tidyverse)
library(leaflet)
library(spatialEco)
library(dplyr)
library(plyr)
library(forcats)




#### 1. LOAD DATA ####
# Open csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv', fileEncoding="UTF-8-BOM")
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv', fileEncoding="UTF-8-BOM")
pp <- read.csv(file='Data/2016-police-perceptions.csv', fileEncoding="UTF-8-BOM")


# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # projection info
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp") # read shp file
borough <- spTransform(borough, CRS(proj)) #transform shapefile to CRS
names(borough@data)[1] <- "DISTRICT" # Rename first column to 'district'
ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp") # .shp for individual wards
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





#### 2a. Police Perceptions ####


# Assign borough to pp
names <- pp$Neighbourhood
names <- sub(" -.*", "", names)
pp$Neighbourhood <- c(names)
names(pp)[1] <- "DISTRICT" # rename 'neighborhood' to 'DISTRICT' 



# Take mean of values based on borough & then take row mean
names <- colnames(pp)[-1] # removes the title 'district' and just leaves district names
pp_ag <- pp %>%
  group_by(DISTRICT) %>% 
  summarise_at(vars(names), mean)



pp_ag$mean_pp <- rowMeans(pp_ag[-1]) # create new column called mean_pp 
pp_mean <- subset (pp_ag, select = c(DISTRICT, mean_pp))



# Assign pp to borough shapefile
pp_borough <- merge(borough, pp_ag, by='DISTRICT') 





#### 2b. Stop and Search ####


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




# merging and ss_ward2 ----------------------------------------------------

# Create polygon data from point count
# ***** DONT TOUCH THIS: 
ss_ward <- merge(ward, ss_ag, by='NAME')  # ss_ag -> 595 rows // ward has 657 rows 

# ***** 


# original data:
nrow(ss_ag) # 595 (with no duplicates)

# merge to wards shp
ss_ward2 <- merge(x = ward, y = ss_ag, all.x = FALSE)
plot(ss_ward2)

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

count(pp) # n = 106
rowMeans(pp[-1]) # returns 107 results ? 


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






# **** SS GENDER DATA *** 
# SS: Gender (>80% men) - COMBINE EMPTY VALUES WITH 'OTHER' (or not?) !!

table(ss@data$Gender) # -> gives count per gender 
#       Female   Male  Other 
#  100    568   8383      3 

table2 <- table(ss@data$Gender)
prop.table(table2) # gives proportions per gender 
round(prop.table(table2) ,3)  # proportions rounded to 3 dp 


# frequency table per gender including empties 
x <- prop.table(table(ss@data$Gender))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# plot mean_pp per gender (F / M / O) -> excluding empties
ggplot(data=ss@data[ss@data$Gender!="",], aes(x=Gender, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# make new dataframe getting rid of gender empties
updated_genders <- data.frame(ss@data[ss@data$Gender != "",])
typeof(updated_genders)


# plot updated df 
ggplot(data=updated_genders, aes(x=Gender, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))






# ss age data -------------------------------------------------------------


# **** SS AGE DATA *** 
# SS: Age (>35% are 18-24) (<25% are 25-34) - LABEL EMPTY VALUES WITH 'OTHER' (done) !!

table(ss@data$Age.range) # original data with empty category
#        10-17    18-24    25-34  over 34 under 10 
# 398     1764     3389     2030     1472        1 

# attempt 1 
levels(ss@data$Age.range)[levels(ss@data$Age.range)==""] <-"OTHER"
table(ss@data$Age.range)


x <- prop.table(table(ss@data$Age.range))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Age.range, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




# SS ethnicity ------------------------------------------------------------


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


x <- prop.table(table(ss@data$Self.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)
             

ggplot(data=ss@data, aes(x=Self.defined.ethnicity, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))

 #SELF DEFINED ETHNICITY



# OFFICER DEFINED ETHNICITY  ----------------------------------------------



# *** The officer-defined ethnicity of the person stopped ***

# SS: officer.defined.ethnicity (>40% white) (40% black) - COMBINE EMPTY VALUES WITH 'OTHER' !!

ss@data$Officer.defined.ethnicity

x <- prop.table(table(ss@data$Officer.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# REPLACE EMPTY WITH "NOT STATED"

levels(ss@data$Officer.defined.ethnicity)[levels(ss@data$Officer.defined.ethnicity)=="CHECK"] <-"NOT STATED" 
# THIS ONE WORKS^

table(ss@data$Officer.defined.ethnicity)


ggplot(data=ss@data, aes(x=Officer.defined.ethnicity, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




# object of search --------------------------------------------------------

# SS: object.of.search (>60% Controlled drugs) (~35% articles for use in criminal damage)
x <- prop.table(table(ss@data$Object.of.search))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Object.of.search, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




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

x <- prop.table(table(ss@data$Outcome))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

ggplot(data=ss@data, aes(x=Outcome, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))








# EDA ---------------------------------------------------------------------

# STOP AND SEARCH EDA -> MY CODE:
# add box plots and diagrams later from up there ^ 

# get summary statistics for FEMALE only data
ss_female <- subset(ss@data, ss@data$Gender == "Female")
median(ss_female$mean_pp) # median police perception -> offender = F --> 70.74074
mean(ss_female$mean_pp) # 71.64513


# MALE ONLY DATA
ss_male <- subset(ss@data, ss@data$Gender == "Male")
median(ss_male$mean_pp) #70.59722
mean(ss_male$mean_pp) #71.09726


# ETHNICITY 
# SS _ WHITE ETHNICITY ONLY 
ss_white <- subset(ss@data, ss@data$Self.defined.ethnicity == "White")
median(ss_white$mean_pp)
# repeat for each ethnicity 




# ESDA --------------------------------------------------------------------


# AUTOCORRELATION -> setting up neighbours and weights matrix
library(knitr)
library(spdep)

ttm()  # interactive viewing

# GSS CODE is individual for each ward ( - would use name but name is not individual)
tm_shape(ss_ward)+tm_polygons()   # Plots all ss_ward polygons

neighbours <- poly2nb(ss_ward, row.names = ss_ward@data$GSS_CODE) # create list of neighbours
W <- nb2mat(neighbours, style="W") # create spatial weight matrix
colnames(W) <- rownames(W)
ss_ward$rowID <-  rownames(ss_ward@data$GSS_CODE) 


tm_shape(ss_ward)+tm_polygons()+tm_text(text="GSS_CODE") # plot polygons with GSS_CODE labels


# check that neighbours is working properly:
# WARDS E05011106 should have the following neighbours: E05011112 / E05009330 / E05011104 / E05011116
nbrs <- which(W["E05011106",]>0)
nbrs
tm_shape(ss_ward[nbrs,])+tm_polygons()+tm_text(text="GSS_CODE") # 



# GLOBAL SPATIAL AUTOCORRELATION MEASURES -> MORANS I 
Wl <- nb2listw(neighbours) # a listw object is a weights list for use in autocorrelation measures.
moran.test(ss_ward@data$ss_occurance, Wl, na.action=na.omit) # omit NA values


# Moran I test under randomisation
# data:  ss_ward@data$ss_occurance  
# weights: Wl 

#omitted: 9, 20, 23, 24, 28, 40, 45, 54, 62, 149, 159, 276, 300, 306,
# 308, 312, 379, 386, 432, 449, 517, 576, 633, 635, 636, 637, 638, 639, 
# 640, 641, 642, 643, 644, 645, 647, 649, 650, 651, 652, 653, 654, 655, 657   

# Moran I statistic standard deviate = 11.571, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#  Moran I statistic       Expectation          Variance 
#       0.2767138176     -0.0016313214      0.0005786173 


# Interpret: 

# For the Global Moran's I statistic, the null hypothesis states that the attribute being analyzed is
# randomly distributed among the features in your study area; 

# When the p-value returned by this tool is statistically significant, you can reject the null hypothesis.



# Moran Test using Monte-Carlo simulation
moran.mc(ss_ward@data$ss_occurance, Wl, nsim=999, na.action=na.omit) 
#data:  ss_ward@data$ss_occurance 
# weights: Wl 
# omitted: 9, 20, 23, 24, 28, 40, 45, 54, 62, 149, 159, 276, 300, 306, 308, 312, 379, 386, 432, 449, 517, 576, 633, 635, 636, 637, 638, 639, 640, 641, 642, 643, 644, 645, 647, 649, 650, 651, 652, 653, 654, 655, 657 
# number of simulations + 1: 1000 

# statistic = 0.27671, observed rank = 1000, p-value = 0.001
# alternative hypothesis: greater





library(gstat)

# stop and search semi variogram -> using stop and search occurrence per ward 
ss_semivar <- variogram(ss_ward2@data$ss_occurance~1, ss_ward2)
 
ss_var_fit <- fit.variogram(ss_semivar, vgm("Sph")) # models: "Exp", "Sph", "Gau", "Mat

plot(ss_semivar, ss_var_fit)

summary(ss_semivar) 


# local autocorrelation ---------------------------------------------------


# Local autocorrelation

# morans scatterplot 

# trying to make the ss occurance and weights list field equal length 
# try recalculating the wl using ss_ward2 instead of ss_ward 
# MAY NEED TO REPEAT THIS FOR EARLIER TESTS ?? 
# (changing ss_Ward to ss_ward2 with new WL )

neighbours2 <- poly2nb(ss_ward2, row.names = ss_ward2@data$GSS_CODE) # create list of neighbours
Wl2 <- nb2listw(neighbours2)

moran.plot(ss_ward2@data$ss_occurance, Wl2, xlab="Stop and Search occurance", 
           ylab="Spatailly lagged SS occurance", labels=ss_ward2@data$DISTRICT)


# GETIS ORD -> G AND G* ARE DONE ON ARCGIS PRO -> look in notion for results 



# LOCAL MORANS I STATISTIC 

# Local Morans I Statistic ------------------------------------------------


Ii <- localmoran(ss_ward2$ss_occurance, Wl2)

ss_ward2$Ii <- Ii[,"Ii"]
ss_ward2$Ii_z <- Ii[,"Z.Ii"]

# plot local moran i statistic 
# A positive value for I indicates that a feature has neighboring features with
#similarly high or low attribute values; this feature is part of a cluster. 
# A negative value for I indicates that a feature has neighboring features with 
# dissimilar values; this feature is an outlier.
tm_shape(ss_ward2) + tm_polygons(col="Ii", palette="-RdBu", style="quantile")

# Plot local morans i z scores (aka showing how far each local moran i value is from the mean )
# blue = below the mean // red = above the mean
tm_shape(ss_ward2) + tm_polygons(col="Ii_z", palette="-RdBu", style="quantile")



# tutorial looks at P values (Pr) where the z score (spread) is < 0 
# -> aka local moran values that are not identical to the mean 

# ...Here we are looking at P values where the z score is not equal to the expected (E) score
# specifically -> unadjusted p values

#### THIS IS WHERE THE CODE STOPS RUNNING FOR ME (Claudia) #####
ss_ward2$Ii_p_unadjusted <- Ii[,"Pr(z != E(Ii))"]  # puts all p values into separate subcolumn of ss_ward2


# IF YOU WANT TO CHANGE THE SIGNIFICANCE LEVEL -> 
# AMEND THE NUMBER BELOW AND RE RUN ***ALL 3*** LINES OF CODE:
ss_ward2$Ii_sig <- "non-significant"     # labels all rows non-significant 
ss_ward2$Ii_sig[which(ss_ward2$Ii_p_unadjusted < 0.05)] <- "significant"   # re-labels as significant where associated p values are below set level 
table(ss_ward2$Ii_sig) # un-adjusted p values at 0.05 level gives 64 significant wards

# Plot at given significance level
tm_shape(ss_ward2) + tm_polygons(col="Ii_sig", palette="-RdBu")





# Bonferroni adjustment: Prevent data from incorrectly appearing to be statistically significant
# Adjusting -> P values
Ii_adjusted <- localmoran(ss_ward2$ss_occurance, Wl2, p.adjust.method="bonferroni")
ss_ward2$Ii_p_adjusted <- Ii_adjusted[,"Pr(z != E(Ii))"]


# IF YOU WANT TO CHANGE THE SIGNIFICANCE LEVEL -> 
# AMEND THE NUMBER BELOW AND RE RUN ***ALL 3*** LINES OF CODE:
ss_ward2$Ii_adj_sig <- "nonsignificant"
ss_ward2$Ii_adj_sig[which(ss_ward2$Ii_p_adjusted < 0.05)] <- "significant"
table(ss_ward2$Ii_adj_sig) # adjusted p values at 0.05 level gives 40 significant wards

tm_shape(ss_ward2) + tm_polygons(col="Ii_adj_sig", palette="-RdBu")

colnames(ss_ward2@data)
# Need ii_sig / ii_adj_sig / ii / zi / Ii_p_unadjusted / ii_p_adjusted


