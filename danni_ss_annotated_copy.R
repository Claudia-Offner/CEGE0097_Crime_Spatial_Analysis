# CRIME SPATIAL ANALYSIS
# Module: CEGE0097
# Student no.: 20198829 ...
# Date: 22 January 2022


install.packages('tidyverse')
install.packages('spatialEco')


# Load Packages
library(tmap)
library(ggplot2)
library(sp)
library(rgdal)
library(tidyverse)
library(leaflet)
library(spatialEco)
install.packages("spatialEco")
library(dplyr)
library(plyr)
library(forcats)
library(knitr)
library(spdep)
library(gstat)


#### 1. LOAD DATA ####
# Open csv files
ss <- read.csv(file='./2016-06-metropolitan-stop-and-search.csv')
crime <- read.csv(file='./2016-06-metropolitan-street.csv')
pp <- read.csv(file='./2016-police-perceptions.csv')


# Open shape files
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # projection info
borough <- readOGR(dsn="./London_Shapefiles/London_Borough_Excluding_MHW.shp") # read shp file
borough <- spTransform(borough, CRS(proj)) #transform shapefile to CRS
names(borough@data)[1] <- "DISTRICT" # Rename first column to 'district'
ward <- readOGR(dsn="./London_Shapefiles/London_Ward.shp") # .shp for individual wards
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
# ss = stop and search
# pp = police perceptions
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


# install point.in.polygon -> package associated 
# ss_ag is the the name of the distructs and the ss_occurance values
ss_ag <- aggregate(ss@data$NAME, list(ss@data$NAME), length)
names(ss_ag) <- c('NAME', 'ss_occurance')




# Merging ward polygons and ss data (creating ss_ward and ss_ward2) ----------------------------------------------------

# Create polygon data from point count
# merge ward polygon data to ss_ag 
# ss_ward has 657 rows for ward names due to duplicates 
# ss_ag only has 595 
ss_ward <- merge(ward, ss_ag, by='NAME')  # ss_ag -> 595 rows // 


# ss_ward2 is created because i am trying to make the length of ...
# ..ss_occurance and NAME the same (important for later functions)
ss_ward2 <- merge(x = ward, y = ss_ag, all.x = FALSE)
plot(ss_ward2)

nrow(ss_ward2) # 614 
nrow(ss_ag) # 595
length(which(table(ss_ward2$NAME)>1)) # 19 rows of ward names are duplicated 
# 614 - 19 = 595 


# Abbey // barnhill -> examples of districts whose names are repeated
ordered_ss_ward2 <- ss_ward2[order(ss_ward2$NAME),]
sub <- subset(ss_ward2@data, ss_ward2@data$NAME == "Abbey") #ignore


ss_ward2_data <- ss_ward2@data
count(is.na(ss_ward2@data$ss_occurance)) # no NA values in ss_occurance 


#### 2c. Crime ####

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

ss_choro <- leaflet(ss_ward) %>%  # USE OF SS_WARD -> DISTRICTS WITH EMPTY SS OCCURANCE VALUES COLOURED GREY
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ss_occurance)(ss_occurance) )
ss_choro


ss_choro <- leaflet(ss_ward2) %>%   # USE OF SS_WARD2 -> DISTRICTS WITH EMPTY SS OCCURANCE LEFT OUT (AKA TRANSPARENT)
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", ss_occurance)(ss_occurance) )
ss_choro

pp_choro <- leaflet(pp_borough) %>% 
  addProviderTiles("CartoDB.Positron")  %>% 
  setView(-0.1257400, 51.5085300, zoom = 10) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", mean_pp)(mean_pp) )
pp_choro



#### 4. NON-SPATIAL EDA - created by Claudia ####

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

##### 4a. Continuous Variables - created by claudia ####


# Mean police perceptions - normaly distributed 

count(pp) # n = 106
rowMeans(pp[-1]) # returns 107 results ? 


ggplot(pp, aes(rowMeans(pp[-1]))) + 
  geom_histogram(aes(y =..density..), breaks = seq(60, 82, by = 1), ) + 
  stat_function(fun = dnorm, args = list(mean = mean(rowMeans(pp[-1])), sd = sd(rowMeans(pp[-1]))))


#### 4b. Categorical Variables - Crime stuff ####



# Examine categorical variables - NEEDS CATEGORICAL AGGREGATION!!


# Crime type: (25% Anti-social behaviour) (20% Violence and sexual offenSe)
x <- prop.table(table(crime@data$Crime.type))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


ggplot(data=crime@data, aes(x=Crime.type, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(crime@data$mean_pp, c(0, 0.97)))













# SS OVERVIEW statistics ----------------------------------------------------------

summary(ss_ag) # summary statistics for ss_occurance
#   ss_occurance   
#   Min.   :  1.00  
#   1st Qu.:  5.00  
#   Median : 11.00  
#   Mean   : 16.15  
#   3rd Qu.: 19.00  
#   Max.   :179.00  


# ss occurance histogram
ggplot(data=ss_ag, aes(ss_ag$ss_occurance)) +
  geom_histogram()+ 
  labs(title="Stop and Search occurances histogram",x="Occurance", y = "Count")


# SS -> EDA : AGE DATA -------------------------------------------------------------


# **** SS AGE DATA *** 

table(ss@data$Age.range) # original data with empty category
#        10-17    18-24    25-34  over 34 under 10 
# 398     1764     3389     2030     1472        1 


# Rename empty age bracket -> used https://www.marsja.se/how-to-rename-factor-levels-in-r-dplyr/#:~:text=How%20do%20I%20Rename%20Factor,%2C%20%22Factor%203%22)%20.
# renaming is very tempermental - but not sure if this is just my computer so have used all 3 methods 

# attempt 1 
levels(ss@data$Age.range) <- c("Unknown", "10-17", "18-24", "25-34", "over 34", "Under 10")


# Attempt 2  -
# levels(ss@data$Age.range)[levels(ss@data$Age.range)==""] <-"Unknown" 


# attempt 3:
# ss@data$Age.range <- recode_factor(ss@data$Age.range, " " = "NOT STATED")


levels(ss@data$Age.range) # this shows new category names 
table(ss@data$Age.range) # these new category names don't always update in the table ^
# hence the three methods in case one doesn't work



# bar chart - ss occurances per age bracket
x <- prop.table(table(ss@data$Age.range))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)



# boxplot -> ss ages vs mean police perception
ggplot(data=ss@data, aes(x=Age.range, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))




# SS -> EDA : SS (self defined) ETHNICITY DATA EDA ------------------------------------------------------------


# *** SS ETHNICITY DATA ***

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


# bar chart -> stop and search ethnicity (self)
x <- prop.table(table(ss@data$Self.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# box plot -> ss by self defined ethnicity vs mean police perception
ggplot(data=ss@data, aes(x=Self.defined.ethnicity, y=mean_pp), las =3) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))


# MEAN AND MEDIAN POLICE PERCEPTION BY (SELF DEFINED) SS ETHNICITY: 

# SS_WHITE 
ss_white <- subset(ss@data, ss@data$Self.defined.ethnicity == "White")
median(ss_white$mean_pp) # 70.74074
mean(ss_white$mean_pp) # 71.60952


# SS Black or Black British
ss_black <- subset(ss@data, ss@data$Self.defined.ethnicity == "Black or Black British")
median(ss_black$mean_pp)  # 70.59722
mean(ss_black$mean_pp) # 70.68117


# SS Asian or Asian British
ss_asian <- subset(ss@data, ss@data$Self.defined.ethnicity == "Asian or Asian British")
median(ss_asian$mean_pp)  # 70.40741
mean(ss_asian$mean_pp) # 70.35847


# SS Asian or Asian British
ss_chinese_other <- subset(ss@data, ss@data$Self.defined.ethnicity == "Chinese or other ethnic group")
median(ss_chinese_other$mean_pp)  # 70.59722
mean(ss_chinese_other$mean_pp) # 71.42339

# mixed
ss_Mixed <- subset(ss@data, ss@data$Self.defined.ethnicity == "Mixed")
median(ss_Mixed$mean_pp)  # 70.59722
mean(ss_Mixed$mean_pp) # 71.12612

# not stated
ss_not_stated <- subset(ss@data, ss@data$Self.defined.ethnicity == "Not Stated (NS)")
median(ss_not_stated$mean_pp)  # 70.74074
mean(ss_not_stated$mean_pp) # 71.69271



# SS -> EDA: OFFICER DEFINED ETHNICITY  ----------------------------------------------


# *** The ethnicity of the person stopped as defined by the officer ***


# REPLACE EMPTY WITH "NOT STATED" -> had real issues with this. functions seem to be tempermental. 
# one of the below options will achieve the desired end result.

# Attempt 1 -> didn't work last time 
levels(ss@data$Officer.defined.ethnicity)[levels(ss@data$Officer.defined.ethnicity)==""] <-"NOT STATED" 
table(ss@data$Officer.defined.ethnicity)

# attempt 2:
levels(ss@data$Officer.defined.ethnicity) <- c("Not Stated", "Asian", "Black", "Other", "White")
table(ss@data$Officer.defined.ethnicity)

# attmpt 3:
ss@data$Officer.defined.ethnicity <- recode_factor(ss@data$Officer.defined.ethnicity,
                                                   " " = "NOT STATED")
levels(ss@data$Officer.defined.ethnicity)
table(ss@data$Officer.defined.ethnicity)


# BAR CHART -> SS OCCURANCES PER (OFFICER DEFINED) ETHNICITY
x <- prop.table(table(ss@data$Officer.defined.ethnicity))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)


# BOX PLOT -> Mean Police perception vs officer defined ethnicity 
ggplot(data=ss@data, aes(x=Officer.defined.ethnicity, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))



# SS -> EDA: OBJECT OF SEARCH --------------------------------------------------------

# (>60% Controlled drugs) (~35% articles for use in criminal damage)

# Bar Chart -> object of search 
x <- prop.table(table(ss@data$Object.of.search))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)

# box plot : object of search vs mean pp 
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








# SS -> EDA: GENDER ---------------------------------------------------------------------


table(ss@data$Gender) # -> gives COUNT per gender / note the empty field with a count of 100 
#       Female   Male  Other 
#  100    568   8383      3 


table2 <- table(ss@data$Gender)
prop.table(table2) 
round(prop.table(table2) ,3)  # gives -> PROPORTIONS rounded to 3 dp 
#     Female   Male  Other 
#0.011  0.063  0.926  0.000 


# GENDER BAR CHART (Frequency) -> M / F/ O / "" 
x <- prop.table(table(ss@data$Gender))
par(fig=c(0,1,0.3,1), new=FALSE)
barplot(x[order(x, decreasing = TRUE)], ylab = "Frecuency (%)", las=2)



# make new dataframe getting rid of gender empties
updated_genders <- data.frame(ss@data[ss@data$Gender != "",])

table(updated_genders$Gender)
# Female   Male  Other 
#    568   8383      3 


# BOXPLOT -> M / F / O 
ggplot(data=updated_genders, aes(x=Gender, y=mean_pp)) + 
  geom_boxplot()+
  coord_cartesian(ylim = quantile(ss@data$mean_pp, c(0, 0.97)))



# POLICE PERCEPTIONS PER GENDER: 

# FEMALE only data
ss_female <- subset(ss@data, ss@data$Gender == "Female")
median(ss_female$mean_pp) # Median police perception when person stopped = F --> 70.74074
mean(ss_female$mean_pp) # Mean -> F ->  71.64513


# MALE ONLY DATA
ss_male <- subset(ss@data, ss@data$Gender == "Male")
median(ss_male$mean_pp) #70.59722
mean(ss_male$mean_pp) #71.09726






# SS -> Global Morans I (ss_occurance) ------------------------------------------


# ** Using ss_ward2 for all autocorrelation measures for continuity **

ttm()  # interactive viewing

# GSS CODE is individual for each ward ( - would use name but name is not individual)
tm_shape(ss_ward2)+tm_polygons()   # Plots all ss_ward polygons
tm_shape(ss_ward)+tm_polygons() 



# GLOBAL SPATIAL AUTOCORRELATION MEASURES -> MORANS I 
neighbours3 <- poly2nb(ss_ward2, row.names = ss_ward2@data$GSS_CODE) # create list of neighbours -> ward2
W3 <- nb2mat(neighbours3, style="W") # create spatial weight matrix
colnames(W3) <- rownames(W3)
ss_ward2$rowID <-  rownames(ss_ward2@data$GSS_CODE) 
tm_shape(ss_ward2)+tm_polygons()+tm_text(text="GSS_CODE") 

Wl3 <- nb2listw(neighbours3) # a listw object is a weights list for use in autocorrelation measures.
moran.test(ss_ward2@data$ss_occurance, Wl3, na.action=na.omit) # omit NA values

#Moran I statistic standard deviate = 11.571, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
# Moran I statistic       Expectation          Variance 
#   0.2767138176            -0.0016313214      0.0005786173 


# Interpret: 
# For the Global Moran's I statistic, the null hypothesis states that the attribute being analyzed is
# randomly distributed among the features in your study area; 
# When the p-value returned by this tool is statistically significant, you can reject the null hypothesis.



# Moran Test using Monte-Carlo simulation

moran.mc(ss_ward2@data$ss_occurance, Wl3, nsim=999, na.action=na.omit) 
#data:  ss_ward@data$ss_occurance 
# weights: Wl 
# omitted: 9, 20, 23, 24, 28, 40, 45, 54, 62, 149, 159, 276, 300, 306, 308, 312, 379, 386, 432, 449, 517, 576, 633, 635, 636, 637, 638, 639, 640, 641, 642, 643, 644, 645, 647, 649, 650, 651, 652, 653, 654, 655, 657 
# number of simulations + 1: 1000 

# statistic = 0.27671, observed rank = 1000, p-value = 0.001
# alternative hypothesis: greater













# SS -> Semi Variogram  ---------------------------------------------------


# stop and search semi variogram -> using stop and search occurrence per ward 
ss_semivar <- variogram(ss_ward2@data$ss_occurance~1, ss_ward2)

ss_var_fit <- fit.variogram(ss_semivar, vgm("Sph")) # models: "Exp", "Sph", "Gau", "Mat

plot(ss_semivar, ss_var_fit)

summary(ss_semivar) 


# SS -> local autocorrelation -> Local Moran scatterplot ---------------------------------------------------

# Morans scatterplot 

neighbours2 <- poly2nb(ss_ward2, row.names = ss_ward2@data$GSS_CODE) # create list of neighbours
Wl2 <- nb2listw(neighbours2)

moran.plot(ss_ward2@data$ss_occurance, Wl2, xlab="Stop and Search occurance", 
           ylab="Spatailly lagged SS occurance", labels=ss_ward2@data$DISTRICT)


# GETIS ORD G AND G* ------------------------------------------------------

# GETIS ORD -> G AND G* ARE DONE ON ARCGIS PRO -> look in notion for results 



# SS -> Local Morans I Statistic ------------------------------------------------


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



# his tutorial looks at P values (Pr) where the z score (spread) is < 0 
# -> aka local moran values that are not identical to the mean 

# ...Here we are looking at P values where the z score is not equal to the expected (E) score
# specifically -> unadjusted p values
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

































