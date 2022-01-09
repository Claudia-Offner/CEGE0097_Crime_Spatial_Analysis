# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL REGRESSION
# Module: CEGE0097
# Student no.: 20198829 [CLAUDIA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')

# CLAUDIA COMENTS:
# What is fairSS for PP_borough race datasets?
# Why are there missing values for PP?
# (NEED i.e.: ss_ward2_black, ss_ward2_asian, ss_ward2_white, ss_ward2_male, ss_ward2_female)
# (NEED i.e.:  crime_ward_antiBeh; crime_ward_offense; crime_ward_allTheft)
# Deal with warnings (pos/neg values & sp::proj4string)
## CREATE K NEAREST NEIGHBOURS FOR NEAREST POLICE STATION (Data cleaning)


# 4.0 Packages & Functions####


# install.packages("spatialreg")
library(spatialreg)

# Set tmap to interactive
tmap_mode("view")

# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# 4.1 LOAD CLEANED DATA --------------------------------------------------------

suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c('crime_ward', 
                           'ss_ward2',  
                           'pp_borough', 'pp_black_borough', 'pp_asian_borough', 'pp_white_borough', 
                           'borough', 'ward', 'proj',
                           'station_ward', 'pop_ward')])

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




# 4.2 ANALYSIS PRE-PROCESSING ####  
####  A. Police Perception: Merge variables of Interest (4) #### 
### goodjob, fair, listens, mean by ALL & race (black, asian, white)
### What is fairSS for PP_borough race datasets?
pp_df <- merge(pp_borough, pp_black_borough[ , c(1,6:ncol(pp_black_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_asian_borough[ , c(1,6:ncol(pp_asian_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_white_borough[ , c(1,6:ncol(pp_white_borough))], by='DISTRICT')
names(pp_df@data)[1] <- "BOROUGH" # change name of spatial delineation (col1) to match ward

####  B. Stop & Search: Merge variables of Interest (6) #### 
### ss_occurance by ALL, race (black, asian, white) and gender (male, female)
ss_df <- ss_ward2

####  C. Crime: Variables of Interest (4) #### 
### crime_occurance by ALL, type (anti-social behaviour, violence & sexual offense, all theft)
crime_df <- crime_ward

####  D. Combine All For Analysis #### 
reg_df <- merge(crime_df, ss_df, by='NAME')
reg_df <- merge(reg_df, station_ward, by='NAME')
reg_df <- merge(reg_df, pop_ward, by='NAME')
reg_df <- merge(reg_df, pp_df[ , c(1,6:ncol(pp_df))], by='BOROUGH')

# Remove missing data
sapply(reg_df@data, function(x) sum(is.na(x)))
missing <- reg_df@data[rowSums(is.na(reg_df@data)) > 0, ]
reg_df <- sp.na.omit(reg_df)

# Reorganize
reg_df@data <- reg_df@data[order(reg_df@data$DISTRICT), ]
rownames(reg_df@data) <- seq(length=nrow(reg_df@data))

# tm_shape(reg_df)+tm_polygons("crime_occurance", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("ss_occurance", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("police_station_occurance", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("Population_Density_km2_2013", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("PP_ALL_MEAN", palette="-RdBu", style="quantile")

# 4.3 LINEAR REGRESSION ####

# Dependent variable: The variable we are trying to predict.
# Independent variables: The variables we are using to predict the dependent variable. They are called independent variables because they are required to independent of one another. Strong correlation between two or more independent variables is referred to as multicollinearity and can lead to unstable parameter estimates, which limits the explanatory power of the models.
# Parameters: Parameters or coefficients are the weights of the regression model. Each independent variable has a parameter that is multiplied by its value to make a prediction.

# STOP AND SEARCH (outcome/dependent) ~ CRIME (independent)
ssc_lreg <- lm(ss_occurance~crime_occurance, data=reg_df@data)
summary(ssc_lreg)
# For every crime occurrence, there is 0.12 more stop and search (sig)
# Model accounts for 50% of data variance

# POLICE PERCEPTIONS (outcome/dependent) ~ CRIME (independent)
ppc_lreg <- lm(PP_ALL_MEAN~crime_occurance, data=reg_df@data)
summary(ppc_lreg)
# For every crime occurrence, there is -0.003 lower police perception (insig)
# Model accounts for 0.002 of data variance

# POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent)
ppss_lreg <- lm(PP_ALL_MEAN~ss_occurance, data=reg_df@data)
summary(ppss_lreg)
# For every stop & search occurrence, there is -0.002 lower stop and search (very insig)
# Model accounts for -0.002 of data variance

# 4.4 TESTING ERRORS FOR SPATIAL AUTOCORRELATION ####

# Create spatial weight matrix (without missing data)
# zero.policy handles empty neighbors - DO NOT REMOVE
reg_W <- nb2listw(poly2nb(reg_df, queen=T),  style="W", zero.policy=T) 

#### A. STOP AND SEARCH (dependent) ~ CRIME (independent) ####

# Linear model residuals
reg_df$ssc_lreg.res <- residuals(ssc_lreg)
tm_shape(reg_df)+tm_polygons("ssc_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ssc_lreg.res)) + geom_histogram(bins=30)

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ssc_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ssc_lreg, reg_W, zero.policy = TRUE) # significant auto-correlation present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ssc_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ssc_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant



#### B. POLICE PERCEPTIONS (dependent) ~ CRIME (independent) ####

# Linear model residuals
reg_df$ppc_lreg.res <- residuals(ppc_lreg)
tm_shape(reg_df)+tm_polygons("ppc_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ppc_lreg.res)) + geom_histogram(bins=30)

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ppc_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ppc_lreg, reg_W, zero.policy = TRUE) # significat autocorrelatoin present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ppc_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ppc_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant



#### C. POLICE PERCEPTIONS (dependent) ~ STOP AND SEARCH (independent) ) ####

# Linear model residuals
reg_df$ppss_lreg.res <- residuals(ppss_lreg)
tm_shape(reg_df)+tm_polygons("ppss_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ppss_lreg.res)) + geom_histogram(bins=30)

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ppss_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ppss_lreg, reg_W, zero.policy = TRUE) # significant autocorrelation present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ppss_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ppss_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant

# 4.5 SPATIAL REGRESSION ####

# Spatial Lag Model: assumes autocorrelation is present in the dependent variable
# Spatial Error Model: assumes autocorrelation is a result of some unobserved variable(s) and is present in the residuals of the model.
# Spatial Durbin Model: is a spatial lag model that assumes autocorrelation may be present in one or more independent variables, as well as the dependent variable
# ASSUMPTIONS: the level of autocorrelation is constant across the study area, such that it can be modelled using a single parameter.

# Error Testing indicates that a Spatial Error Model should be used for formulas A & C 
# B was not spatially (or linearly) significant for either so is not included

# STOP AND SEARCH (outcome/dependent) ~ CRIME (independent)
ssc_ESreg <- errorsarlm(ss_occurance~crime_occurance, data=reg_df@data, listw=reg_W)
summary(ssc_ESreg)
# For every crime occurrence, there is 0.12 more stop and search (sig)

reg_df$ssc_ESreg.res <- residuals(ssc_ESreg)
reg_df$ssc_ESreg.fit <- exp(fitted.values(ssc_ESreg))
# ggplot(data=reg_df@data, aes(ssc_ESreg.res)) + geom_histogram(bins=30)
# ggplot(data=reg_df@data, aes(ssc_ESreg.res)) + geom_qq() + geom_qq_line()
tm_shape(reg_df)+tm_polygons("ssc_ESreg.res", palette="-RdBu", style="quantile")
tm_shape(reg_df)+tm_polygons("ssc_ESreg.fit", style="quantile")
# CHECK: Is all spatial autocorelation accounted for?


# POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent)
ppss_ESreg <- errorsarlm(PP_ALL_MEAN~ss_occurance, data=reg_df@data, listw=reg_W)
summary(ppss_ESreg)
# For every stop & search occurrence, there is -0.01 lower police perception (marginally insig)

reg_df$ppss_ESreg.res <- residuals(ppss_ESreg)
reg_df$ppss_ESreg.fit <- exp(fitted.values(ppss_ESreg))
# ggplot(data=reg_df@data, aes(ppss_ESreg.res)) + geom_histogram(bins=30)
# ggplot(data=reg_df@data, aes(ppss_ESreg.res)) + geom_qq() + geom_qq_line()
tm_shape(reg_df)+tm_polygons("ppss_ESreg.res", palette="-RdBu", style="quantile")
tm_shape(reg_df)+tm_polygons("ppss_ESreg.fit", style="quantile")



#### Geographically Weighted Regression ####
# Geographically weighted regression (GWR) is a method for analysing spatially varying relationships (C. Brunsdon, Fotheringham, and Charlton 1998).
# The method attempts to model heterogeneity using geographically varying regression coefficients. 
# This enables maps of the coefficients to be produced, which can provide insights into how the relationship between the dependent and independent variables varies across space.

#### Regression Kriging ####
# Regression Kriging is an approach that combines Kriging interpolation with regression modelling. In simple terms, the values of a process at unknown locations are interpolated as the sum of a linear regression model and an interpolated residual


#### FINAL MODEL ####



x <- lm(ss_occurance~crime_occurance
        +BLACK_mean
        +ASIAN_mean
        +WHITE_mean
        +Mean_Popuation_Age_2013
        +Population_Density_km2_2013
        +Mortality_Ratio_2013
        +Life_Expectancy_2013
        +Median_House_Prices_2013
        +Mean_Household_Income_2013
        +Total_crime_rate_2013
        +Ethnic_Group_White_2013
        +Ethnic_Group_Asian_2013
        +Ethnic_Group_Black_2013, data=reg_df@data)

summary(x)

# Linear model residuals
reg_df$x.res <- residuals(x)
tm_shape(reg_df)+tm_polygons("x.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(x.res)) + geom_histogram(bins=30)

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=x.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(x, reg_W, zero.policy = TRUE) # significant auto-correlation present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(x, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(x, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant

# Do a durbin regression
