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

# 4.0 Functions & Settings ####

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
                           'borough', 'ward', 'proj')])

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

####  B. Stop & Search: Merge variables of Interest (6) #### 
### ss_occurance by ALL, race (black, asian, white) and gender (male, female)
ss_df <- ss_ward2

####  C. Crime: Variables of Interest (4) #### 
### crime_occurance by ALL, type (anti-social behaviour, violence & sexual offense, all theft)
crime_df <- crime_ward

####  D. Combine All For Analysis #### 
reg_df <- merge(crime_df, ss_df, by='NAME')
reg_df <- merge(reg_df, pp_df[ , c(1,6:ncol(pp_df))], by='DISTRICT')

# Remove missing data
sapply(reg_df@data, function(x) sum(is.na(x)))
missing <- crime_df@data[rowSums(is.na(crime_df@data)) > 0, ]
reg_df <- sp.na.omit(reg_df)

# Reorganize
reg_df@data <- reg_df@data[order(reg_df@data$DISTRICT), ]
rownames(reg_df@data) <- seq(length=nrow(reg_df@data))

# tm_shape(reg_df)+tm_polygons("ss_occurance", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("PP_ALL_MEAN", palette="-RdBu", style="quantile")
# tm_shape(reg_df)+tm_polygons("crime_occurance", palette="-RdBu", style="quantile")

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
ppc_lreg <- lm(PP_ALL_MEAN~crime_occurance, data=reg_df@data, na.action=na.exclude)
summary(ppc_lreg)
# For every crime occurrence, there is -0.003 lower police perception (insig)
# Model accounts for 0.002 of data variance

# POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent)
ppss_lreg <- lm(PP_ALL_MEAN~ss_occurance, data=reg_df@data, na.action=na.exclude)
summary(ppss_lreg)
# For every stop & search occurrence, there is -0.002 lower stop and search (very insig)
# Model accounts for -0.002 of data variance

# 4.4 TESTING ERRORS FOR SPATIAL AUTOCORRELATION ####

# Create spatial weight matrix (without missing data)
# zero.policy handles empty neighbors - DO NOT REMOVE
reg_W <- nb2listw(poly2nb(reg_df, queen=T),  style="W", zero.policy=T) 

#### A. STOP AND SEARCH (outcome/dependent) ~ CRIME (independent) ####

# Linear model residuals
reg_df$ssc_lreg.res <- residuals(ssc_lreg)
tm_shape(reg_df)+tm_polygons("ssc_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ssc_lreg.res)) + geom_histogram()

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ssc_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ssc_lreg, reg_W, zero.policy = TRUE) # significant auto-correlation present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ssc_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ssc_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant



#### B. POLICE PERCEPTIONS (outcome/dependent) ~ CRIME (independent) ####

# Linear model residuals
reg_df$ppc_lreg.res <- residuals(ppc_lreg)
tm_shape(reg_df)+tm_polygons("ppc_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ppc_lreg.res)) + geom_histogram()

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ppc_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ppc_lreg, reg_W, zero.policy = TRUE) # significat autocorrelatoin present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ppc_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ppc_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant



#### C. POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent) ) ####

# Linear model residuals
reg_df$ppss_lreg.res <- residuals(ppss_lreg)
tm_shape(reg_df)+tm_polygons("ppss_lreg.res", palette="-RdBu", style="quantile")

# Check distribution for normality
ggplot(data=reg_df@data, aes(ppss_lreg.res)) + geom_histogram()

# QQ-plot of linear model residuals
ggplot(data=reg_df@data, aes(sample=ppss_lreg.res)) + geom_qq() + geom_qq_line()

# Residal Autocorrelation
lm.morantest(ppss_lreg, reg_W, zero.policy = TRUE) # significat autocorrelatoin present

# Identify type of Autocorrelatoin (is spatial error or spatial lag model better?)
lm.LMtests(ppss_lreg, reg_W, test="RLMlag", zero.policy = TRUE) # lag autocorrelation insignificant
lm.LMtests(ppss_lreg, reg_W, test="RLMerr", zero.policy = TRUE) # error autocorrelation significant





# rownames(missing)
# reg_df <- reg_df[!(rownames(reg_df@data) %in% rownames(missing)),]
# reg_df@data <- na.omit(reg_df@data)
# x <- reg_df[reg_df$DISTRICT != 'City of Westminster']
# colnames(reg_df@data)
