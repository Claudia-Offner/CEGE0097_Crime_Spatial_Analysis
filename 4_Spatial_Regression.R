# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL REGRESSION
# Module: CEGE0097
# Student no.: 20198829 [CLAUDIA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/CEGE0097_Crime_Spatial_Analysis')

# 4.0 Packages & Functions####

# install.packages("spatialreg")
# install.packages("corrplot")
library(spatialreg)
library(corrplot)
library(ggfortify)

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

# 4.1 LOAD & PROCESS DATA --------------------------------------------------------

suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c('crime_ward', 'crime_ward_asb', 'pp_ag_borough_shp',
                           'ss_ward2','ss_ward2_asian', 'ss_ward2_chinese_other', 'ss_ward2_white', 'ss_ward2_black', 'ss_ward2_mixed', 'ss_ward2_female', 'ss_ward2_male',
                           'pp_borough', 'pp_black_borough', 'pp_asian_borough', 'pp_white_borough', 
                           'borough', 'ward', 'proj',
                           'station_ward', 'pop_ward')])

# Make PP lists Spatial objects
pp_borough <- pp_borough[[1]]
pp_black_borough <- pp_black_borough[[1]]
pp_asian_borough <- pp_asian_borough[[1]]
pp_white_borough <- pp_white_borough[[1]]
pp_borough <- merge(borough, pp_borough[ , c(1,6:ncol(pp_borough))], by='DISTRICT')
pp_black_borough <- merge(borough, pp_black_borough[ , c(1,6:ncol(pp_black_borough))], by='DISTRICT')
pp_asian_borough <- merge(borough, pp_asian_borough[ , c(1,6:ncol(pp_asian_borough))], by='DISTRICT')
pp_white_borough <- merge(borough, pp_white_borough[ , c(1,6:ncol(pp_white_borough))], by='DISTRICT')

####  A. Police Perception: Merge variables of Interest (4F) #### 
### goodjob, fair, listens, mean by ALL & race (black, asian, white)
### What is fairSS for PP_borough race datasets?
pp_df <- merge(pp_borough, pp_black_borough[ , c(1,6:ncol(pp_black_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_asian_borough[ , c(1,6:ncol(pp_asian_borough))], by='DISTRICT')
pp_df <- merge(pp_df, pp_white_borough[ , c(1,6:ncol(pp_white_borough))], by='DISTRICT')
names(pp_df@data)[1] <- "BOROUGH" # change name of spatial delineation (col1) to match ward

####  B. Stop & Search: Merge variables of Interest (6) #### 
### ss_occurance by ALL, race (black, asian, white)

ss_df <- merge(ss_ward2, ss_ward2_white, by='NAME')
ss_df <- merge(ss_df, ss_ward2_black, by='NAME')
ss_df <- merge(ss_df, ss_ward2_asian, by='NAME')
ss_df <- merge(ss_df, ss_ward2_chinese_other, by='NAME')
ss_df <- merge(ss_df, ss_ward2_mixed, by='NAME')
ss_df@data[is.na(ss_df@data)] <- 0


####  C. Crime: Variables of Interest (4) #### 
### crime_occurance by ALL, type (anti-social behaviour)
crime_df <- merge(crime_ward, crime_ward_asb, by='NAME')
crime_df@data[is.na(crime_df@data)] <- 0


####  D. Combine All Datasets For Analysis #### 
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

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c('crime_df', 'ss_df', 'pp_df', 'pop_ward', 'station_ward', 'reg_df', 'borough', 'ward', 'proj')])





# 4.2 LINEAR REGRESSION ####

# Dependent variable: The variable we are trying to predict.
# Independent variables: The variables we are using to predict the dependent variable. They are called independent variables because they are required to independent of one another. Strong correlation between two or more independent variables is referred to as multicollinearity and can lead to unstable parameter estimates, which limits the explanatory power of the models.
# Parameters: Parameters or coefficients are the weights of the regression model. Each independent variable has a parameter that is multiplied by its value to make a prediction.

#### Crude Associations ####
# Test the crude associations that make theoretical sense and check their adjusted-r squares

# STOP AND SEARCH (outcome/dependent) ~ CRIME (independent)
ssc_crude <- lm(ss_occurance_ALL~crime_occurance_ALL, data=reg_df@data)
summary(ssc_crude)
# For every crime occurrence, there is 0.12 more stop and search (sig)
# Model accounts for 50% of data variance - INTERESTING

# POLICE PERCEPTIONS (outcome/dependent) ~ CRIME (independent)
ppc_crude <- lm(PP_ALL_MEAN~crime_occurance_ALL, data=reg_df@data)
summary(ppc_crude)
# Model accounts for 0.002 of data variance - NOT OF INTEREST

# POLICE PERCEPTIONS (outcome/dependent) ~ STOP AND SEARCH (independent)
ppss_crude <- lm(PP_ALL_MEAN~ss_occurance_ALL, data=reg_df@data)
summary(ppss_crude)
# Model accounts for -0.002 of data variance - NOT OF INTEREST

#### Intra-variable Associations ####

# Check correlations of all variables of interest
select <- c("ss_occurance_ALL","ss_occurance_white","ss_occurance_black","ss_occurance_asian",
            'crime_occurance_ALL', 'crime_occurance_asb',
            'ALL_mean', 'BLACK_mean', 'ASIAN_mean', 'WHITE_mean', 'WHITE_fairSS', 'BLACK_fairSS', 'ASIAN_fairSS',
            'Mean_Popuation_Age_2013', 'Population_Density_km2_2013', 'Mortality_Ratio_2013', 'Life_Expectancy_2013', 'Median_House_Prices_2013',
            'Mean_Household_Income_2013', 'Total_crime_rate_2013','Ethnic_Group_White_2013', 'Ethnic_Group_Asian_2013', 'Ethnic_Group_Black_2013',
            'police_station_occurance')
reg_cor <- cor(reg_df@data[select])
palette = colorRampPalette(c("red", "white", "blue")) (20)
heatmap(x = reg_cor, col = palette, symm = TRUE)

# Identify which variables have significant associations with ss_occurance_ALL and crime_occurance_ALL

# Effect Modifiers: SS ethnicity and crime type 
# Potential confounders:  
# WHITE_mean, WHITE_fairSS, BLACK_fairSS, ASIAN_fairSS, Mean_Popuation_Age_2013, 
# Population_Density_km2_2013, Mortality_Ratio_2013, Life_Expectancy_2013,
# Median_House_Prices_2013, Total_crime_rate_2013, 
# Ethnic_Group_Black_2013, police_station_occurance

#### Model Building ####

# Add each variable one at a time to check confounding effects against crude association

# Check all potential confounders 
lm_model <- lm(ss_occurance_ALL~crime_occurance_ALL,
               # RUN: different S&S outcomes
               # +ss_occurance_white
               # +ss_occurance_black
               # +ss_occurance_asian
               # +crime_occurance_asb
               # CONFOUNDERS
               # +WHITE_mean,  # CONFOUNDER
               # +WHITE_fairSS,  # CONFOUNDER
               # +BLACK_fairSS,  # CONFOUNDER
               # +ASIAN_fairSS,  # CONFOUNDER
               # +Mean_Popuation_Age_2013,  # CONFOUNDER
               # +Population_Density_km2_2013,  # CONFOUNDER
               # +Mortality_Ratio_2013,  # no change from crude - REMOVE
               # +Life_Expectancy_2013,  # no change from crude - REMOVE
               # +Median_House_Prices_2013, # CONFOUNDER 
               # +Total_crime_rate_2013,  # CONFOUNDER 
               # +Ethnic_Group_Black_2013,   # CONFOUNDER 
               # CONTROLS: Police Stations
               # +police_station_occurance,  # no change from crude - REMOVE
               data=reg_df@data)
        
summary(lm_model)

#### Final Linear Model ####

# Check all potentially influential variables and select those of moderate- highly significant (p=<0.10)
lm_model <- lm(ss_occurance_ALL~crime_occurance_ALL
               # RUN: different S&S outcomes
               # ss_occurance_ALL
               # ss_occurance_white
               # ss_occurance_black
               # ss_occurance_asian
               # CONFOUNDERS: Police perception
               +WHITE_mean  
               +WHITE_fairSS
               +BLACK_fairSS
               +ASIAN_fairSS
               # CONFOUNDERS: Population
               +Mean_Popuation_Age_2013
               +Population_Density_km2_2013
               +Median_House_Prices_2013
               +Total_crime_rate_2013
               +Ethnic_Group_Black_2013,
               data=reg_df@data)

summary(lm_model)

# Calculate Mean Square Error (MSE) for comparisons
mean(lm_model$residuals^2) #170.7308

# 4.3 TESTING ERRORS FOR SPATIAL AUTOCORRELATION ####

# Create spatial weight matrix (without missing data)
reg_W <- nb2listw(poly2nb(reg_df, queen=T),  style="W", zero.policy=T) 

# Check Assumptions: fitted residuals, QQ-plot, scale-location, residual leverage
autoplot(lm_model)

# Residuals vs Fitted: Used to check the linear relationship assumptions. 
  # A horizontal line, without distinct patterns is an indication for a linear relationship, which is good.
# Normal Q-Q: Used to examine whether the residuals are normally distributed. 
  # It’s good if residuals points follow the straight dashed line.
# Scale-Location: Used to check the homogeneity of variance of the residuals (homoscedasticity). 
  # Horizontal line with equally spread points is a good indication of homoscedasticity. This is not the case in our example, where we have a heteroscedasticity problem.
# Residuals vs Leverage: Used to identify influential cases, that is extreme values that might influence 
  # the regression results when included or excluded from the analysis. 

# Plot model residuals
reg_df$lm_res <- residuals(lm_model)

tm_shape(reg_df)+tm_polygons("lm_res", palette="-RdBu", title="Linear Residuals (SS - All)", style="quantile")

# RESULT: Assumptions are violated, but this could be due to spatial components

# Residual Autocorrelation
lm.morantest(lm_model, reg_W, zero.policy = TRUE) # significant auto-correlation present

# Identify type of Autocorrelation (is spatial error or spatial lag model better?)
lm.LMtests(lm_model, reg_W, test="RLMlag", zero.policy = TRUE) # lag Autocorrelation significant
lm.LMtests(lm_model, reg_W, test="RLMerr", zero.policy = TRUE) # error Autocorrelation significant
# Requires a Durbin spatial regression


# 4.4 SPATIAL REGRESSION ####

# Spatial Lag Model: assumes autocorrelation is present in the dependent variable
# Spatial Error Model: assumes autocorrelation is a result of some unobserved variable(s) and is present in the residuals of the model.
# Spatial Durbin Model: is a spatial lag model that assumes autocorrelation may be present in one or more independent variables, as well as the dependent variable
# ASSUMPTIONS: the level of autocorrelation is constant across the study area, such that it can be modelled using a single parameter.


durbin_model <- lagsarlm(ss_occurance_ALL~crime_occurance_ALL
                         # RUN: different S&S outcomes
                         # ss_occurance_ALL
                         # ss_occurance_white
                         # ss_occurance_black
                         # ss_occurance_asian
                         # CONFOUNDERS: Police perception
                         +WHITE_mean  
                         +WHITE_fairSS
                         +BLACK_fairSS
                         +ASIAN_fairSS
                         # CONFOUNDERS: Population
                         +Mean_Popuation_Age_2013
                         +Population_Density_km2_2013
                         +Median_House_Prices_2013
                         +Total_crime_rate_2013
                         +Ethnic_Group_Black_2013,
                         data=reg_df@data,
                         listw=reg_W,
                         type='mixed')

summary(durbin_model, Nagelkerke=TRUE)

# Calculate Mean Square Error (MSE) for comparisons
mean(durbin_model$residuals^2) # 156.6166

# Spatial Regression Comparison
reg_df@data$durbin_res <- residuals(durbin_model)
reg_df@data$durbin_fit <- predict(durbin_model)

tm_shape(reg_df)+
  tm_polygons("durbin_res", title = "Durbin Residuals (SS - All)", palette="-RdBu", style="quantile")

t1 <- tm_shape(reg_df)+
      tm_polygons("ss_occurance_ALL", title = "Actual SS Occurance (All)", palette="-RdBu", style="quantile")
t2 <- tm_shape(reg_df)+
      tm_polygons("durbin_fit", title = "Durbin SS Predicted (All)", palette="-RdBu", style="quantile")
tmap_arrange(t1,t2)


#### A. SS_White & Crime ####

durbin_model_w <- lagsarlm(ss_occurance_white~crime_occurance_ALL
                         # CONFOUNDERS: Police perception
                         +WHITE_mean  
                         +WHITE_fairSS
                         +BLACK_fairSS
                         +ASIAN_fairSS
                         # CONFOUNDERS: Population
                         +Mean_Popuation_Age_2013
                         +Population_Density_km2_2013
                         +Median_House_Prices_2013
                         +Total_crime_rate_2013
                         +Ethnic_Group_Black_2013,
                         data=reg_df@data,
                         listw=reg_W,
                         type='mixed')

summary(durbin_model_w, Nagelkerke=TRUE)

# Calculate Mean Square Error (MSE) for comparisons
mean(durbin_model_w$residuals^2) # 32.1708

# Spatial Regression Comparison
reg_df@data$durbinw_res <- residuals(durbin_model_w)
reg_df@data$durbinw_fit <- predict(durbin_model_w)

tm_shape(reg_df)+
  tm_polygons("durbinw_res", title = "Durbin Residuals (SS - White)", palette="-RdBu", style="quantile")

t1 <- tm_shape(reg_df)+
      tm_polygons("ss_occurance_white", title = "Actual SS Occurance (White)", palette="-RdBu", style="quantile")
t2 <- tm_shape(reg_df)+
      tm_polygons("durbinw_fit", title = "Durbin SS Predicted (White)", palette="-RdBu", style="quantile")
tmap_arrange(t1,t2)


#### B. SS_Black & Crime ####

durbin_model_b <- lagsarlm(ss_occurance_black~crime_occurance_ALL
                           # CONFOUNDERS: Police perception
                           +WHITE_mean  
                           +WHITE_fairSS
                           +BLACK_fairSS
                           +ASIAN_fairSS
                           # CONFOUNDERS: Population
                           +Mean_Popuation_Age_2013
                           +Population_Density_km2_2013
                           +Median_House_Prices_2013
                           +Total_crime_rate_2013
                           +Ethnic_Group_Black_2013,
                           data=reg_df@data,
                           listw=reg_W,
                           type='mixed')

summary(durbin_model_b, Nagelkerke=TRUE)

# Calculate Mean Square Error (MSE) for comparisons
mean(durbin_model_b$residuals^2) # 39.71099


# Spatial Regression Comparison
reg_df@data$durbinb_res <- residuals(durbin_model_b)
reg_df@data$durbinb_fit <- predict(durbin_model_b)

tm_shape(reg_df)+
  tm_polygons("durbinb_res", title = "Durbin Residuals (SS - Black)", palette="-RdBu", style="quantile")

t1 <- tm_shape(reg_df)+
      tm_polygons("ss_occurance_black", title = "Actual SS Occurance (Black)", palette="-RdBu", style="quantile")
t2 <- tm_shape(reg_df)+
      tm_polygons("durbinb_fit", title = "Durbin SS Predicted (Black)", palette="-RdBu", style="quantile")
tmap_arrange(t1,t2)


#### C. SS_Asian & Crime ####

durbin_model_a <- lagsarlm(ss_occurance_asian~crime_occurance_ALL
                           # CONFOUNDERS: Police perception
                           +WHITE_mean  
                           +WHITE_fairSS
                           +BLACK_fairSS
                           +ASIAN_fairSS
                           # CONFOUNDERS: Population
                           +WHITE_mean  # pp
                           +Mean_Popuation_Age_2013
                           +Population_Density_km2_2013
                           +Median_House_Prices_2013
                           +Total_crime_rate_2013
                           +Ethnic_Group_Black_2013,
                           data=reg_df@data,
                           listw=reg_W,
                           type='mixed')

summary(durbin_model_a, Nagelkerke=TRUE)

# Calculate Mean Square Error (MSE) for comparisons
mean(durbin_model_a$residuals^2) # 6.664985



# Spatial Regression Comparison
reg_df@data$durbina_res <- residuals(durbin_model_a)
reg_df@data$durbina_fit <- predict(durbin_model_a)

tm_shape(reg_df)+
  tm_polygons("durbina_res", title = "Durbin Residuals (SS - Asian)", palette="-RdBu", style="quantile")+
  tm_legend(outside=TRUE)

t1 <- tm_shape(reg_df)+
      tm_polygons("ss_occurance_asian", title = "Actual SS Occurance (Asian)", palette="-RdBu", style="quantile")+
      tm_legend(outside=TRUE)

t2 <- tm_shape(reg_df)+
      tm_polygons("durbina_fit", title = "Durbin SS Predicted (Asian)", palette="-RdBu", style="quantile")+
      tm_legend(outside=TRUE)

tmap_arrange(t1,t2)
