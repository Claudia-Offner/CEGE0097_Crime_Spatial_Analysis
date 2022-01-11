# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL EDA: STOP & SEARCH
# Module: CEGE0097
# Student no.: [DANNI]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c("ss", "borough", "ward", "proj")])




#  # SS -> Global Morans I (ss_occurance) ------------------------------------------


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

































