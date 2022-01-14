# Project: CRIME SPATIAL ANALYSIS
# File: SPATIAL EDA: CRIME
# Module: CEGE0097
# Student no.: [PRATIBHA]
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))

# Remove all environment objects except those of interest to this analysis
rm(list=ls()[! ls() %in% c("crime", "borough", "ward", "proj")])

#AUTOCORRELATION
      library(knitr)
      library(spdep)
      
      ttm()  # interactive viewing
      
      # GSS CODE is individual for each ward ( - would use name but name is not individual)
      tm_shape(crime_ward)+tm_polygons()   # Plots all ss_ward polygons
      
      neighbours <- poly2nb(crime_ward, row.names = crime_ward@data$GSS_CODE) # create list of neighbours
      W <- nb2mat(neighbours, style="W") # create spatial weight matrix
      colnames(W) <- rownames(W)
      crime_ward$rowID <-  rownames(crime_ward@data$GSS_CODE) 
      
      tm_shape(crime_ward)+tm_polygons()+tm_text(text="GSS_CODE") # plot polygons with GSS_CODE labels
      
      # check that neighbours is working properly:
      # WARDS E05011106 should have the following neighbours: E05011112 / E05009330 / E05011104 / E05011116
      nbrs <- which(W["E05011106",]>0)
      nbrs
      tm_shape(crime_ward[nbrs,])+tm_polygons()+tm_text(text="GSS_CODE") # 
      
      # GLOBAL SPATIAL AUTOCORRELATION MEASURES -> MORANS I 
      Wl <- nb2listw(neighbours) # a listw object is a weights list for use in autocorrelation measures.
      moran.test(crime_ward@data$crime_occurance, Wl, na.action=na.omit) # omit NA values
      
      # Moran I test under randomisation
      # data:  crime_ward@data$crime_occurance  
      # weights: Wl    
      
      # Moran I statistic standard deviate = 12.974, p-value < 2.2e-16
      # alternative hypothesis: greater
      # sample estimates:
      # Moran I statistic : 0.2924911825 
      # Expectation -0.0015408320 
      # Variance 0.0005136137 
      
      # Moran Test using Monte-Carlo simulation
      moran.mc(crime_ward@data$crime_occurance, Wl, nsim=999, na.action=na.omit) 
      
      # Monte-Carlo simulation of
      #Moran I
      #data:  crime_ward@data$crime_occurance 
      #weights: Wl  
      #number of simulations + 1: 1000 
      #statistic = 0.29249, observed
      #rank = 1000, p-value = 0.001
      #alternative hypothesis: greater
      
      library(gstat)
      
      crime_semivar <- variogram(crime_ward_2@data$crime_occurance~1, crime_ward_2)
      crime_var_fit <- fit.variogram(crime_semivar, vgm("Sph")) # models: "Exp", "Sph", "Gau", "Mat
      
      plot(crime_semivar, crime_var_fit)
      
      summary(crime_semivar) 
      
      neighbours2 <- poly2nb(crime_ward_2, row.names = crime_ward_2@data$GSS_CODE) # create list of neighbours
      Wl2 <- nb2listw(neighbours2)
      
      moran.plot(crime_ward_2@data$crime_occurance, Wl2, xlab="Crime occurance", 
                 ylab="Spatailly lagged crime occurance", labels=crime_ward_2@data$DISTRICT)
      
     #  GETIS ORD -> G AND G* not working
     # Thinkng of doing in R. 
