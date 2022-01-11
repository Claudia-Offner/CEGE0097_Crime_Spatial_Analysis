# Project: CRIME SPATIAL ANALYSIS
# File: NON-SPATIAL EDA / VISUALS
# Module: CEGE0097
# Student no.: 20198829, 20164326 ...
# Date: 22 JANUARY 2022

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/crime_spatial_analysis/')

#### 2.1 LOAD CLEANED DATA ####
suppressWarnings(source("1_Data_cleaning.R"))



#### 2.2a Police Perceptions #### 

# TOMMY: Add your NON-SPATIAL / VISUAL bits here


#### 2.2b Stop and Search #### 
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



# POLICE PERCEPTIONS PER GENDER: - probably should ignore s

# FEMALE only data
ss_female <- subset(ss@data, ss@data$Gender == "Female")
median(ss_female$mean_pp) # Median police perception when person stopped = F --> 70.74074
mean(ss_female$mean_pp) # Mean -> F ->  71.64513


# MALE ONLY DATA
ss_male <- subset(ss@data, ss@data$Gender == "Male")
median(ss_male$mean_pp) #70.59722
mean(ss_male$mean_pp) #71.09726










#### 2.2c Crime #### 

# PRATIBHA: Add your NON-SPATIAL / VISUAL bits here