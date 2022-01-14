# CRIME SPATIAL ANALYSIS
# Module: CEGE0097
# Student no.: 20198829, 20164326, ... , ...
# Date: 22 January 2022
# subtitle: Ethnicity & Police Perception


#_______________________________________________________________________________
# 0. SETUP
#_______________________________________________________________________________

# set environment
setwd('C:\\Users\\Tommy\\OneDrive - University College London\\Modules-Notebooks\\CEGE0097_Geocomputation\\Assignment')

webshot::install_phantomjs()

# install.packages("pacman")# INSTALL THIS TO ACCESS ALL OTHER MODULES
pacman::p_load(tmap,        # for creating thematic maps
               ggplot2,     # for creating plots from a dataframe 
               sp,          # for accessing classes/methods that handle spatial data
               rgdal,       # for reading spatial data in various formats via GDAL 
               tidyverse,   # collection of packages that work in harmony to model, transform & visualise data
               leaflet,     # for creating interactive 'web' maps in the console
               spatialEco,  # for spatial data manipulation, querying, sampling & modelling.
               dplyr,       # for efficient manipulation of dataframe like objects
               plyr,
               grid,
               gridExtra,
               forcats,
               knitr,
               viridis,
               hrbrthemes,
               maptools,
               rgeos,
               ggthemes,
               leaflet.extras2,
               htmlwidgets, 
               htmltools,
               mapview,
               spdep)       # for creating spatial weight matrices)           

#_______________________________________________________________________________
# 1. LOADING DATA
#_______________________________________________________________________________

# load csv files
ss <- read.csv(file='Data/2016-06-metropolitan-stop-and-search.csv')  # read csv and assign to var
crime <- read.csv(file='Data/2016-06-metropolitan-street.csv')
pp <- read.csv(file='Data/2016-police-perceptions.csv')
pp_wab <- read.csv(file='Data/2015-16 _to_2020-21_inclusive_neighbourhood_indicators_final_221221.csv') # wab = white, asian, black
ethnicity_by_borough <- read.csv(file='Data/ethnic-groups-by-borough.csv') # https://data.london.gov.uk/dataset/ethnic-groups-borough

# load shapefiles
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # sets projection var using proj4string of params
ward <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
ward <- spTransform(ward, CRS(proj))
borough <- readOGR(dsn="Data/London_Shapefiles/London_Borough_Excluding_MHW.shp") # reads OGR data into vector object
borough <- spTransform(borough, CRS(proj)) # transforms 'borough' to WGS84 proj
names(borough@data)[1] <- "DISTRICT"  # change name of spatial delineation (col1) to match ward
#neighbourhood <- readOGR(dsn="Data/London_Shapefiles/London_Ward.shp")
#neighbourhood <- spTransform(ward, CRS(proj))

#_______________________________________________________________________________
# 2. PREPROCESSING DATA
#_______________________________________________________________________________

## 2.1 GENERAL CLEANING ------------------------------------------------------->

### Check for missing values
sapply(pp, function(x) sum(is.na(x)))
sapply(pp_wab, function(x) sum(is.na(x)))
sapply(ethnicity_by_borough, function(x) sum(is.na(x)))

### create subsets and drop missing/irrelevant fields
crime <- subset (crime, select = -c(Crime.ID, Context, Reported.by, Falls.within, Location, LSOA.code, LSOA.name, Last.outcome.category))
ss <- subset(ss, select = -c(Part.of.a.policing.operation, Type, Legislation, Policing.operation, Removal.of.more.than.just.outer.clothing, Outcome.linked.to.object.of.search))
borough@data <- subset(borough@data, select = -c(SUB_2006, SUB_2009))

### drop NA values
crime <- na.omit(crime)
ss <- na.omit(ss)
pp <- na.omit(pp)
pp_wab <- na.omit(pp_wab)
ethnicity_by_borough <- na.omit(ethnicity_by_borough)

### Add unique id's
ss$ID <- seq_along(ss[,1])
crime$ID <- seq_along(crime[,1])

## 2.2. PREPROCESSING ETHNIC GROUPS PER BOROUGH ------------------------------->

## 2.3. PREPROCESSING POLICE PERCEPTION (PP) ---------------------------------->

### drop missing/irrelevant fields
pp <- pp[-1,]                 # subheading row
pp_wab <- pp_wab[-c(1, 2), ]  # subheading row

### remove characters from 'grouping'
pp$Group<-gsub("Group","",as.character(pp$Group))

### select subsets
pp <- subset(pp, select = c(1,3,4,7,8)) # selects only good-job, fair and listens indicators
pp <- pp[ , c(1,2,4,5,3)]               # reorder cols to neighbour, fair, listens, good-job

### summarise indicator names (e.g. listens <- "The police in your area listen to the concerns of local people")
names(pp) <- c('neighbourhood', 'grouping', 
                   'fair_all', 'listens_all', 'goodjob_all')
names(pp_wab) <- c('neighbourhood', 
                   'fair_w', 'listens_w', 'goodjob_w', 'fairSS_w', 
                   'fair_a', 'listens_a', 'goodjob_a', 'fairSS_a', 
                   'fair_b', 'listens_b', 'goodjob_b', 'fairSS_b')

### amalgamate pp and pp_wab datasets
pp <- merge(pp, pp_wab, by='neighbourhood')
pp <- pp[ , c('neighbourhood','grouping',
              'goodjob_w','goodjob_a','goodjob_b', 'goodjob_all',
              'listens_w', 'listens_a', 'listens_b', 'listens_all',
              'fair_w', 'fair_a', 'fair_b', 'fair_all',
              'fairSS_w', 'fairSS_a', 'fairSS_b')] 

### change fields to numeric where necessary
names <- colnames(pp)[-1]
pp[ ,names] <- apply(pp[ , names], 2, function(x) as.numeric(as.character(x)))  # characters to numeric

## 2.4. SCALE1: NEIGHBOURHOOD LEVEL ------------------------------------------->


## 2.5. SCALE2: BOROUGH LEVEL ------------------------------------------------->

### foo: select borough name within neighbourhood name

names <- pp$neighbourhood                         # get neighbourhood names
names <- sub(" -.*", "", names)                   # get district name from neighbourhood name
pp <- add_column(pp, DISTRICT = '', .before = 1)  # create placeholder
pp$DISTRICT <- c(names)                           # populate placeholder

### identify boroughs where 25% of neighbourhoods are NA
na_per_borough <- setNames(aggregate(goodjob_w ~ DISTRICT, data=pp, function(x) {sum(is.na(x))}, na.action = NULL), c("DISTRICT", "na_count"))
nbr_per_borough <- setNames(aggregate(pp$DISTRICT, by=list(pp$DISTRICT), FUN=length), c("DISTRICT", "nbr_count"))
na_nbr <- merge(nbr_per_borough, na_per_borough, by='DISTRICT')
na_nbr$perc_na <- with(na_nbr, na_count / nbr_count * 100)
na_nbr <- na_nbr[!(na_nbr$perc_na<25.1),]
omit_boroughs <- list(na_nbr$DISTRICT)  # res: Bexley, Kingston upon Thames, Merton, Richmond upon Thames

### delete rows that contain NA
pp <- na.omit(pp) 

### get mean of all indicator scores per borough (for exploring variation in eda)
pp$mean_w <- round(rowMeans(pp[, c('goodjob_w', 'listens_w', 'fair_w', 'fairSS_w')]), 1)
pp$mean_a <- round(rowMeans(pp[, c('goodjob_a', 'listens_a', 'fair_a', 'fairSS_a')]), 1)
pp$mean_b <- round(rowMeans(pp[, c('goodjob_b', 'listens_b', 'fair_b', 'fairSS_b')]), 1)
pp$mean_all <- round(rowMeans(pp[, c('goodjob_all', 'listens_all', 'fair_all')]), 1)

### aggregate and merge pp to borough shapefile
names <- colnames(pp)[-c(1,2,3)]
pp_ag_borough <- pp %>%
  group_by(DISTRICT) %>% 
  summarise_at(vars(names), mean)
pp_ag_borough <- rapply(pp_ag_borough, f = round, classes = "numeric", how = "replace", digits = 1) # round all vals to 1 d.p.
pp_ag_borough <- pp_ag_borough[!pp_ag_borough$DISTRICT %in% omit_boroughs[[1]], ]                   # omit boroughs where over 25% it's neighbourhoods were NA

### calculate and append ethnicity population rank per borough
names(ethnicity_by_borough)[names(ethnicity_by_borough) == "ï..DISTRICT"] <- "DISTRICT"         # remove special characters
ethnicity_by_borough <- ethnicity_by_borough[-c(6)]                                             # remove 'other' category
ethnicity_by_borough$White <- as.numeric(gsub(",","",ethnicity_by_borough$White))               # remove commas and convert to numeric field
ethnicity_by_borough$Asian <- as.numeric(gsub(",","",ethnicity_by_borough$Asian))
ethnicity_by_borough$Black <- as.numeric(gsub(",","",ethnicity_by_borough$Black))
ethnicity_by_borough$Total <- as.numeric(gsub(",","",ethnicity_by_borough$Total))
ethnicity_by_borough$perc_w <- with(ethnicity_by_borough, White / Total * 100)                  # calculate percentage of overall population
ethnicity_by_borough$perc_a <- with(ethnicity_by_borough, Asian / Total * 100)
ethnicity_by_borough$perc_b <- with(ethnicity_by_borough, Black / Total * 100)
ethnicity_by_borough$rank_w[order(-ethnicity_by_borough$White)] <- 1:nrow(ethnicity_by_borough) # rank percentage
ethnicity_by_borough$rank_a[order(-ethnicity_by_borough$Asian)] <- 1:nrow(ethnicity_by_borough)
ethnicity_by_borough$rank_b[order(-ethnicity_by_borough$Black)] <- 1:nrow(ethnicity_by_borough)
ethnicity_by_borough <- ethnicity_by_borough[c('DISTRICT', 'perc_w','perc_a', 'perc_b', 'rank_w','rank_a', 'rank_b')]        # retain only rank

# merge datasets
pp_ag_borough <- merge(pp_ag_borough, ethnicity_by_borough, by='DISTRICT') # merge ethnicity rankings to borough-level dataset
pp_ag_borough <- pp_ag_borough[sample(nrow(pp_ag_borough)),]               # shuffle
pp_ag_borough_shp <- merge(borough, pp_ag_borough, by='DISTRICT')          # merge borough-level dataset to borough shapefile                                   
pp_ag_borough <- pp_ag_borough_shp[sample(nrow(pp_ag_borough_shp)),]

## 2.6. SCALE3: GROUPING LEVEL ------------------------------------------------>

### aggregate and merge pp to groupings --- groupings consolidate similar neighbourhoods
names <- colnames(pp)[-c(1,2,3)]
pp_ag_grouping <- pp %>%
  group_by(grouping) %>% 
  summarise_at(vars(names), mean)
pp_ag_grouping <- rapply(pp_ag_grouping, f = round, classes = "numeric", how = "replace", digits = 1) # round all vals to 1 d.p.
# pp_ag_nbr_shp <- merge(neighbourhoods, pp_ag_grouping, by='DISTRICT') # merge to neighbourhoods shapefile

#_______________________________________________________________________________
# 3. NON-SPATIAL EDA
#_______________________________________________________________________________

## 3.1. EXPLORING DESCRIPTIVE VIA STATISTICS ---------------------------------->

### ethnicity summary
ncol(ethnicity_by_borough)     # col count
colnames(ethnicity_by_borough) # col names 
summary(ethnicity_by_borough)  # summary breakdown
for (i in colnames(ethnicity_by_borough)){
  print(paste(i, sd(ethnicity_by_borough[[i]])))
}

### AOI: top/bottom ethnicities per borough w/ na boroughs exluded
top3_pop_w <- head(pp_ag_borough[order(pp_ag_borough$rank_w),][1],3)  # Bromley,            Barnet,             Wandsworth
top3_pop_a <- head(pp_ag_borough[order(pp_ag_borough$rank_a),][1],3)  # Newham,             Tower Hamlets,      Redbridge
top3_pop_b <- head(pp_ag_borough[order(pp_ag_borough$rank_b),][1],3)  # Lambeth.            Croydon,            Southwark

bot3_pop_w <- head(pp_ag_borough[order(-pp_ag_borough$rank_w),][1],3) # Kensington&Chelsea, Brent,              Barking&Dagenham, Brent,           
bot3_pop_a <- head(pp_ag_borough[order(-pp_ag_borough$rank_a),][1],3) # Havering,           Bromley,            Southwark
bot3_pop_b <- head(pp_ag_borough[order(-pp_ag_borough$rank_b),][1],3) # Sutton,             Harrow,             Havering


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# interpretation:
# heterogeneous city based on ethnicity.
# disparity from borough to borough
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### pp per borough
head(pp_ag_borough)                        # print first rows
nrow(pp_ag_borough)                        # count observations (rows)
ncol(pp_ag_borough)                        # count attributes (cols)
colnames(pp_ag_borough)                    # get attribute names
summary(pp_ag_borough)                     # get attribute stats e.g. min, max, mean, median, quartile(s)
for (i in colnames(pp_ag_borough)){        # get standard deviation per attribute
  print(paste(i, sd(pp_ag_borough[[i]])))
}

### AOIs: top/bottom boroughs based on mean perception
top3_pos_boroughs_w <- head(pp_ag_borough[order(-pp_ag_borough$mean_w),][1],3)  # Kensington&Chelsea,  Bromley,              Havering
top3_pos_boroughs_a <- head(pp_ag_borough[order(-pp_ag_borough$mean_a),][1],3)  # Westminster,         Kensington&Chelsea,   Hammersmith&Fulham
top3_pos_boroughs_b <- head(pp_ag_borough[order(-pp_ag_borough$mean_b),][1],3)  # Westminster,         Barking&Dagenham,     Hounslow

top3_neg_boroughs_w <- head(pp_ag_borough[order(pp_ag_borough$mean_w),][1],3)   # Haringey,            Hackney,              Islington
top3_neg_boroughs_a <- head(pp_ag_borough[order(pp_ag_borough$mean_a),][1],3)   # Islington,           Hackney,              Haringey 
top3_neg_boroughs_b <- head(pp_ag_borough[order(pp_ag_borough$mean_b),][1],3)   # Hackney,             Haringey,             Waltham Forest

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# interpretation:
# respondents from Kensington&Chelsea and Westminster tends to have the most positive perceptionn of the police
# respondents from Hackney, Haringey and Islington consistently have the most negative perception of the police across all ethnicities

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.2. EXPLORING DISTRIBUTION VIA HISTOGRAMS --------------------------------->

### foo: distribution per pp indicator
distribution <- function(pp_ag_borough, indicator, white_col, asian_col, black_col) {
# recreate df: col1: district, col2: ethnicity, col3: score
  districts <- rep(c(pp_ag_borough[["DISTRICT"]]))
  districts <- append(districts, rep(c(pp_ag_borough[["DISTRICT"]])))
  districts <- append(districts, rep(c(pp_ag_borough[["DISTRICT"]])))
  ethnicity <- rep(c("WHITE","ASIAN", "BLACK"),each=length(rep(c(pp_ag_borough[["DISTRICT"]]))))
  scores <- rep(pp_ag_borough[[white_col]])
  scores <- append(scores, rep(pp_ag_borough[[asian_col]]))
  scores <- append(scores, rep(pp_ag_borough[[black_col]]))
  data <- data.frame(districts, ethnicity,  scores)  # combine into new df
  # generate statistics
  mean <- ddply(data, "ethnicity", summarise, grp.mean=mean(scores))
  q1_lower <- ddply(data, "ethnicity", summarise, grp.mean=quantile(scores)[2])
  q2_median <- ddply(data, "ethnicity", summarise, grp.mean=median(scores))
  q3_upper <- ddply(data, "ethnicity", summarise, grp.mean=quantile(scores)[4])
  # plot results
  plot <- ggplot(data, aes(x=scores, fill=ethnicity, colour=ethnicity)) + 
    ggtitle(indicator) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.3) +
    geom_density(alpha=0.3) +
    geom_vline(data=mean, aes(xintercept=grp.mean, color=ethnicity), linetype="dotdash", size=1) +
    #geom_vline(data=q1_lower, aes(xintercept=grp.mean, color=ethnicity), linetype="dotted", size=1) +
    #geom_vline(data=q2_median, aes(xintercept=grp.mean, color=ethnicity), linetype="dotted", size=1, alpha=0.5) +
    #geom_vline(data=q3_upper, aes(xintercept=grp.mean, color=ethnicity), linetype="dashed", size=1) +
    labs(title=indicator,x="Score", y = "Density")+
    scale_x_continuous(limits=c(40,90), breaks=seq(40,90,10), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0,0.2), breaks=seq(0,0.2,0.02), expand = c(0, 0)) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank())
  return(plot)
}

### call and combine histograms for each indicator
hi_goodjob <- distribution(pp_ag_borough, "Good Job", "goodjob_w", "goodjob_a", "goodjob_b")
hi_listens <- distribution(pp_ag_borough, "Listens", "listens_w", "listens_a", "listens_b")
hi_fair <- distribution(pp_ag_borough, "Fair", "fair_w", "fair_a", "fair_b")
hi_fair_ss <- distribution(pp_ag_borough, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b")
### one legend only (insp: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs)
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(hi_fair_ss)
hi_grid <- grid.arrange(arrangeGrob(hi_goodjob + theme(legend.position="none"),
                                    hi_listens + theme(legend.position="none"),
                                    hi_fair + theme(legend.position="none"),
                                    hi_fair_ss + theme(legend.position="none"),
                                    nrow=2), top = textGrob("Mean PP Distribution by Indicator",gp=gpar(fontsize=18, font=2)), 
                                    mylegend, nrow=2,heights=c(10, 1))
hi_grid
#ggsave(hi_grid, file="fig1_pp_all_distribution_histograms.jpeg", width=30, unit='cm', dpi=300) # save to file

## 3.3. EXPLORING VARIATION VIA BAR CHARTS ------------------------------------>

### foo: create bar-charts that show indicator score 
pp_barchart <- function(pp_ag_borough, indicator, white_col, asian_col, black_col) {
  # recreate df: col1: district, col2: ethnicity, col3: score
  districts <- rep(c(pp_ag_borough[["DISTRICT"]]))
  districts <- append(districts, rep(c(pp_ag_borough[["DISTRICT"]])))
  districts <- append(districts, rep(c(pp_ag_borough[["DISTRICT"]])))
  ethnicity <- rep(c("WHITE","ASIAN", "BLACK"),each=length(rep(c(pp_ag_borough[["DISTRICT"]]))))
  scores <- rep(pp_ag_borough[[white_col]])
  scores <- append(scores, rep(pp_ag_borough[[asian_col]]))
  scores <- append(scores, rep(pp_ag_borough[[black_col]]))
  data <- data.frame(districts, ethnicity,  scores)  # combine into new df
  # plot results
  plot <- ggplot(data, aes(x=scores, y=districts, fill=ethnicity)) + 
    geom_bar(position="dodge", stat="identity", alpha=.9) +
    scale_y_discrete(label=abbreviate) +
    labs(x = 'SCORE(%)') +
    coord_cartesian(xlim=c(40,90)) +
    scale_x_continuous(breaks=seq(40,90,10))+
    ggtitle(indicator) +
    theme_minimal() +
    theme(axis.title.y=element_blank(), 
          legend.position="bottom", 
          panel.grid.minor = element_line(colour="gray96"),
          panel.grid.major = element_line(colour="lightgrey"),
          legend.title = element_blank(), 
          plot.title = element_text(hjust = 0.5))
  return(plot)
}

### call and combine bar graphs for each indicator
br_goodjob <- pp_barchart(pp_ag_borough, "Good Job", "goodjob_w", "goodjob_a", "goodjob_b")
br_listens <- pp_barchart(pp_ag_borough, "Listens", "listens_w", "listens_a", "listens_b")
br_fair <- pp_barchart(pp_ag_borough, "Fair", "fair_w", "fair_a", "fair_b")
br_fair_ss <- pp_barchart(pp_ag_borough, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b")
brlegend <- g_legend(br_goodjob)
br_grid <- grid.arrange(arrangeGrob(br_goodjob + theme(legend.position="none"),
                                    br_listens + theme(legend.position="none"),
                                    br_fair + theme(legend.position="none"),
                                    br_fair_ss + theme(legend.position="none"),
                                    nrow=1), top = textGrob("Mean PP by Borough",gp=gpar(fontsize=18, font=2)), 
                                    brlegend, nrow=2,heights=c(10, 1))
br_grid
#ggsave(br_grid, file="fig2_pp_all_variation_barcharts.jpeg", width=30, unit='cm', dpi=300) # save to file

## 3.4. EXPLORING VARIATION VIA BOX PLOTS ------------------------------------->

### foo: non-geo boxplots by ethnicity
pp_boxplot <- function(pp, indicator, white_col, asian_col, black_col) {
  # recreate df: col1: district, col2: ethnicity, col3: score
  districts <- rep(c(pp[["DISTRICT"]]))
  districts <- append(districts, rep(c(pp[["DISTRICT"]])))
  districts <- append(districts, rep(c(pp[["DISTRICT"]])))
  ethnicity <- rep(c("WHITE","ASIAN", "BLACK"),each=length(rep(c(pp[["DISTRICT"]])))) # each = 98
  scores <- rep(pp[[white_col]])
  scores <- append(scores, rep(pp[[asian_col]]))
  scores <- append(scores, rep(pp[[black_col]]))
  length(scores) # 294
  data <- data.frame(districts, ethnicity, scores)
  # plot results
  plot <- ggplot(data, aes(x=ethnicity, y=scores, fill=ethnicity)) + 
    geom_boxplot(position="dodge") +
    scale_y_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0)) +
    theme(legend.position="bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x=element_blank()) +
    ggtitle(indicator) 
  return(plot)
}

### call and combine boxplots for each indicator
bx_goodjob <- pp_boxplot(pp, "Good Job", "goodjob_w", "goodjob_a", "goodjob_b")
bx_listens <- pp_boxplot(pp, "Listens", "listens_w", "listens_a", "listens_b")
bx_fair <- pp_boxplot(pp, "Fair", "fair_w", "fair_a", "fair_b")
bx_fair_ss <- pp_boxplot(pp, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b")
bxlegend <- g_legend(bx_goodjob)
bx_grid <- grid.arrange(arrangeGrob(bx_goodjob + theme(legend.position="none"),
  bx_listens + theme(legend.position="none"),
  bx_fair + theme(legend.position="none"),
  bx_fair_ss + theme(legend.position="none"),
  nrow=1), top = textGrob("PP Variation by Ethnicity",gp=gpar(fontsize=18, font=2)), 
  bxlegend, nrow=2,heights=c(10, 1))
bx_grid
#ggsave(bx_grid, file="fig3_pp_all_variation_boxplots.jpeg", width=30, unit='cm', dpi=300) # save to file

### foo: geo boxplots for fair s&s'
pp_geo_boxplotter <- function(pp, indicator, white_col, asian_col, black_col, query) {
  # recreate df: col1: district, col2: ethnicity, col3: score
  districts <- rep(c(pp[["DISTRICT"]]))
  districts <- append(districts, rep(c(pp[["DISTRICT"]])))
  districts <- append(districts, rep(c(pp[["DISTRICT"]])))
  ethnicity <- rep(c("WHITE","ASIAN", "BLACK"),each=length(rep(c(pp[["DISTRICT"]])))) # each = 98
  scores <- rep(pp[[white_col]])
  scores <- append(scores, rep(pp[[asian_col]]))
  scores <- append(scores, rep(pp[[black_col]]))
  data <- data.frame(districts, ethnicity, scores)
  if (query == 'Most Positive (Black Comm.)') {
    data <- data[data$districts %in% top3_pos_boroughs_b$DISTRICT, ]
  } else if (query == 'Most Negative (Black Comm.)') {
    data <- data[data$districts %in% top3_neg_boroughs_b$DISTRICT, ]
  } else {
    data <- data
  }
  # plot results
  if (query == 'Most Positive (Black Comm.)' || query == 'Most Negative (Black Comm.)')  {
    plot <- ggplot(data, aes(x=ethnicity, y=scores, color=ethnicity, fill=ethnicity)) + 
      geom_boxplot(width=0.7, alpha=.5, position="dodge") +
      facet_wrap(~ districts) +
      xlab("") +
      scale_y_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0)) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_blank()) +
      ggtitle(query) 
    return(plot)
  } else {
    plot <- ggplot(data, aes(y=districts, x=scores, color=ethnicity, fill=ethnicity)) + 
      geom_boxplot(width=2, alpha=.5, position=position_dodge(width=0.005)) +
      scale_x_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0)) +
      theme(legend.position="bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
      ggtitle(indicator) 
    return(plot)
  }
}
### call and combine boxplots for fair S&S indicator only
bx_geo_fair_ss_top <- pp_geo_boxplotter(pp, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b", 'Most Positive (Black Comm.)')  # call1: most black people
bx_geo_fair_ss_bot <- pp_geo_boxplotter(pp, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b", 'Most Negative (Black Comm.)')  # call2: least black people
bx_geo_fair_ss <- pp_geo_boxplotter(pp, "Fair S&S", "fairSS_w", "fairSS_a", "fairSS_b", 'null')  # call2: least black people
bx_geo_grid <- grid.arrange(bx_geo_fair_ss, 
               grid.arrange(arrangeGrob(bx_geo_fair_ss_top + theme(legend.position="none"),
                                      bx_geo_fair_ss_bot + theme(legend.position="none"), nrow=2),
                          nrow=2,heights=c(10, 1)), top = textGrob("'Fair S&S' Indicator by Borough",gp=gpar(fontsize=18, font=2)), nrow=1, widths=c(2,1))
bx_geo_grid
#ggsave(bx_geo_grid, file="fig3_pp_fairSS_variation_boxplots.jpeg", width=30, unit='cm', dpi=300) # save to file

## 4.1. EXPLORING GEOGRAPHIC VARIATION ---------------------------------------->

### ethnicity --- insp: https://www.blog.cultureofinsight.com/2017/06/building-dot-density-maps-with-uk-census-data-in-r/

#### importing new data
ldn_oa <- readOGR(dsn="Data/London_Shapefiles/OA_2011_London_gen_MHW.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
ldn_b <- readOGR(dsn = "Data/London_Shapefiles/London_Borough_Excluding_MHW.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
ldneth <- read.csv(file='Data/bulk.csv', stringsAsFactors = F, check.names = F) %>%
  select(geography, "Ethnic Group: White; measures: Value", "Ethnic Group: Asian/Asian British; measures: Value", "Ethnic Group: Black/African/Caribbean/Black British; measures: Value")
names(ldneth) <- c('geography', 'white', 'asian', 'black')

#### prepare ethnicity data for dot density map
ldndata <- merge(ldn_oa@data, ldneth, by.x="OA11CD", by.y="geography", sort=FALSE) # merge ao .shp with the ethnic group data using geocodes
dots_num <- select(ldndata, white:black) / 10                 # df of number of people each dot represents
point_sdfs <- lapply(names(dots_num), function(x) {           # allocate dots in their respective polygons
  dotsInPolys(ldn_oa, as.integer(dots_num[, x]), f="random")})# f="random" means dots are randomly distributed 
ldn_b@data$id <- row.names(ldn_b@data)
ldn_b_points <- fortify(ldn_b, region = "id")
ldn_b_df <- merge(ldn_b_points, ldn_b@data, by = "id")        # convert sdf to df: retaining .shp data
dfs <- lapply(point_sdfs, function(x) {                       # convert spatialdf sdf to df: scrape coords of each dot
  data.frame(coordinates(x)[,1:2])})
ethnicities <- c("White", "Asian", "Black")                   # add an ethnicity variable to allow for categorising data by colour
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]}
dd_final <- bind_rows(dfs)# bind all dfs together
dd_final$Ethnicity <- factor(dd_final$Ethnicity, levels = ethnicities) # set factor levels to dictate plotting order from most > least dots
row.names(ldn_b) <- ldn_b$NAME
centroids <- data.frame(gCentroid(ldn_b, byid = TRUE))        # get the centre point of each borough (polygon)
centroids$NAME <- row.names(ldn_b)

#### plot dot density map of ethnic groups
pal <- c("#619cff", "#f8766d", "#00ba38")
centroids <- data.frame(gCentroid(ldn_b, byid = TRUE))        # get the centre point of each borough (polygon)
ethnicity_dd <- ggplot(ldn_b_df) +
  ggtitle("Heterogeneity of Ethnic Groups by Ward (w/ Borough Boundaries)", subtitle = "Dot Density Map") +
  geom_point(data=dd_final, aes(x, y, colour=Ethnicity), size=0.03, stroke=0) +
  geom_polygon(aes(long, lat, group=group), colour="#505050", size=0.6, fill=NA) +
  scale_colour_manual(values=pal) +
  theme_map() +
  theme(plot.background=element_rect(fill="black"), legend.position="bottom") +
  coord_map() +
  theme(plot.subtitle = element_text(hjust=0.5, colour="#FFFFFF", size=15, face="bold"), 
        plot.caption = element_text(color='white'), 
        plot.title=element_text(hjust=0.5, colour="#FFFFFF", size=20, face="bold"), 
        legend.position="bottom", legend.title=element_blank(), legend.spacing.x = unit(0, 'cm')) +
  geom_label(data=centroids, aes(x=x, y=y, label=rownames(centroids)), size=2, alpha=0.5, label.size=NA, fill=alpha(c("white"),0.5)) +
  guides(fill = guide_legend(label.position = "bottom"), color=guide_legend(override.aes=list(size=5, fill=NA))) +
  labs(caption="Data Source: UK Census 2011")
ethnicity_dd
#ggsave(ethnicity_dd, file="fig4_census_ethnicity_heterogeneity.jpeg",  width=30, unit='cm', dpi=550) # save to file


# plot leaflet map of Fair S&S indicator with slider for comparison
pal_domain <- data.frame(sample = c(pp_ag_borough$fairSS_b, pp_ag_borough$fairSS_w))
pal1 <- colorNumeric(palette = "RdYlGn", domain = pp_ag_borough$fairSS_b)
pal2 <- colorNumeric(palette = "RdYlGn", domain = pp_ag_borough$fairSS_w)
pal3 <- colorNumeric(palette = "RdYlGn", domain = pal_domain$sample)
factop <- function(x) {ifelse(is.na(x), 0, 0.7)} # make NA vals 0 ~ transparent
p_popup <- paste0("<strong>Borough:  </strong>", pp_ag_borough_shp$DISTRICT,
                  "<p>\n<strong>Black Resp.:  </strong>", pp_ag_borough_shp$fairSS_b, # create pop-ups on-click
                  "<p>\n<strong>White Resp.:  </strong>", pp_ag_borough_shp$fairSS_w)
choro_fairSS <- leaflet(pp_ag_borough_shp) %>% 
  addMapPane("left", zIndex = 0) %>%
  addMapPane("right", zIndex = 0) %>%
  addControl(tags$div(HTML("Black Community (left)")), position = "bottomleft") %>%
  addControl(tags$div(HTML("White Community (right)")), position = "bottomright") %>%
  addProviderTiles("CartoDB.DarkMatter", group="carto", layerId="black", options=pathOptions(pane="left")) %>%
  addProviderTiles("CartoDB.DarkMatter", group="carto", layerId="white", options=pathOptions(pane="right")) %>%
  addPolygons(options=pathOptions(pane="left"), # panel
              fillColor=~pal1(fairSS_b), fillOpacity=~factop(fairSS_b), # fill styling
              color="gray", weight=1, opacity = 1, stroke=TRUE, # line styling
              smoothFactor=0.5, popup = p_popup) %>% # labels
  addPolygons(options=pathOptions(pane="right"), # panel
              fillColor=~pal2(fairSS_w), fillOpacity=~factop(fairSS_w), # fill styling
              color="gray", weight=1, opacity = 1, stroke=TRUE, # line styling
              smoothFactor=0.5, popup = p_popup) %>% # labels
  addLegend(layerId = "white", pal=pal3, values=~pal_domain$sample, opacity = 1, title = "Fair S&S (%)") %>%
  addLabelOnlyMarkers(data=centroids, lng=~x, lat=~y, label=~rownames(centroids),
                      labelOptions=labelOptions(noHide=TRUE, direction='center', textOnly=TRUE, style = list(
                        "padding"     = "3px 8px",
                        "font-family" = "Leelawadee UI",
                        "text-shadow" = sprintf("-0.8px 0 %s, 0 0.8px %s, 0.8px 0 %s, 0 -0.8px %s, 0 0 7px %s", 'white', 'white', 'white', 'white', 'white')
                      ))) %>%
  addSidebyside(layerId = "sidecontrols", rightId = "white", leftId = "black")
choro_fairSS
#mapshot(choro_fairSS, url = paste0(getwd(), "/fig5_pp_fairSS_variation_choropleths.html"),
        #file = paste0(getwd(), "/fig5_pp_fairSS_variation_choropleths.png"))



#_______________________________________________________________________________
# 5. SPATIAL ANALYSIS: POLICE PERCEPTIONS (PP)
#_______________________________________________________________________________

# ______________________________________________________________________________
# NOTES
# ______________________________________________________________________________

# questions:
    
    ## how closely associated are results for fair_SS than to the other indicators?
    ## how are police perceptions distributed? (dispersed, random or clustered)
    ## whats the degree to which similar/dissimilar boroughs are clustered?
    ## Where does the clustering occur and not occur?
    ## What method to define a polygon's 'neighbours'?
    ## How does using different neighbour definitions change the results?
    ## does aggregating to grouping rather than borough reduce variation?
    ## is there a relationship between where black communities are and negeative police perception

# 0: defining neighbours
      ## Contiguity method: Queen or Rook (plus order)
      ## Fixed Distance method: Euclidean Distance (e.g. within 2km)
      ## Nearest Neighbours (K): number of closest neighbours (e.g. 3)
              
# 1. testing for autocorrelation
      ## adjacency-based autocorrelation
          ### scale: global ~ identify general clusters
              #### Global Moran's I
              #### Global Geary's C
              #### Global G
              #### Global G(d)
          ### scale: local tests ~ identify precise clusters
              #### Local Moran Scatterplot (graphically) 
              #### Local Moran's I (statistics)  
              #### Local Getis and Ord's Gi (and Gi*) 
      ## distance-based autocorrelation
              #### Semivariance
              #### Semivariogram

# 2. testing for regression
      ## autocorrelation
              #### Moran's I 
              #### Lagrange test for spatial lag dependence 
              #### Lagrange test for spatial error dependence 
      ## lag/error
              #### Spatial Lag Model
              #### Spatial Error Model  
              #### Spatial Durbin Model 


# ______________________________________________________________________________
# LINEAR NON-SPATIAL AUTOCORRELATION
# ______________________________________________________________________________

## foo:  assess strength of correlation between two sets of data using PMCC (Pearson's)
r_cor <- function(pp_ag_borough_shp, var1, var2, var1_label, var2_label, title) {
  x <- var1
  y <- var2
  xy <- data.frame(x=x, y=y)
  xy1 <- data.frame(x=sort(x), y=sort(y))
  xy2 <- data.frame(x=sort(x), y=sort(y, decreasing = T))
  p2 <- ggplot(xy1, aes(x, y))+ 
    geom_point()+
    geom_smooth(method="lm")+
    labs(title = "Positive Correlation (1=perfect)", x = var1_label, y = var2_label) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", -3, 3, label=paste("r =", round(cor(xy1$x, xy1$y, use="complete.obs"), 3)), x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface=2)
    #scale_y_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0)) +
    #scale_x_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0))
  p3 <- ggplot(xy2, aes(x, y))+ 
    geom_point()+
    geom_smooth(method="lm")+
    labs(title = "Negative Correlation (-1=perfect)", x = var1_label, y = var2_label) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", 2, 3, label=paste("r =", round(cor(xy2$x, xy2$y, use="complete.obs"), 3)), x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface=2)
    #scale_y_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0)) +
    #scale_x_continuous(limits=c(30,100), breaks=seq(30,100,10), expand = c(0, 0))
  grid <- grid.arrange(p2,p3, nrow=1, top = textGrob(title,gp=gpar(fontsize=12, font=2)))
}

## call2: 'fair_ss' against 'perc_b'
r_cor_white <- r_cor(pp_ag_borough_shp, pp_ag_borough_shp$fairSS_b, pp_ag_borough_shp$perc_b, "fairSS_b(%)", "borough_black_pop(%)", "Black Respondents")
r_cor_black <- r_cor(pp_ag_borough_shp, pp_ag_borough_shp$fairSS_w, pp_ag_borough_shp$perc_w, "fairSS_w(%)", "borough_white_pop(%)", "White Respondents")
r_col_grid <- grid.arrange(r_cor_white, r_cor_black, ncol=1, top = textGrob("S&S Fairness Perception VS Population Proportion",gp=gpar(fontsize=18, font=2)))
r_col_grid
# ______________________________________________________________________________
# SPATIAL CORRELATION ANALYSIS: Borough-Level Aggregation
# ______________________________________________________________________________

## SPATIAL WEIGHTS MATRICES (SWM) ---------------------------------------------->

### METHOD1: Queen (Contiguity edges corners) SWM
view(pp_ag_borough_shp)
pp_ag_borough_shp <- sp.na.omit(pp_ag_borough_shp)
plot(pp_ag_borough_shp)
#### queen: build SWM
neighbours_queen <- poly2nb(pp_ag_borough_shp, row.names = pp_ag_borough_shp@data$DISTRICT, queen = T) # create list of neighbours # create list of neighbours for each polygon
w <- nb2mat(neighbours_queen, style="W")                              # spatial weight matrix (swm) from the neighbours list
colnames(w) <- rownames(w)                                            # use the same row names for col names
pp_ag_borough_shp$QueenrowID <-  rownames(pp_ag_borough_shp@data)     # add row IDs a col in the data matrix
tm_shape(pp_ag_borough_shp)+tm_polygons()+tm_text(text="QueenrowID")  # print all with QueenrowID

#### queen: build SWL
Wl <- nb2listw(neighbours_queen)
#### ~~~~~~
    # Number of regions ...................... 33 
    # Number of nonzero links ................ 136 
    # Percentage nonzero weights ............. 12.48852 
    # Average number of links ................ 4.121212 
    # Weights style .......................... W 
    # Weights constants summary:
    #     n   nn S0       S1       S2
    #  W 33 1089 33 16.85206 135.4052
#### ~~~~~~

#### queen: testing
nbrs_first10_qry <- w[1:10,1:10] #### test 1
nbrs_first10_tbl <- kable(nbrs_first10_qry, digits=3, caption="First 10 rows and columns of W for London Boroughs", booktabs=T)
nbrs_first10_plt <- tm_shape(pp_ag_borough_shp[1:10,])+tm_polygons()+tm_text(text="QueenrowID", size = 0.7) # plot first 10 polygons
nbrs_first10_plt
nbrs_sutton_qry <- which(w["Sutton",]>0)
nbrs_sutton_tbl <- kable(which(nbrs_sutton_qry), digits=3, caption="Sutton's Neighbours using Queen Definition", booktabs=T) # results as table
nbrs_sutton_plt <- tm_shape(pp_ag_borough_shp[nbrs_sutton_qry,])+tm_polygons()+tm_text(text="DISTRICT")
nbrs_sutton_plt


### METHOD 2: Rook (Contiguity edges only)

#### rook: build SWM
neighbours_rook <- poly2nb(pp_ag_borough_shp, row.names = pp_ag_borough_shp@data$DISTRICT, queen = F) # create list of neighbours # create list of neighbours for each polygon
w <- nb2mat(neighbours_rook, style="W")                                           # create a spatial weight matrix (swm) from the neighbours list
colnames(w) <- rownames(w)                                                         # use the same row names for col names
pp_ag_borough_shp$RookrowID <-  rownames(pp_ag_borough_shp@data)                                      # add row IDs a col in the data matrix
tm_shape(pp_ag_borough_shp)+tm_polygons()+tm_text(text="QueenrowID") 

#### rook: build SWL
W2 <- nb2listw(neighbours_rook)

#### rook: testing
nbrs_islington_qry <- which(w["Islington",]>0) #### r tests
nbrs_islington_tbl <- kable(which(nbrs_islington_qry), digits=3, caption="Inslington's Neighbours using Rook Definition", booktabs=T) # results as table
nbrs_islington_plt <- tm_shape(pp_ag_borough_shp[nbrs_islington_qry,])+tm_polygons()+tm_text(text="DISTRICT")
nbrs_islington_plt
  
### METHOD3: Fixed Distance (3km)
library(sf)
neighbours_fd <- dnearneigh(st_geometry(st_centroid(borough)), 0, 3000)
W <- nb2mat(neighbours_fd, style="W")                                           # create a spatial weight matrix (swm) from the neighbours list
colnames(W) <- rownames(W)                                                   # use the same row names for col names
borough$RookrowID <-  rownames(borough@data)
  
### METHOD4: K nearest neighbors

## GLOBAL AUTOCORRELATION TESTS ----------------------------------------------->


### GLOBAL: 
moran(pp_ag_borough_shp@data$fairSS_b, Wl, n=length(Wl$neighbours), S0=Szero(Wl))
### ~~~~~~
# results:
    # Moran I statistic ........... 0.388566
    # $K [1] 2.297801


### GLOBAL: Moran I test under randomisation
moran.test(pp_ag_borough_shp@data$fairSS_b, Wl, na.action=na.omit) # omit NAs
### ~~~~~~
# description:
    # permutation test that calculates the statistical significance of the prior Moran's I statistic
    # randomly shuffles data and checks how often the 'Moran I statistic' is as large (positive or negative) as the observed statistic
    # e.g. if est performed 999 times and none of the test statistics are higher than the observed statistic;
    # 99.9% confidence that the level of autocorrelation is not due to chance.
# results:
    # Moran I statistic 
      # standard deviate .......... 2.9771
      # p-value ................... 0.001455
    # alternative hypothesis....... greater
    # sample estimates:
      # Moran I statistic ......... 0.38856600                
      # Expectation ............... -0.03846154           
      # Variance .................. 0.02057469
# interpretation:
    # poor global correlation in fair_ss responses by black respondents
### ~~~~~~

### GLOBAL: Monte-Carlo Simulation of Moran I --------------------------------->
moran.mc(pp_ag_borough_shp@data$fairSS_b, Wl, nsim=999, na.action=na.omit) 

### ~~~~~~
# description:
# results:
    # number of simulations ....... 1: 1000 ........ number of random simulations
    # statistic ................... 0.38857 ........ calculated value of Moran's I
    # observed rank ............... 998 ............ rank of the statistic against the 999 simulations
    # p-value ..................... 0.001 .......... statistical significance of the result
    # alternative hypothesis ...... greater
# intepretation:
    # statistic indicates moderate positive autocorrelation
    # observed rank indicates there was higher than 997 random permutations / simulation
    # p-value states indicates 0.1% probability of the observed Moran's I statistic being due to chance - expressed as a 99.9% confidence interval
    # The 'fairSS_b' displays moderate positive autocorrelation in London
### ~~~~~~

### Global Geary's C ---------------------------------------------------------->
### Global G ------------------------------------------------------------------>
### Global G(d) --------------------------------------------------------------->

### Local Moran Scatterplot (graphically) 
mp <- moran.plot(pp_ag_borough_shp@data$fairSS_b, Wl, labels=pp_ag_borough_shp@data$DISTRICT, 
           xlab="S&S Fairness Perception", ylab="Spatially Lagged S&S Fairness Perception")
if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") + 
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    xlab(xname) + ylab(paste0("Spatially lagged ", xname)) +
    ggtitle("Local Moran Scatterplot: S&S Fairness amongst Black Respondents")
}
### ~~~~~~
# description:
    # measures how correlated nearby values;
    # identifies clusters of high and low values
    # x-axis shows the value of a spatial process at location i
    # y-axis shows the weighted sum of the values at the neighbours of i
    # plot divided into 4 cluster quadrants:
        # Top-right:    High value neighbouring high values ~ positive 
        # Bottom-right: High value neighbouring low values  ~ negative
        # Top-left:     Low value neighbouring high values  ~ negative 
        # Bottom-left:  Low value neighbouring low values    ~ positive 
# results:
    # sparse plot
    # some clustered boroughs in 'low values neighbouring low values' section
    # e.g. Hackney, Camden, Islington ~ Central-North Boroughs
### ~~~~~~


### Local Moran's I (statistics)

#### build row standardized adjacency-based swm
Ii <- localmoran(pp_ag_borough_shp$fairSS_b, Wl)
pp_ag_borough_shp$Ii <- Ii[,"Ii"]
tm_shape(pp_ag_borough_shp) + tm_polygons(col="Ii", palette="-RdBu", style="quantile")
### Warning: Variable "Ii" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

#### statistically significant wards with the unadjusted p-value
pp_ag_borough_shp$Iip_unadjusted <- Ii[,"Pr(z > 0)"]
pp_ag_borough_shp$Ii_un_sig <- "nonsignificant"
pp_ag_borough_shp$Ii_un_sig[which(pp_ag_borough_shp$Iip_unadjusted < 0.05)] <- "significant"
tm_shape(pp_ag_borough_shp) + tm_polygons(col="Ii_un_sig", palette="-RdBu")



### Local Getis and Ord's Gi (and Gi*) 

#### create distance based swm
NbrL <- dnearneigh(coordinates(pp_ag_borough_shp), 0, 10000) 
D <- nb2listw(NbrL, style="B")
D_star <- nb2listw(include.self(NbrL), style="B")
#### ~~~~~~
# Number of regions ............. 27 
# Number of nonzero links ....... 702 
# Percentage nonzero weights .... 96.2963 
# Average number of links ....... 26 
#### ~~~~~~

#### set test parameters and run test
G <- globalG.test(pp_ag_borough_shp$fairSS_b, D) # Warning: Out-of-range p-value: reconsider test arguments
G <- globalG.test(pp_ag_borough_shp$fairSS_b, D_star) # Warning: Out-of-range p-value: reconsider test arguments
Gi <- localG(pp_ag_borough_shp$fairSS_b, D)
pp_ag_borough_shp$Gi <- Gi
Gi_star <- localG(pp_ag_borough_shp$fairSS_b, D_star)
pp_ag_borough_shp$Gi <- Gi
pp_ag_borough_shp$Gi_star <- Gi_star
tm_shape(pp_ag_borough_shp) + tm_polygons(col="Gi_star", palette="-RdBu", style="quantile")


# ______________________________________________________________________________
# REGRESSION TESTS
# ______________________________________________________________________________

### Moran's I 
### Lagrange test for spatial lag dependence 
### Lagrange test for spatial error dependence 
### Spatial Lag Model
### Spatial Error Model  
### Spatial Durbin Model 