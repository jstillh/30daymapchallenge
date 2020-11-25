# 
# 30 Days Map Challenge
# Day 25 - Covid 19
# Jonas Stillhard, November 2020

# This script describes the data preparation for the 30daymapchallenge, day 25.
# The data stems from open data zurich
# https://github.com/openZH/covid_19
# You need to retrieve the following dataset: 
# - COVID19_Fallzahlen_CH_total_v2.csv
# - mappingCanton_BFS.csv
# in 
# https://github.com/openZH/covid_19/tree/master
# 
# This can be done by either cloning the entire repository (it's huge!) or by only cloning the relevant 
# files following this so-thread - see answer by udondan.
# https://stackoverflow.com/questions/600079/git-how-do-i-clone-a-subdirectory-only-of-a-git-repository
# 
# Either way, you then need to define the path to the respective folder as data_dir. 
# The data can be found in the data-directory and is up to date until 23rd of November.
# 
# Population data stems from Bundesamt für Statistik - the data wrangling is done in this script.
# 
# Spatial Polygons will be loaded using raster::get_data

# Set up R ----------------------------------------------------------------


requiredPackages <- c("dplyr", "raster", "sp", "lubridate", "rgeos", "readxl", "rgdal", "tidyr", "zoo")


# install/load required packages:
if (exists("requiredPackages")) {
  # install required packages that are not installed yet:
  new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  # load required packages:
  lapply(requiredPackages, library, character.only=T)
}

rm(new.packages, requiredPackages)


# We are not using setwd() but define a char-string here
# w_dir <- "C:/Users/JS/Documents/R/30daymapchallenge"
w_dir <- "C:/gitrepos/30daymapchallenge"

# Change this. In Fact, you only need the file COVID19_Fallzahlen_CH_total_v2.csv in the Covid19 repo.
# data_dir <- "C:/Users/JS/Documents/R/covid_19"
data_dir <- "C:/gitrepos/30daymapchallenge/data"


# Read Data ---------------------------------------------------------------


# Now read in the f&%*in xls by BFS
popDat <- readxl::read_excel("C:/gitrepos/30daymapchallenge/data/population_data.xlsx", sheet = 1, col_names = T)

# this will download the administrative boundaries of Switzerland
ch <- raster::getData("GADM", country = "CHE", level = 1)

sp::plot(ch)


# Read in covid data
# covidData <- read.csv(paste0(data_dir, "/COVID19_Fallzahlen_CH_total_v2.csv"), stringsAsFactors = F)
covidData <- read.csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv", stringsAsFactors = F)
bfsMapping <- read.csv(paste0(w_dir, "/data/mappingCanton_BFS.csv"))


head(covidData)
str(covidData)


# Data wrangling ----------------------------------------------------------
# The following things are being done here: 
# - Make date column to date format
# - Remove FL
# - create a column with number of new cases using dplyr::lag on ncumul_conf

# Population data: 
# - Tidy up the shitty file. 
# - go for long format. Stupid BFS!

# Polygon Data: 
# - Change crs from wgs 84 to ch1903


ch <- sp::spTransform(ch, CRS("+init=epsg:21781"))

# Complete date-canton combinations and smoothly fill NAs (for confirmed and deceased) in between while keeping NAs at the start and the end of each cantonal time series
covidData <- covidData %>% 
  complete(date, abbreviation_canton_and_fl) %>% 
  group_by(abbreviation_canton_and_fl) %>% 
  arrange(date) %>% 
  mutate(ncumul_conf = round(na.fill(ncumul_conf, c(NA, "extend")), 0),
         ncumul_deceased = round(na.fill(ncumul_deceased, c(NA, "extend")))) %>% 
  ungroup()

# Change date column to date formant
covidData$date <- as.Date(covidData$date)
covidData$week <- lubridate::week(covidData$date)

# Remove FL
covidData <- covidData[covidData$abbreviation_canton_and_fl != "FL",]

# Add BFS identifier
covidData$bfs <- bfsMapping$bfs[match(covidData$abbreviation_canton_and_fl, bfsMapping$abk)]

# Order df on date and canton to be able to do the lag operation below
covidData <- covidData[order(covidData$abbreviation_canton_and_fl, covidData$date),]

# Now, create a new_conf column 
covidData$nconf <- ifelse(dplyr::lag(covidData$abbreviation_canton_and_fl) == covidData$abbreviation_canton_and_fl
                            , covidData$ncumul_conf - dplyr::lag(covidData$ncumul_conf), NA)

# Create a new dead column 
covidData$ndead <- ifelse(dplyr::lag(covidData$abbreviation_canton_and_fl) == covidData$abbreviation_canton_and_fl
                          , covidData$ncumul_deceased - dplyr::lag(covidData$ncumul_deceased), NA)


# Create a identifier to join afterwards - we'll use the abbrevations
ch$kt <- substr(ch$HASC_1, 4, 5)

# We now create midpoints for each of the polygons of the cantons. We'll use these for the buffer applied afterwards
cCentre <- rgeos::gCentroid(ch, byid = T, id = ch$abb)

cCentre$id <- ch$kt


# Now we create weekly values for each canton

weeklyVals <- covidData %>% 
  dplyr::group_by(abbreviation_canton_and_fl, week) %>% 
  dplyr::summarise(nWeekNew = sum(na.omit(nconf))
                   , nWeekDead = sum(na.omit(ndead))) %>% 
  as.data.frame()

colnames(weeklyVals) <- c("kt", "week", "nWeekNew", "nWeekDead")

# Create daily 7 day sums for each canton

dailyVals <- covidData %>% 
  group_by(abbreviation_canton_and_fl) %>% 
  mutate(nWeekNew = rollsum(nconf, 7, fill = NA, align = "right"),
         nWeekDead = rollsum(ndead, 7, fill = NA, align = "right")) %>% 
  as.data.frame() %>% 
  dplyr::select(kt = abbreviation_canton_and_fl, date, nWeekNew, nWeekDead) %>% 
  mutate(nWeekNew = replace_na(nWeekNew, 0), nWeekDead = replace_na(nWeekDead, 0))


# Prepare population data
popDat <- as.data.frame(popDat)

# Row 3 contains the colnames we want to have. 
# Row 7 contains the population (10k) per canton

colnames(popDat) <- popDat[3,]
popDat <- popDat[7,]
popDat <- popDat[,4:ncol(popDat)]
popDat <- t(popDat)
popDat <- as.data.frame(popDat)

# The colum,n containing the population data is a lovely factor (f%&çCk). change to numeric
popDat$pop1k <- as.numeric(as.character(popDat$`7`))

popDat$kt <- row.names(popDat)
popDat$bfs <- bfsMapping$bfs[match(popDat$kt, bfsMapping$abk)]


# To get a value per 100k inhabitants, we'll add the population data to the weeklyVals df
popDat$pop100k <- popDat$pop1k/100

weeklyVals$bfs <- bfsMapping$bfs[match(weeklyVals$kt, bfsMapping$abk)]
weeklyVals$pop100k <- popDat$pop100k[match(weeklyVals$bfs, popDat$bfs)]

dailyVals$bfs <- bfsMapping$bfs[match(dailyVals$kt, bfsMapping$abk)]
dailyVals$pop100k <- popDat$pop100k[match(dailyVals$bfs, popDat$bfs)]

# Create values per 100 k
weeklyVals$nWeek100k <- weeklyVals$nWeekNew/weeklyVals$pop100k
weeklyVals$nWeek100kD <- weeklyVals$nWeekDead/weeklyVals$pop100k

dailyVals$nWeek100k <- dailyVals$nWeekNew/dailyVals$pop100k
dailyVals$nWeek100kD <- dailyVals$nWeekDead/dailyVals$pop100k

# Save the workspace. 
save.image(paste0(w_dir, "./data/dat.RData"))
