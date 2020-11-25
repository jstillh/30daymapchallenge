# 30 Days Map Challenge
# Day 25 - Covid 19
# Jonas Stillhard, Simon Graf, November 2020
# 
# You first need to run the script 1_data_preparation. 
# This will save a RData image to the data folder. 
# This file will then be loaded here.


# Set up R ----------------------------------------------------------------


requiredPackages <- c("dplyr", "raster", "sp", "lubridate", "rgeos", "purrr", "magick")


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

rm( new.packages, requiredPackages)


# We are not using setwd() but define a char-string here
# w_dir <- "C:/Users/JS/Documents/R/30daymapchallenge"
# w_dir <- "C:/gitrepos/30daymapchallenge"
w_dir <- "H:/R/30daymapchallenge"

# Source the function by B. Bolker 
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")


# Read Data ---------------------------------------------------------------

# We now read the data created in 1_data_preparation
load(paste0(w_dir, "/data/dat.RData"))



# Create plots ------------------------------------------------------------


# Plotting rules: 
# We first retrieve the max value for the weekly vals and will round up this
# value to the next 100.
# maxV <- ceiling(max(weeklyVals$nWeek100k)/100)*100
maxV <- ceiling(max(dailyVals$nWeek100k)/100)*100

# Create a vector for the cuts which is used below for the creation of the 
# color ramp and the cutting.
cts <- seq(0, maxV, 10)

# Create colorRamp
clr <- colorRampPalette(c("white", RColorBrewer::brewer.pal(9, "Reds")))(length(cts))
names(clr) <- c(1:length(clr))

# Cut weekly vals
weeklyVals$grps <- cut(weeklyVals$nWeek100k, seq(0, maxV, 10), labels = F, include.lowest = T)
dailyVals$grps <- cut(dailyVals$nWeek100k, seq(0, maxV, 10), labels = F, include.lowest = T)

# We now add the colors to the weekly vals DF
weeklyVals$clr <- clr[match(weeklyVals$grps, names(clr))]
dailyVals$clr <- clr[match(dailyVals$grps, names(clr))]

# Add buffer for legend. 
# Bit hacky...
buffP <- sp::SpatialPoints(coords = matrix(c(780000, 85000), ncol = 2))
b1 <- raster::buffer(buffP, 750)
buffP <- sp::SpatialPoints(coords = matrix(c(780000, (85000 + 4.5*750)), ncol = 2))
b2 <- raster::buffer(buffP, 5 * 750)
buffP <- sp::SpatialPoints(coords = matrix(c(780000, (85000 + 9.5 * 750)), ncol = 2))
b3 <- raster::buffer(buffP, 10 * 750)
buffP <- sp::SpatialPoints(coords = matrix(c(780000, (85000 + 19.5 * 750)), ncol = 2))
b4 <- raster::buffer(buffP, 20 * 750)
buffP <- sp::SpatialPoints(coords = matrix(c(780000, (85000 + 24.5*750)), ncol = 2))
b5 <- raster::buffer(buffP, 25 * 750)

# Create layout matrix
lyt <- matrix(c(c(2, 3, 3, 3, 4, 4, 4), rep(c(rep(1, 6), 5), 4)), ncol = 7, nrow = 5, byrow = T)
layout(lyt, widths = rep(1, 7), heights = rep(1, 5))
layout.show(n = 5)

# Create a total cases dataset to add to the plots
totCases <- weeklyVals %>% 
  dplyr::group_by(week) %>% 
  dplyr::summarise(nCases = sum(nWeekNew)
                   , nMort = sum(nWeekDead)) %>% 
  as.data.frame()

totCasesDaily <- dailyVals %>% 
  group_by(date) %>% 
  summarise(nCases = sum(nWeekNew), 
            nMort = sum(nWeekDead)) %>% 
  as.data.frame()

maxDate <- lubridate::as_date(max(dailyVals$date))
minDate <- lubridate::as_date(min(dailyVals$date))

# for(i in c(4:47)){
for(i in c(minDate:maxDate-2)){
  # Add a leading zero if i < 10
  # k <- ifelse(nchar(i) == 1, paste0("0", i), i)
  # k <- case_when(
  #   nchar(i) == 1 ~ paste0("00", i),
  #   nchar(i) == 2 ~ paste0("0", i),
  #   TRUE ~ paste(i)
  # )
  k <- lubridate::as_date(i)
  
  # png(paste0(w_dir, "/maps/png/", k, "_day25.png")
  # We are taking i here as this will properly order the output - this is important for the 
  # the gif-creation.
  png(paste0(w_dir, "/maps/png/", i, "_day25.png")
      , width = 2000, height = 1500, res = 200)
  par(mar = rep(.8, 4))

  layout(lyt, widths = rep(1, 6), heights = rep(1, 5))
  
  # x <- weeklyVals[weeklyVals$week == i,]
  x <- dailyVals[dailyVals$date == k,]
  cCentre$nWeekD <- x$nWeek100kD[match(cCentre$id, x$kt)]
  ch$col <- x$clr[match(ch$kt, x$kt)]
  cCent <- cCentre[!is.na(cCentre$nWeekD) & cCentre$nWeekD > 0,]
  plot(ch, col = ch$col)
  if(nrow(cCent@data) != 0){
    cBuff <- raster::buffer(cCent, width = cCent$nWeekD*750, dissolve = F)
    plot(cBuff, add = T, lwd = 2)
  }
  
  plot(b1, add = T, lwd = 1.5)
  plot(b2, add = T, lwd = 1.5)
  plot(b3, add = T, lwd = 1.5)
  plot(b4, add = T, lwd = 1.5)
  plot(b5, add = T, lwd = 1.5)
  
  text(x = 780000, y  = 81000, "Deceased/100k per week")
  text(x = 780000, y = 85000+13*750, "5")
  text(x = 780000, y = 85000+23*750, "10")
  text(x = 780000, y = 85000+43*750, "20")
  text(x = 780000, y = 85000+53*750, "25")
  
  plot(0, type = "n", axes = F, xlab = "", ylab = "")
  text(0.57, 0.5, format(k, "%d.%m.%y"), adj = 0, cex = 2)

  
  # Line plot for cases
  plot(0, xlim = c(minDate, maxDate), ylim = c(0, 55000), axes = F, type = "n", ylab = "", xlab = "")
  lines(totCasesDaily$date[totCasesDaily$date <= k], totCasesDaily$nCases[totCasesDaily$date <= k], lwd = 1.5, col = "blue")
  text( x= minDate+10, y = 50000, "New cases/week", adj = 0)
  # text(x = minDate+10, y = 45000, paste("Total cases:", sum(totCasesDaily$nCases[totCasesDaily$date <= k])), adj = 0)
  axis(1, at = seq(minDate, maxDate+10, 28)
       , labels = format(lubridate::as_date(seq(minDate, maxDate+10, 28)), "%d.%m"), line = .5)
  axis(2, at = seq(0, 55000, 5000), labels = seq(0, 55000, 5000), las = 1)
  
  # Line plot for deceased
  plot(0, xlim = c(minDate, maxDate), ylim = c(0, 650), axes = F, type = "n", ylab = "", xlab = "")
  lines(totCasesDaily$date[totCasesDaily$date <= i], totCasesDaily$nMort[totCasesDaily$date <= i], lwd = 1.5)
  text(x= minDate+10, y = 590, "New deceased/week", adj = 0)
  # text(x= minDate+10, y = 530, paste("Total deceased:", sum(totCasesDaily$nMort[totCasesDaily$date <= i])), adj = 0)
  axis(1, at = seq(minDate, maxDate+10, 28)
       , labels = format(lubridate::as_date(seq(minDate, maxDate+10, 28)), "%d.%m"), line = .5)
  axis(2, at = seq(0, 650, 50), labels = seq(0, 650, 50), las = 1)
  
  # Add color-legend
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  legend(x = 0, y = .75, fill = clr[seq(0, 140, 10)], legend = seq(100, 1400, 100), bty = "n", y.intersp = 1,  border = "NA"
         , box.cex = c(2, 3))
  text(x = 0, y = .8, "New cases/100k \n per week", adj = 0)

  
  dev.off()
}



# Create Gif --------------------------------------------------------------


# Now, we are going to create the gif. 
# This requires quite some resources

files <- list.files(paste0(w_dir, "/maps/png"), full.names = T)

gc(full = T)

images <- purrr::map(files, magick::image_read)
images <- magick::image_join(images)
images <- magick::image_scale(images, "1500x2000")
animation <- magick::image_animate(images, fps = 10, optimize = T)
magick::image_write(animation, paste0(w_dir, "/maps/day25.gif"))
