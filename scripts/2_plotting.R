# 30 Days Map Challenge
# Day 25 - Covid 19
# Jonas Stillhard, Simon Graf, November 2020
# 
# You first need to run the script 1_data_preparation. 
# This will save a RData image to the data folder. 
# This file will then be loaded here.


# Set up R ----------------------------------------------------------------


requiredPackages <- c("dplyr", "raster", "sp", "lubridate", "rgeos", "readxl")


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
w_dir <- "C:/Users/JS/Documents/R/30daymapchallenge"



# Read Data ---------------------------------------------------------------

# We now read the data created in 1_data_preparation
load(paste0(w_dir, "/data/dat.RData"))



# Create plots ------------------------------------------------------------


# Plotting rules: 
# We first retrieve the max value for the weekly vals and will round this
# value to the next 1k.
maxV <- ceiling(max(weeklyVals$nWeek100k)/100)*100

# Create a vector for the cuts which is used below for the creation of the 
# color ramp and the cutting.
cts <- seq(0, maxV, 10)

# Create colorRamp
clr <- colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(length(cts))
names(clr) <- c(1:length(clr))

# Cut weekly vals
weeklyVals$grps <- cut(weeklyVals$nWeek100k, seq(0, maxV, 10), labels = F, include.lowest = T)

# We now add the colors to the weekly vals DF
weeklyVals$clr <- clr[match(weeklyVals$grps, names(clr))]

# Create layout matrix

lyt <- matrix(c(c(2, 3, 3, 3, 4, 4, 4), rep(c(rep(1, 6), 5),2), rep(c(rep(1, 6), 6),2)), ncol = 7, nrow = 5, byrow = T)
layout(lyt, widths = rep(1, 7), heights = rep(1, 5))
layout.show(n = 6)

totCases <- weeklyVals %>% 
  dplyr::group_by(week) %>% 
  dplyr::summarise(nCases = sum(nWeekNew)
                   , nMort = sum(nWeekDead)) %>% 
  as.data.frame()


for(i in c(4:47)){
  png(paste0(w_dir, "/maps/png/", i, "_day25.png")
      , width = 2000, height = 1500, res = 200)
  par(mar = rep(.8, 4))

  layout(lyt, widths = rep(1, 6), heights = rep(1, 5))
  
  
  
  x <- weeklyVals[weeklyVals$week == i,]
  cCentre$nWeekD <- x$nWeek100kD[match(cCentre$id, x$kt)]
  ch$col <- x$clr[match(ch$kt, x$kt)]
  cCentre$nWeekD[is.na(cCentre$nWeekD)] <- 0
  cBuff <- raster::buffer(cCentre, width = cCentre$nWeekD*750, dissolve = F)
  plot(ch, col = ch$col)
  plot(cBuff, add = T, lwd = 2)
  
  
  
  plot(0, type = "n", axes = F, xlab = "", ylab = "")
  text(0.57, 0.5, paste0("Week ", i), adj = 0, cex = 2)

  
  plot(0, xlim = c(3, 52), ylim = c(0, 55000), axes = F, type = "n", ylab = "", xlab = "week")
  lines(totCases$week[totCases$week <= i], totCases$nCases[totCases$week <= i], lwd = 1.5, col = "blue")
  text( x= 4, y = 45000, "New cases/week", adj = 0)
  text(x = 4, y = 40000, paste("Total cases:", sum(totCases$nCases[totCases$week <= i])), adj = 0)
  axis(1, at = seq(4, 48, 4), labels = seq(4, 48, 4), line = .5)
  axis(2, at = seq(0, 55000, 5000), labels = seq(0, 55000, 5000), las = 1)
  
  plot(0, xlim = c(3, 52), ylim = c(0, 650), axes = F, type = "n", xlab = "week", ylab = "")
  lines(totCases$week[totCases$week <= i], totCases$nMort[totCases$week <= i], lwd = 1.5)
  text( x= 4, y = 520, "New deceased/week", adj = 0)
  axis(1, at = seq(4, 48, 4), labels = seq(4, 48, 4), line = .5)
  axis(2, at = seq(0, 600, 30), labels = seq(0, 600, 30), las = 1)
  
  # Add color-legend
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  legend(x = 0, y = 1, fill = clr[seq(0, 140, 10)], legend = seq(10, 1400, 100), bty = "n", y.intersp = 0.5, box.lwd = 0)
  
  # Add size-legend
  plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  legend(x = 0, y = 1,legend = seq() pch = 16, cex = c())
  
  dev.off()
}

for(i in c(10:46)){
  x <- weeklyVals[weeklyVals$week == i,]
  treemap::treemap(x, index = "kt", vSize = "nWeek100k", vColor = "clr", type = "color", algorithm = "pivotSize", sortID = "kt")
}


rgl::polygon3d(ch, ch$nWeek100k)



for(i in c(10:46)){
  x <- weeklyVals[weeklyVals$week == i,]  
  ch$nWeek100k <- x$nWeek100k[match(ch$kt, x$kt)]
  y <- raster::raster(ext = extent(ch))
  chRas <- rasterize(ch, y, field = ch$nWeek100k)
  clrs <- x$clr[order(x$kt)]
  rgl::open3d(windowRect = c(0, 00, 1200, 1000))
  rgl::rgl.viewpoint( theta = 40, phi = 0, fov = 40, zoom = 1, 
                      scale = par3d("scale"), interactive = TRUE, 
                      type = c("userviewpoint", "modelviewpoint") )
  rasterVis::plot3D(chRas, col = clrs, useLegend = T, rev = F, theta = 40, phi = 200, r = 150, asp = 1, smooth = T, zlim = c(0, 1400))
  rgl::snapshot3d(paste0("C:/Users/JS/Documents/R/Uholka/6_30daychallenge/maps/day25/", i, "_day25.png"))
}

treemap::treemap(x, index = "kt", vSize = "nWeek100k", vColor = "clr", type = "color")

