# 30daymapchallenge
*Simon Graf, Jonas Stillhard*  

This repository contains data and scripts for the covid-map based on the covid-data of open data zh.  

---

[data](./data): Contains the relevant datasets. Data from [open data zh](https://github.com/openZH/covid_19) is up to date until November, 24th 2020.  
[scripts](./scripts): Contains the scripts used for the data preparation and the plotting of the gif. 

---
### Requirements
To run the scripts, you'll need the following packages (these will be installed automatically in the scripts)  
`dplyr`, `raster`, `rgeos`, `sp`, `lubridate`, `rgeos`, `readxl`, `purrr` and `magick`.  
Additionally, the script `2_plotting` will source a function by [B. Bolker](http://www.math.mcmaster.ca/bolker/R/misc/legendx.R). 
