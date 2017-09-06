
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(sp)
library(rasterVis)
library(RStoolbox)
library(maps)
library(mapproj)
library(animation)
library(twitteR)
library(cowplot)

## set your local wd
setwd("projects/modis/spain")

##for the twittR oauth 
options(httr_oauth_cache=T)

#read in cities

places <- readOGR("ne_10m_populated_places", 'ne_10m_populated_places')
places_spain <- places[places$NAME %in% c("Madrid","Barcelona"), ]
places_spain <- data.frame(places_spain)

#grabs the current time

currentTime <- Sys.Date()
yr <- format(Sys.Date(), "%Y")


#sets the julien day from the current Time
currentJD <- yday(currentTime)

#a function that takes a JD and returns a three digit JD 
fixJD <-  function (currentJD) {
  
  if (nchar(currentJD)==1){
    currentJD <-  paste0("00",currentJD)
  }
  else if (nchar(currentJD)==2){
    currentJD <- paste0("0",currentJD)
  }
  
  else {
    currentJD <- currentJD
  }
  return 
  currentJD
  
}

#run the function based on the current day
currentJD <- fixJD(currentJD)

#create the spain url

urlspain <- paste0("https://lance3.modaps.eosdis.nasa.gov/imagery/subsets/?subset=Spain.",yr,currentJD,".terra.1km.tif")

#fucntion download the raster, save to file named based on year and date

getFile <- function(url) {
  
  currentFile <- paste0(yr,currentJD,".tiff")
  download.file(url, currentFile )
  return (currentFile)
}

#invoke function
currentFile <- getFile(urlspain)

#function to map the raster

createMap<- function(currentFile) { 
  
  #sets ggsave arguemnets based on the coord_fixed value
  height = 9
  width = (1.307) * height
  
  #create rasterbrick
  currentRaster <- brick(currentFile)
  print(extent(currentRaster))
  ggRGB(currentRaster, r=1, g=2, b=3, maxpixels =9999999999999999)+
    coord_fixed(1.307, xlim=c(-10.0 , 4.0 ),  ylim=c(35.5265, 44.0701))+
    geom_point(data=places_spain, family="mono", fontface = "bold",  aes(x=LONGITUDE, y=LATITUDE, label=NAME))+
    geom_label(data=places_spain, fontface = "bold", size=2.5, nudge_y =-.22, color="white", fill="black", alpha=.8, aes(x=LONGITUDE, y=LATITUDE, label=NAME))+
    #annotate("text", x = 37, y = 5, label =format(currentTime, format="%B %d %Y")
             #, color="black", size=6, family="sans",fontface = "bold" )+
    theme_nothing()+

    ggsave(filename=paste0(yr, currentJD,"spain.png"), width = width, height = height, units = "in", dpi=200)
}

#invoke the function on the file
createMap(currentFile)