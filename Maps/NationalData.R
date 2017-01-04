 # Make a Cool Map
# Download District Data From NCES at http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip

# Input Data into R

library(foreign)
library(leaflet)
library(leafletR)
library(rgdal)
library(rgeos)
library(sp)
library(geojsonio)

#input url of map api 

url <- "http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip"

#unzip it

content <- download.file(url, "CCD1314.zip")

wd <- paste0(getwd(),"/","CCD1314.zip")

unzip(wd, junkpaths = TRUE)

#load spss data

spssdata <- read.spss("PPEDATA2014.sav")

# now the shapefile

shpdata <- readOGR(getwd(), "District_Level_Common_Core_of_Data_20132014")

  
  # transfer spss data into usable format/ round ADJPPE
  
  spssdata <- as.data.frame(spssdata)
  spssdata$ADJPPE <- round(spssdata$ADJPPE, digits = 0)
  spssdata$NCESID <-  as.character(spssdata$NCESID)
  
  # get rid of excess variables
  
  spssdata2 <- spssdata[c("NCESID", "DISTRICTNAME", "ADJPPE", "POORPER", "SPECPER")]
  
  #change map format
  
  shpdata <- spTransform(shpdata, CRS("+init=epsg:4326"))
  
  # separate district data from mapping data to add spssdata
  
  shpDistrictData <- shpdata@data[, c("OBJECTID", "GEOID")]
  
  # create spatial polygons
  
  shpdata2 <-gSimplify(shpdata, tol = 0.01, topologyPreserve = TRUE)
  
  
  # write the geojson
  
  shpdata4 <- SpatialPolygonsDataFrame(shpdata2, data = shpDistrictData)
  
  # get rid of those grody nulls
  
  shpdata5 <- lapply(shpdata4@data, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  shpdata5$NCESID <- as.numeric(as.character(shpdata5$GEOID))
  
  # same
  
  shpdata5 <- lapply(shpdata5, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  #long gross merge that doesnt mess up data
  
  shpdata5 <- merge(shpdata5, spssdata2, by = "NCESID", all.x = TRUE)
  shpdata5 <- shpdata5[c("OBJECTID", "DISTRICTNAME", "ADJPPE")]
  shpdata5 <- merge(shpdata4, shpdata5, by = "OBJECTID", all.x = TRUE)
  
  # write a geojson file 
  
  writeOGR(shpdata5, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014FULL", ".geojson"), layer = "", driver = "GeoJSON")
  
  