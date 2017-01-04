
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


# now the shapefile

OGRdata <- readOGR(paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014TopoFULL", ".geojson"), "OGRGeoJSON")

OGRdata1 <- readOGR(paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014FULL", ".geojson"), "OGRGeoJSON")

OGRdata1@data$id <- rownames(OGRdata1@data)

OGRdataframe <- subset(OGRdata1@data, id %in% OGRdata@data$id)

OGRdata@data <- OGRdataframe


  
  # write a geojson file 
  
  writeOGR(OGRdata, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014theFullness", ".geojson"), layer = "", driver = "GeoJSON")
  
  # lets quantile the data now for coloring
  
  quants <- round(quantile(spssdata$ADJPPE, probs = seq(0, 1, 0.20), na.rm = TRUE), 0)
  quants[1]<-0
  # Lets get the look down
  
  hoverover <- c("DISTRICTNAME", "ADJPPE")
  colouryform <- c("#17414B", "#5AB2C5", "#F7969C","#BD4F4D", "#7A2425")
  style <- styleGrad(prop="ADJPPE", breaks=c(quants), right=FALSE, style.par = "col",
                     style.val = rev(colouryform), fill.alpha = 1, leg = "Per-Pupil Expenditures", lwd = 2, alpha = 1, col = "white")
  
  # creates the map
  
  map <- leaflet(data = paste0("MapFolder/ShapeFile/District_Level_Common_Core_of_Data_20132014theFullness", ".geojson"), dest = "MapFolder/Shapefile",
                 title = paste0("index ", i), base.map="osm",
                 incl.data = TRUE, style = style,  popup = hoverover)
  
  






