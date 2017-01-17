# load packages into R
library(maptools)
library(mapproj)
library(rgdal)
library(foreign)
library(plyr)
library(sp)
library(ggplot2)
library(RColorBrewer)


#input url of map api 
url <- "http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip"

#unzip it
content <- download.file(url, "CCD1314.zip")
wd <- paste0(getwd(),"/CCD1314.zip")
unzip(wd, junkpaths = TRUE)

# Read in Map Data and subsequent Data for join
shpdata <- readOGR(getwd(), "District_Level_Common_Core_of_Data_20132014")
spssdata <- data.frame(read.spss("C://Users/jw2249a/Desktop/foldeR/DataDump/RestSec.sav"))

# categories for spssdata
spssdata$PERCENT[spssdata$PERCENT >= .8] <- "i"
spssdata$PERCENT[spssdata$PERCENT == 0] <- "a"
spssdata$PERCENT[spssdata$PERCENT > 0 & spssdata$PERCENT < .01] <- "b"
spssdata$PERCENT[spssdata$PERCENT >= .01 & spssdata$PERCENT < .02] <- "c"
spssdata$PERCENT[spssdata$PERCENT >= .02 & spssdata$PERCENT < .05] <- "d"
spssdata$PERCENT[spssdata$PERCENT >= .05 & spssdata$PERCENT < .1] <- "e"
spssdata$PERCENT[spssdata$PERCENT >= .1 & spssdata$PERCENT < .2] <- "f"
spssdata$PERCENT[spssdata$PERCENT >= .2 & spssdata$PERCENT < .5] <- "g"
spssdata$PERCENT[spssdata$PERCENT >= .5 & spssdata$PERCENT < .8] <- "h"
spssdata$PERCENT[is.na(spssdata$PERCENT)] <- "MA"

# Transform to Albers Projection
shpdata2 <- spTransform(shpdata, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# get rid of extraneous info 
shpdata2@data <- shpdata2@data[, c("OBJECTID", "GEOID", "STATE2")]
shpdataframe <- shpdata2@data

# create merge column
shpdataframe$GEOID <- as.character(shpdataframe$GEOID)
spssdata <- data.frame(as.character(spssdata$LEAID), spssdata$PERCENT)
names(spssdata) <- c("GEOID", "Percent")

#join columns by Geoid
cleanDat <- join(shpdataframe, spssdata, by="GEOID", match = "all")

# set rownames to original data to join back to spatial data
rownames(cleanDat) <- 0:(length(cleanDat$OBJECTID)-1)

# join together Spatial data
shpdata2 <- SpatialPolygonsDataFrame(shpdata2, data = cleanDat)

# Moving alaska in projection
alaska <- shpdata2[shpdata2$STATE2 == "02", ]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(shpdata2)

# Move hawaii in projection
hawaii <- shpdata2[shpdata2$STATE2 == "15", ]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5300000, -1600000))
proj4string(hawaii) <- proj4string(shpdata2)

# rejoin Hawaii and Alaska to Map
shpdata2 <- shpdata2[!shpdata2$STATE2 %in% c("02", "15", "72"), ]
shpdata2 <- rbind(shpdata2, alaska, hawaii)

# Prepare map to be map by converting to dataframe
map <- fortify(shpdata2, region = "GEOID")

# rejoin initial data for mapping
shpdata2@data$id <- shpdata2@data$GEOID
map2 <- join(map, shpdata2@data, by="id")

# get url and download state map and repeat process
url <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip"
content <- download.file(url, "cb_2015_us_state_20m.zip")
wd <- paste0(getwd(), "/cb_2015_us_state_20m.zip")
unzip(wd, junkpaths = TRUE)
statedata <- readOGR(getwd(), "cb_2015_us_state_20m")

# Transform to Albers Projection
statedata2 <- spTransform(statedata, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# Move Alaska for projection
alaska <- statedata2[statedata2$GEOID == "02", ]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(statedata2)

# Move Hawaii for Projection
hawaii <- statedata2[statedata2$GEOID == "15", ]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5300000, -1600000))
proj4string(hawaii) <- proj4string(statedata2)

# join Alaska and Hawaii to statemap
statedata2 <- statedata2[!statedata2$GEOID %in% c("02", "15", "72"), ]
statedata2 <- rbind(statedata2, alaska, hawaii)

# prepare for projection
statemap <- fortify(statedata2)

# export map as csv to just load it in next time
write.csv(map2, "map2.csv")
write.csv(statemap, "statemap.csv")

# ggplot it
gg <- ggplot() 

gg <- gg + geom_map(data=map2, map=map2,
                    aes(x=long, y=lat, map_id=id, group=GEOID, fill=Percent),
                         color="white", size = .2)

gg <- gg + scale_fill_manual(breaks = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "MA"),
                              labels = c("0", "0 to 1", "1 to 2", "2 to 5", "5 to 10", "10 to 20", "20 to 50", "50 to 80", "80 to 100", "NA"),
                             values = c("#D2F3FA", brewer.pal(8, "YlOrRd"), "grey50"), na.value = "grey50", 
                             name="% SPECED Restraint and Seclusion\n(per 100 students)")
gg <- gg + geom_map(data = statemap, map = statemap, aes(x = long, y = lat, map_id = id, group = group), color = "#D3D3D3", size = 1.5, fill = NA)
gg <- gg + theme_classic() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   plot.background = element_rect(fill = "#f6ebc4"),
                                   panel.background = element_rect(fill = "#f6ebc4"),
                                   legend.title = element_text(size = 25, face = "bold"),
                                   legend.text = element_text(size = 20), 
                                   legend.background = element_rect(fill = "#f6ebc4"))

gg

