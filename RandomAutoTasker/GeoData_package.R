library(rgdal)
library(plyr)
library(sp)
library(rgeos)
library(geojsonio)
library(stringi)
library(stringr)

download_geo <- function(url){
xurl <- data.frame(str_extract_all(url, ""), stringsAsFactors = FALSE)
filetype <- stri_sub(url, max(which(xurl == "."))+1, length(xurl[, 1]))
nameit <- readline(prompt = "Do you want to name file (y) or have it default to 'DownloadedFile'?(n) (y/n): ")
  if (nameit == "y" | nameit == "yes"){
    named <- readline(promp="What do you want to name the file? ")
  }
  if (nameit != "y" && nameit != "yes") {
    named <- "DownloadedFile"
  }
downdest <- download.file(url, paste0(named, stri_sub(url, max(which(xurl == ".")), length(xurl[, 1]))))



if (filetype == "zip"){
  wd <- paste0(getwd(),"/", named, ".zip")
  files <- unzip(wd, list = TRUE)
  num <- which(grepl(".shp", files$Name))
  mun <- which(grepl("json", files$Name))
  if (sum(num) < 1 && sum(mun) < 1) stop('zipped folder not reading shp or geojson file')
  unzip(wd, junkpaths = TRUE)
  xfiles <- data.frame(str_extract_all(files[1, 1], ""), stringsAsFactors = FALSE)
  filetype <-stri_sub(files[1,1], max(which(xfiles == "."))+1, length(xfiles[, 1]))
  named <- stri_sub(files[1,1], 1, max(which(xfiles == "."))-1)
}
if (filetype == "geojson" | filetype == "json"){
  mapdata <<- geojson_read(paste0(named, ".", filetype), what = "sp")
}
if (filetype == "shp" | filetype == "dbf" | filetype == "cpg" | filetype == "shx" | filetype == "sbn") {
  mapdata <<- readOGR(getwd(), named)
}

}

import_geo <- function(geodata) {
  xgeo <- data.frame(str_extract_all(geodata, ""), stringsAsFactors = FALSE)
  filetype <- stri_sub(geodata, max(which(xgeo == "."))+1, length(xgeo[, 1]))
  named <- stri_sub(geodata, max(which(xgeo == "\\")+1),max(which(xgeo == "."))-1)
  if (filetype == "zip") {
    wd <- paste0(geodata)
    files <- unzip(wd, list = TRUE)
    num <- which(grepl(".shp", files$Name))
    mun <- which(grepl("json", files$Name))
    if (sum(num) < 1 && sum(mun) < 1) stop('zipped folder not reading shp or geojson file')
    unzip(wd, junkpaths = TRUE)
    if (sum(num > 1)) {
      loc <- files[num, 1]
    }
    if (sum(mun > 1)) {
      loc <- files[mun, 1]
    }
    xfiles <- data.frame(str_extract_all(loc, ""), stringsAsFactors = FALSE)

    filetype <-stri_sub(loc, max(which(xfiles == "."))+1, length(xfiles[, 1]))
    named <- stri_sub(loc, 1, max(which(xfiles == "."))-1)
  }
  if (filetype == "geojson" | filetype == "json") {
    mapdata <<- geojson_read(geodata, what = "sp")
  }
  if (filetype == "shp" | filetype == "dbf" | filetype == "cpg" | filetype == "shx" | filetype == "sbn") {
    mapdata <<- readOGR(getwd(), named)
  }
}
environment(download_geo) <- asNamespace("GeoData")
environment(import_geo) <- asNamespace("GeoData")

