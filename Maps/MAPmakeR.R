"""NOT very serious
mostly testinf out data creation here
also i should use more for loops"""
library(foreign)
library(ggplot2)
library(RColorBrewer)
library(Quandl)
library(plyr)
library(maps)
library(rMaps)
library(rCharts)

setwd('DataDump/')
ACSData <- read.spss('TopsAh.sav')
ACSDataFrame <- data.frame(ACSData)
options(max.print = 1000000)
options(scipen = 10)
dim(ACSDataFrame)
all_states <- map_data("state")
i <- 1
d <- NULL
while (i < 51) {
  x <- subset(ACSDataFrame, LSTATE %in% state.abb[i])
  x$LSTATE <- state.name[i]
  d <- rbind(d, x)
  i <- i+1
  if (i == 52) {
    break
  }
} 
random <- c(1:4)
fillKey <- c(rep.int(random, 49), 1)
d$fillKey <- fillKey

x <- NULL
d$region <- tolower(d$LSTATE)
d$region <- as.factor(d$region)
options(rcharts.cdn = TRUE)
dat <- d
dat <- transform(d,
               State = state.abb[match(as.character(LSTATE), state.name)],
               fillKey = cut(fillKey, quantile(fillKey, seq(0, 1, 1/4)), labels = 1:4),
               MEMBER = as.numeric(MEMBER, 1, 4))
fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'red'),
  c(LETTERS[1:5], 'defaultFill')
)
dat2 <- dlply(na.omit(dat), "MEMBER", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'LSTATE')
  return(y)
})
options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
map


