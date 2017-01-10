#set a working directory and prenumbered values to grab already prepared data
setwd("C://Users/jw2249a/Desktop/States")
years <- c("06", "07", "08", "09", as.character(10:16))
w <- data.frame(1)
y <- 5
counter <- 1
i <- 2
while (counter < length(state.name)+1) {
  while (y < 12) {
    x <- read.csv(paste0(state.name[counter], "APData", years[y], ".csv"), stringsAsFactors = FALSE) 
    v <- data.frame(x[7,5])
    w <- merge(w, v, all = TRUE)
    colnames(w)[i] <- paste0(state.name[counter], years[y])
    i <- i+1
    y <- y+1
  }
  y <- 5
  counter <- counter+1
}
w <- w[, 2:351]
write.csv(w, "CompsciCount.csv")
rm(list = ls())
compscicount <- read.csv("compsciCount.csv")[ , 2:351]

no1 <- 1
no2 <- 7
NoDat <- data.frame(state.name, 1:50, 1:50, 1:50, 1:50, 1:50, 1:50, 1:50)
colnames(NoDat) <- c("State", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
x <- 1
while (no2 <= 350) {
  w <- compscicount[1, no1:no2]
  NoDat[x, 2:8] <- w
  x <- x+1
  no1 <- 7*(x-1)+1
  no2 <- 7*x
}
write.csv(NoDat, "CompsciCount.csv")