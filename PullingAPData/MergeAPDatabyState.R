setwd("C://Users/jw2249a/Desktop/States")
years <- c("06", "07", "08", "09", as.character(10:16))
counter <- 1
while (counter < length(state.name)+1){
  y <- 1
  w <- NULL
  w$Score <- c("5", "4", "3", "2", "1", "Total")
  v <- NULL
  v$Score <- c("5", "4", "3", "2", "1", "Total")
  while (y < 12) {
  x <- read.csv(paste0(state.name[counter], "APData", years[y], ".csv"), stringsAsFactors = FALSE)
  names(x) <- c("1", "Score", paste0("CalculusAB 20", years[y]), paste0("CalculusBC 20", years[y]), paste0("Compsci 20", years[y]),paste0("Statistics 20", years[y]))
  x$Score <- c("NA", "5", "4", "3", "2", "1", "Total")
  x <- x[2:7, 2:6]
  
  
  c <- 2
  z <- x
  while (c <= length(names(x))) {
  x[, c] <- as.numeric(x[, c])
  z[, c] <- (x[, c]/x[6, c])*100
  names(z)[c] <- paste0(names(x[c]), "%")
  c <- c+1
  }
  z$Score <- c("5", "4", "3", "2", "1", "Total")
  w <- merge(w, x, by="Score")
  v <- merge(v, z, by="Score")
  w <- merge(w, v, by="Score")
  y <- y+1
  
  if (y == 12) {
    write.csv(w, paste0(state.name[counter], "_APDataTotal.csv"))
    write.csv(v, paste0(state.name[counter], "_APDataPercent.csv"))
  }
  }
  counter <- counter+1
}
