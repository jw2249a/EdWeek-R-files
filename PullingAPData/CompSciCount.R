setwd("C://Users/jw2249a/Desktop/States")
years <- c("06", "07", "08", "09", as.character(10:16))
w <- data.frame(1)
y <- 1
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
  y <- 1
  counter <- counter+1
}
w <- w[, 2:551]
write.csv(w, "CompsciCount.csv")
