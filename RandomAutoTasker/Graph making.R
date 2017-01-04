setwd("C://Users/jw224/Desktop/States")
years <- c("06", "07", "08", "09", as.character(10:16))
counter <- 2
while (counter < length(state.name)+1){
  y <- 1
  w <- NULL
  w$Score <- c("5", "4", "3", "2", "1", "Total")
  CalcABTot <- 1:10
  CalcABPass <- 1:10
  CalcBCTot <- 1:10
  CalcBCPass <- 1:10
  CompSciPass <- 1:10
  CompSciTot <- 1:10
  StatTot <- 1:10
  StatPass <- 1:10
  Year <- 1:10
  while (y < 12) {
    x <- read.csv(paste0(state.name[counter], "APData", years[y], ".csv"), stringsAsFactors = FALSE)
    names(x) <- c("1", "Score", paste0("CalculusAB 20", years[y]), paste0("CalculusBC 20", years[y]), paste0("Compsci 20", years[y]),paste0("Statistics 20", years[y]))
    x$Score <- c("NA", "5", "4", "3", "2", "1", "Total")
    x <- x[2:7, 2:6]
    c <- 2
    
   
    
    while (c <= length(x)) {
    x[, c] <- as.numeric(x[, c])
    c <- c+1
    }
  
    
    CalcABTot[y] <- x[6, 2]
    CalcABPass[y] <- sum(x[1:3, 3])/x[6, 2]
    CalcBCTot[y] <- x[6, 3]
    CalcBCPass[y] <- sum(x[1:3, 3])/x[6, 3]
    CompSciPass[y] <- x[6, 4]
    CompSciTot[y] <- sum(x[1:3, 4])/x[6, 4]
    StatTot[y] <- x[6, 5]
    StatPass[y] <- sum(x[1:3, 5])/x[6, 5]
    Year[y] <- paste0("20", years[y])
    
    
    y <- y+1
    if (y == 11) {
    data <- data.frame(Year, CalcABTot, CalcABPass, CalcBCTot, CalcBCPass, CompSciPass, CompSciTot, StatTot, StatPass)
    assign(paste0(state.name[counter], "forGraph"), data)
    }
  }
  counter <- counter+1
}
library(ggplot2)
library(ggiraph)
`West VirginiaforGraph`
ggp <- ggplot(`West VirginiaforGraph`, aes(x = Year, y = CalcABPass, size = CalcABPass, color = CalcBCTot))+geom_point()
gg1 <- ggp + geom_point_interactive(aes(data_id = CalcABTot, tooltip = CalcABTot), size = 7)
ggiraph(code = print(gg1), hover_css = "fill:white; opacity:.5")
