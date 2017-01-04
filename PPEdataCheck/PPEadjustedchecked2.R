library(foreign)
# load Spss doc
PPEadjusted2014 <- read.spss(file.choose())
#and csv
PPEadjusted2013 <- read.csv(file.choose())
# put it into data frame
PPEadjusted2014 <- data.frame(PPEadjusted2014)

# rename NCESID column
colnames(PPEadjusted2013)[1] <- "NCESID"

# subset columns by NCESID 
PPEadjusted <- merge(PPEadjusted2014, PPEadjusted2013, by="NCESID")

dimnames(PPEadjusted)

# keep only variables you need

PPEadjusted <- PPEadjusted[, c(1, 2, 5, 7, 11)]

# rename and reorder ugly variables

colnames(PPEadjusted) <- c("NCESID", "DISTRICT", "ADJPPE2014", "State", "ADJPPE2013")

PPEadjusted <- PPEadjusted[, c(5, 1, 2, 4, 3)]

# calculate change in variables

PPEadjusted$percchange <- ((PPEadjusted$ADJPPE2014-PPEadjusted$ADJPPE2013)/PPEadjusted$ADJPPE2013*100)

# z scores
totSD <- sd(PPEadjusted$percchange, na.rm = T)*sqrt((length(PPEadjusted$percchange)-1)/(length(PPEadjusted$percchange)))
totAvg <- mean(PPEadjusted$percchange, na.rm = T)

z <- function(x) (x-totAvg)/totSD

PPEadjusted$zscoreraw <- apply(PPEadjusted[, "percchange", drop=FALSE], 1, z)

# subset and export 

outlierstotal <- subset(PPEadjusted, zscoreraw >=3 | zscoreraw <= -3)

write.csv(outlierstotal, "National_Outliers.csv")

# now by state 
i = 1
stateOutliers <- NULL
while (i < 52) { 
  
  statesub <- subset(PPEadjusted, State %in% state.abb[i])
  
  # z scores
  totSD <- sd(statesub$percchange, na.rm = T)*sqrt((length(statesub$percchange)-1)/(length(statesub$percchange)))
  totAvg <- mean(statesub$percchange, na.rm = T)
  
  z <- function(x) (x-totAvg)/totSD
  
  statesub$zscoreraw <- apply(statesub[, "percchange", drop=FALSE], 1, z)
  
  # subset and export 
  x <- subset(statesub, zscoreraw >=3 | zscoreraw <= -3)
  assign(paste0("outliers", i), x)
  
  
  stateOutliers <- rbind(stateOutliers, x)
  
  i <- i+1
 
  } 
write.csv(stateOutliers, "State_Outliers.csv")
