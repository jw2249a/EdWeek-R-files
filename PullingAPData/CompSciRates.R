setwd("States")
count <- read.csv("CompsciCount.csv")

count <- count[order(count$State), ]

pop <- read.csv("PopData.csv")

names(count) <- c("State", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

popPer1000 <- pop

popPer1000[, 1] <- pop[, 1]

popPer1000 <- popPer1000[c(1:8, 10:51), 1:7]

names(popPer1000) <- c("State", "2015", "2014", "2013", "2012", "2011", "2010")
popPer1000 <- popPer1000[order(popPer1000$State), ]

rate2009.10 <- count$"2010"/popPer1000$"2010"*1000
rate2010.11 <- count$"2011"/popPer1000$"2011"*1000
rate2011.12 <- count$"2012"/popPer1000$"2012"*1000
rate2012.13 <- count$"2013"/popPer1000$"2013"*1000
rate2013.14 <- count$"2014"/popPer1000$"2014"*1000
rate2014.15 <- count$"2015"/popPer1000$"2015"*1000

rates <- data.frame(popPer1000$State, rate2009.10, rate2010.11, rate2011.12, rate2012.13, rate2013.14, rate2014.15)

rates$change <- rates$rate2014.15-rates$rate2009.10

rates$PercChange <- (rate2014.15-rates$rate2009.10)/rates$rate2009.10

write.csv(rates, "CompsciRates.csv")
