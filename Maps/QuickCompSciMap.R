
library(ggplot2)
library(plyr)
states <- map_data("state")

statedata <- read.csv("States/CompsciRates.csv")
statedata$region <- tolower(as.character(statedata[, 2]))

newstates <- join(states, statedata, by="region")
newstates$change <- as.numeric(as.character(newstates$change))
newstates <- newstates[newstates$region!="district of colombia", ]
p <- ggplot()
p <- p + geom_polygon(data=newstates, aes(x=long, y=lat, group = group, fill=change),colour="white"
) + scale_fill_continuous(low = "red", high = "blue", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Change in Compsci Test-Taking /n Per 10,000 Students by State" 
                             ,title = "Computer Science Change 2010-2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

