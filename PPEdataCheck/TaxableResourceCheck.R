library(foreign)

# import document 

taxableresources <- read.spss(file.choose())

# get rid of DC 

taxableresources <- data.frame(taxableresources)
taxes <- rbind(taxableresources[1:8,], taxableresources[10:51,])

# original analysis
# setting scientific notation off cause its dumb
options(scipen = 10)
# load in base data and multiply it by multiplier
Check <- NULL
Check$state <- taxes$Area
Check$GDP <- taxes$GDP
Check$MilGDP <- Check$GDP*1000000
Check$Total <- taxes$Total
Check$Federal <- taxes$Federal
Check$thousTotal <- Check$Total*1000
Check$thousFederal <- Check$Federal*1000

# find Taxable resource indicator and old mean of indicator

Check$TotalminusFederal <- Check$thousTotal-Check$thousFederal
Check$TaxableResourceIndicator <- Check$TotalminusFederal/Check$MilGDP*100
OldAverage <- mean(Check$TaxableResourceIndicator)

#new Analysis

TotalDiffTotalFed <- sum(Check$TotalminusFederal)
TotalGDP <- sum(Check$MilGDP)
NationalResourceIndicator <- TotalDiffTotalFed/TotalGDP*100

# data frame and export
Check <- data.frame(Check)
analysis <- data.frame("US", OldAverage, TotalDiffTotalFed, TotalGDP, NationalResourceIndicator)
write.csv(c(Check, analysis), "TaxableResourceCheck.csv")
