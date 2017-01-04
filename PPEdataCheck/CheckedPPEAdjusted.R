library(foreign)
# load Spss doc
PPEadjusted <- read.spss(file.choose())
# put it into data frame
PPEadjusted <- data.frame(PPEadjusted)
# take out US total from analyses
newPPEadjusted <- rbind(PPEadjusted[1:44,], PPEadjusted[46:52,])

# create new output to check against old 

Check <- NULL

# transfer over states and unadjusted PPE and CWI 14

Check$State <- newPPEadjusted$State
Check$UnadjustedPPE <- newPPEadjusted$UnadjustedPerPupil
Check$CWI14 <- newPPEadjusted$ECWI2014

# run old analysis 

Check$AdjustedPPEExpenditures <- (newPPEadjusted$UnadjustedPerPupil/newPPEadjusted$ECWI2014)
OldUSAggregate <- mean(Check$AdjustedPPEExpenditures)
oldUStotal <- sum(Check$AdjustedPPEExpenditures)

# run new analysis 

Check$adj1 <- Check$UnadjustedPPE/Check$CWI14
newUSTotalExpenditure <- sum(Check$UnadjustedPPE)
newUSTotalCWI <- sum(Check$CWI14)
newUSadj1 <- newUSTotalExpenditure/newUSTotalCWI
newUSCWI <- 1.5519
Check$NewAdjustedPPEExpenditures <- Check$adj1*newUSCWI
newUSAdjustedExpenditures <- newUSadj1*newUSCWI

# alternative analysis
"""So this is an analysis based on the idea that we should the previous way weighted all of the states towards the US average before aggregating them,
which I think mildly affected the outcome. I thought it would be better here to wait until after adding the states together to use the national CWI, 
so we can correctly weight the states instead of (as seen in the results), weighting down the bigger states with the mean CWI individually """

altUSTotalExpenditure <- sum(adj1)
altUSAdjustedTotalExpenditure <- altUSTotalExpenditure*newUSCWI
altUSAdjustedAverageExpenditure <- altUSAdjustedtotalExpenditure/51

# create data frames and export to csv

newUSAnalysis <- data.frame("US", newUSTotalExpenditure, newUSTotalCWI, newUSadj1, newUSCWI, newUSAdjustedExpenditures)
altUSAnalysis <- data.frame("US", altUSTotalExpenditure, altUSAdjustedTotalExpenditure, altUSAdjustedAverageExpenditure)
Check <- data.frame(Check)
write.csv(Check, "CheckedPPEAdjusted.csv")
write.csv(newUSAnalysis, "newCheckedPPEAdjusted.csv")
write.csv(altUSAnalysis, "altCheckedPPEAdjusted.csv")
write.csv(c(Check, newUSAnalysis, altUSAnalysis), "newnewCheckedPPEAdjusted.csv")
