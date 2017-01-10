setwd("C://Users/jw2249a/Desktop/States")
counter <- 1
w <- data.frame(1:46)
x <- read.csv(paste0(state.name[counter], "_APDataPercent.csv"))[3:5, ]
row.names(w) <- colnames(x)

while (counter < length(state.name)) {
  
x <- read.csv(paste0(state.name[counter], "_APDataPercent.csv"))[3:5, ]
c <- 3
z <- data.frame(1:46)
rownames(z) <- colnames(x)
while (c < length(x)+1) {
z[c, ] <- sum(x[, c])
c <- c+1
}
colnames(z) <- state.name[counter]

w <- cbind(w, z)
if (counter == 1) {
  w <- data.frame(w[!w[, 1]])
  w <- cbind(w, z)
}
counter <- counter+1
}
w <- w[3:46, ]
write.csv(w, "FinalStateData.csv")


