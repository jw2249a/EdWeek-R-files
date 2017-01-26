
setwd("C:/Users/jw2249a/Desktop/Education Companies")
dirlist <- dir()
Tweets <- list()
Following <- list()
Followers <- list()
Lists <- list()
y <- 1

for (i in dirlist) {
  x <- read.csv(i, as.is = TRUE)[0:4, 1]
  header <- gsub(".csv", "", i)
  l <- 1
  while (l <= length(x)) {
  x[l] <- substr(x[l], as.numeric(gregexpr("n", x[l]))+1, nchar(x[l])-1)
  l <- l+1
  }
  print(x)
  Tweets <- rbind(Tweets, x[1])
  Following <- rbind(Following, x[2])
  Followers <- rbind(Followers, x[3])
  Lists <- rbind(Lists, x[4])
  names(Tweets)[y] <- header
  rownames(Following)[y] <- header
  rownames(Followers)[y] <- header
  rownames(Lists)[y] <- header
  y <- y+1
}
output <- data.frame(rownames(Followers), Followers, Following, Lists, Tweets)
colnames(output)[1] <- "Ed Company"
output <- data.frame(lapply(output, as.character), stringsAsFactors = FALSE)
output[output=="b'"] <- NA
write.csv(output, "EdDataformatted.csv")

