# download files
# Did This manually because it was a small task but definitely will use XML next time 
# because the lack of organization on collegeboards website made this take 2long4me
library(readxl)
setwd("C://Users/jw2249a/Desktop/States")
i <- 1
y <- 11
years <- c("06", "07", "08", "09", as.character(10:16))
while (y > 0) {
while (i < length(state.name)+1) {
  if (sum(as.numeric(y == c(10, 11))) > 0) {
  w <- paste0(tolower(gsub(" ", "-", state.name[i])),"-summary-20", years[y], ".xlsx")
 
  url <- paste0("https://secure-media.collegeboard.org/digitalServices/misc/ap/", w)
  content <- download.file(url, w, mode = "wb")
  }
  if (y == 9) {
    
    url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/2014/", gsub(" ", "_", state.name[i]), "_Summary.xlsx")
                  w <- paste0(gsub(" ", "_", state.name[i]),"_Summary_",  years[y], ".xlsx")
                  content <- download.file(url, w,  mode = "wb")
  }
  if (sum(as.numeric(y == c(7, 8))) > 0) {
    
    url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/2013/", gsub(" ", "_", state.name[i]), "_Summary_13.xls")
    w <- paste0(gsub(" ", "_", state.name[i]), "_Summary_", years[y], ".xls")
    content <- download.file(url, w, mode = "wb")
  }
  if (sum(as.numeric(y == c(4, 5, 6))) > 0) {
    
    url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/20", years[y], "/", toupper(gsub(" ", "_", state.name[i])), "_Summary_", years[y], ".xls")
    w <- paste0(toupper(gsub(" ", "_", state.name[i])), "_Summary_", years[y], ".xls")
    content <- download.file(url, w, mode = "wb")
  }
  if (y == 3) {
    if (i != 37) {
    url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/20", years[y], "/", toupper(gsub(" ", "-", state.name[i])), "_Summary_", years[y], ".xls")
    }
    if (i == 37) {
      url <- "http://media.collegeboard.com/digitalServices/pdf/research/2008/OREGON_Summar_08y.xls"
    }
      w <- paste0(toupper(gsub(" ", "-", state.name[i])), "_Summary_", years[y], ".xls")
      content <- download.file(url, w, mode = "wb")
    }
    if (y == 2) {
      
      url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/20", years[y], "/", toupper(gsub(" ", "-", state.name[i])), "_Summary.xls")
      w <- paste0(toupper(gsub(" ", "_", state.name[i])), "_Summary_", years[y], ".xls")
      content <- download.file(url, w, mode = "wb")
    }
  if (y == 1) {
    url <- paste0("http://media.collegeboard.com/digitalServices/pdf/research/", toupper(gsub(" ", "-", state.name[i])), "_Summary_20", years[y], ".xls")
      w <- paste0(toupper(gsub(" ", "-", state.name[i])), "_Summary_", years[y], ".xls")
      content <- download.file(url, w, mode = "wb")
    }
 
  assign(paste0(state.name[i], "APData", years[y]), read_excel(w, sheet = 1, na = "NA", skip = 4))
  x <- read_excel(w, sheet = 1, na = "NA", skip = 4)
  if (y == 11) {
    write.csv(data.frame(x[c(1,65:70), c(3, 7, 8, 11, 36)]), paste0(state.name[i], "APData", years[y], ".csv")) 
  }
  if (y == 10) {
    write.csv(data.frame(x[c(1,65:70), c(3, 7, 8, 11, 35)]), paste0(state.name[i], "APData", years[y], ".csv")) 
  }
  if (sum(as.numeric(y == c(7, 8, 9))) > 0) {
  write.csv(data.frame(x[c(1,65:70), c(3, 7, 8, 11, 33)]), paste0(state.name[i], "APData", years[y], ".csv"))
  }
  if (sum(as.numeric(y == c(5, 6))) > 0) {
    write.csv(data.frame(x[c(1,64:69), c(2, 6, 7, 10, 31)]), paste0(state.name[i], "APData", years[y], ".csv"))
  }
  if (sum(as.numeric(y == c(2, 3, 4))) > 0) {
    write.csv(data.frame(x[c(1, 64:69), c(2, 6, 7, 10, 35)]), paste0(state.name[i], "APData", years[y], ".csv"))
  }
  if (y == 1) {
    write.csv(data.frame(x[c(1, 64:69), c(2, 6, 7, 10, 33)]), paste0(state.name[i], "APData", years[y], ".csv"))
  }
  
  i <- i+1
}
y <- y-1
i <- 1
}
