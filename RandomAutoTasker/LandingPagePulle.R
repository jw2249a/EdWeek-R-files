library(XML)
library(RCurl)
x <- 1
while (x < state.name) {
  w <- htmlParse(paste0("http://www.p.edweek.org/ew/qc/2017/state-highlights/2017/01/04/",  tolower(state.name[x]), "-state-highlights-report-page.html"))
  plain.text <- xpathSApply(w, "//p", xmlValue)
  y <- plain.text[4:19]
  
  write(y, paste0(state.name[x], ".txt"))
  x <- x+1
}