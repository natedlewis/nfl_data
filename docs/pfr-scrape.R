# packages
library(glue)
library(rvest)
library(xml2)

https://www.pro-football-reference.com/fantasy/QB-fantasy-matchups.htm#

gen_tables <- function(year) {
  
  url <- "https://www.pro-football-reference.com/fantasy/QB-fantasy-matchups.htm"
  
  url2 <- "/fantasy.htm"
  
  url <- paste(url1, year, url2, sep = "")
  
  url_p <- read_html(url)
  table_year <- html_table(url_p, header = TRUE, fill = TRUE)
  
  
  data <- table_year[[1]]
  
  names(data) <- paste(names(data), data[1, ], sep = "_")
  data <- data[-1,]
  colnames(data)[1:5] <- c("RK", "Player", "Team", "FantPos", "Age")
  
  data <- data[!data$RK == "Rk",]
  
  return(data)
}

