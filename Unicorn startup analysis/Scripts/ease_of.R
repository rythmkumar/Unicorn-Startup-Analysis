
## loading library

library(rvest)

## scraping data from website
html <- read_html("https://en.wikipedia.org/wiki/Ease_of_doing_business_index")
table <- html %>%  html_table()

## cleaning data 
data <- table[[2]]
data <- as.data.frame(data)
data <- data[-1, ]
data[data == "—"] <- NA

## saving data in csv format
write.csv(data, "Ease_of_doing_business_index1.csv", row.names = FALSE)
