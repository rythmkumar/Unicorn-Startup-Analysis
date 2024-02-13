#loading required libraries

library(rvest) 
library(tidyverse)

# Scraping data from website

html<-read_html("https://www.cbinsights.com/research-unicorn-companies")
table=html%>%html_table()
table<-table[[1]]

# Cleaning Data

value<-table[,2]
value1<-sapply(1:1230,function(k) table[k,2]%>%substring(2))
table[,2]=as.numeric(value1)
table[476,4]="Singapore"
table[551,4]="Hong Kong"

by_country=table%>%group_by(Country)%>%summarise(No_of_Unicorns=n(),`Total_Valuation($B)`=sum(`Valuation ($B)`))%>%arrange(desc(No_of_Unicorns))

# Saving in data in csv format

write.csv(table,"data_1.csv")
write.csv(by_country,"country.csv")
