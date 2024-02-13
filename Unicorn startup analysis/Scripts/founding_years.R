#Loading required libraries

library(rvest)
library(tidyverse)
library(dplyr)

# Scraping data from website

html = read_html('https://inc42.com/the-indian-unicorn-tracker/')
table = html %>% html_table()
table = table[[1]]

table<-table[-111,]

# Saving Indian Origin Startups data in csv 

write.csv(table,file="india_found")

# Scraping data for United-States

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states")
table=html%>%html_table()
table=table[[2]]
table
vec=c(1:12)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states")
table=html%>%html_table()
table=table[[2]]

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-2")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-3")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-4")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-5")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-6")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-7")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-8")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-9")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-10")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-11")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-states-page-12")
table1=html%>%html_table()
table=rbind(table,table1[[2]])

# Saving Data in Csv format

write.csv(table,file="usa_startup.csv")

# Scraping data for China

vec=c(2:6)
links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-china-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-china")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in Csv format

write.csv(table,"china_startup.csv")

# Scraping data for UK

vec=c(2:6)
links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-united-kingdom-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-united-kingdom")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in CSV format

write.csv(table,"uk_startup.csv")

# Scraping data for ISRAEL

links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-israel-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-israel")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in Csv format

write.csv(table,"israel_startup.csv")

# Scraping data for Germany

links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-germany-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-germany")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in CSV format

write.csv(table,"germany_startup.csv")

# Scraping data for France

links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-france-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-france")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in CSV format

write.csv(table,"france_startup.csv")

# Scraping data for South-Korea

links=paste0("https://tracxn.com/d/unicorn-corner/unicorns-list-south-korea-page-",vec)

html=read_html("https://tracxn.com/d/unicorn-corner/unicorns-list-south-korea")
table=html%>%html_table()
table=table[[2]]
for(link in links){
  html=read_html(link)
  table1=html%>%html_table()
  table=rbind(table,table1[[2]])
}

# Saving Data in CSV format

write.csv(table,"south-korea_startup.csv")
