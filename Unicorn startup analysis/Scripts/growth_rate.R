library(tidyverse)
library(dplyr)
library(rvest)

data = read_csv("south-korea-gdp-growth-rate.csv")

data
print(data, n = 60)

typeof(data)

df = data_frame(data)

print(df,n = 60)
df = df[59:71,1]
df = df %>% tail(10)
print(df, n = 10)
df[1,1]
strsplit(df[[1]][1],",")

typeof(df[[1]][1])


country = c("israel", "germany", "china", "united-states", "united-kingdom")
list = list(NULL)


for(c in country){
  path = paste0(c, "-gdp-growth-rate.csv")
  print(path)
  data = read_csv(path)
  data_f = data_frame(data)
  data_f = data_f %>% tail(10)
  
  df = rbind(df,data_f)
}
df = df[[1]]
df[1]
for(i in 1:70){
  list[i] = strsplit(df[i],",")
}
list

library(tibble)

my_tibble <- as_tibble(do.call(rbind, list))

# Rename columns if needed
colnames(my_tibble) <- c("year", "Growth_rate", "Annual_change")

# Print the resulting tibble
print(my_tibble, n = 70)

my_tibble <- my_tibble %>%
  mutate(group = rep(c("south-korea","india", "israel", "germany", "china", "united-states", "united-kingdom"), each = 10))


