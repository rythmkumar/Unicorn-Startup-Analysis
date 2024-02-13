library(rvest)
library(tidyverse)
library(dplyr)

#reading the website.
html = read_html("https://en.m.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)")
table = html %>% html_table()

any(duplicated(table))
table = table[[3]]

#removing duplicated entries.
if (any(duplicated(names(table)))) {
  names(table) <- make.unique(names(table), sep = "_")
}


result <- table %>%
  slice(3:n())

#renaming the columns of result.
colnames(result) = c("Country", "UN region", "IM_forecast", "IMF_year", "World Bank_forecast", "World Bank_year", "UN_forecast", "UN_year")

#Removing Taiwan since it had a missing value.
result = result %>% filter(Country != "Taiwan")
print(result, n = 50)

#Extracting data of only top 50 countries.
result = result %>% slice(1:50)
result['UN_year']
result

#Saving the data extracted and cleaned.
write.csv(result, "GDP.csv")



#Reading the website to extract information about start-ups with female co-founders.
html = read_html("https://www.failory.com/startups/female-founder-unicorns")

#var contains the names of all the companies. 
var = html %>% html_elements("h3") %>% html_text()
class(var)
var = c("", var)

# extracting other information about the companies.
info = html %>% html_elements("li") %>% html_text()
info[1333]


#creating an data-frame with "" as first row.
empty_df <- data.frame(
  Valuation = character(1),   
  Country = character(1), 
  State = character(1),
  City = character(1),
  Started = character(1),
  Founders = character(1),
  Industries = character(1),
  employees = character(1),
  Funding = character(1),
  rounds = character(1),
  investors = character(1)
)

#splitting the list into what the info is about and the info.
splitted = sapply(1:1333, function(k) str_split(info[k], ":"))

#creating a new row for each new company. 
new_row <- data.frame(
  Valuation = " ",
  Country = " ",
  State = " ",
  City = " ",
  Started = " ",
  Founders = " ",
  Industries = " ",
  employees = " ",
  Funding = " ",
  rounds = " ",
  investors = " ",
  stringsAsFactors = FALSE
)

# Populating the data frame.
j = 1
column_names <- colnames(empty_df)
for(i in 1:1333){
  
  if(grepl(column_names[1],splitted[[i]][1])){
    empty_df <- rbind(empty_df, new_row)
    j = j+1
    empty_df$Valuation[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[2],splitted[[i+1]][1]))){
      empty_df$Country[j] = " "
    }
    
    
  }
  else if(grepl(column_names[2],splitted[[i]][1])){
    empty_df$Country[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[3],splitted[[i+1]][1]))){
      empty_df$State[j] = " "
    }
  }
  
  else if(grepl(column_names[3],splitted[[i]][1])){
    empty_df$State[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[4],splitted[[i+1]][1]))){
      empty_df$City[j] = " "
    }
  }
  
  else if(grepl(column_names[4],splitted[[i]][1])){
    empty_df$City[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[5],splitted[[i+1]][1]))){
      empty_df$Started[j] = " "
    }
  }
  
  else if(grepl(column_names[5],splitted[[i]][1])){
    empty_df$Started[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[6],splitted[[i+1]][1]))){
      empty_df$Founders[j] = " "
    }
  }
  
  else if(grepl(column_names[6],splitted[[i]][1])){
    empty_df$Founders[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[7],splitted[[i+1]][1]))){
      empty_df$Industries[j] = " "
    }
  }
  
  else if(grepl(column_names[7],splitted[[i]][1])){
    empty_df$Industries[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[8],splitted[[i+1]][1]))){
      empty_df$employees[j] = " "
    }
  }
  
  else if(grepl(column_names[8],splitted[[i]][1])){
    empty_df$employees[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[9],splitted[[i+1]][1]))){
      empty_df$Funding[j] = " "
    }
  }
  
  else if(grepl(column_names[9],splitted[[i]][1])){
    empty_df$Funding[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[10],splitted[[i+1]][1]))){
      empty_df$rounds[j] = " "
    }
  }
  
  else if(grepl(column_names[10],splitted[[i]][1])){
    empty_df$rounds[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[11],splitted[[i+1]][1]))){
      empty_df$investors[j] = " "
    }
  }
  
  else if(grepl(column_names[11],splitted[[i]][1])){
    empty_df$investors[j] = splitted[[i]][2]
    if(i != 1333 & !(grepl(column_names[1],splitted[[i+1]][1]))){
      empty_df$Valuation[j] = " "
    }
  }
}
#Adding the names of company column.
empty_df = empty_df %>% mutate(company_name = var)
#Managing the order of columns 
empty_df = empty_df %>% select(c(12, 1:9, 11))

#Saving the data.
write_csv(empty_df, "Female_founders1.csv")

