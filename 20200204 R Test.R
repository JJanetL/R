library(readxl)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(data.table)

#import data
data <- read_excel("~/Desktop/310104.xls",sheet = 'Data1', na = "empty")

#keep only last 10 year data
data <- tail(data,40)

# set column names to call it later
colnames(data)[1] <- "Quarter"

# convert date to date format
data$date <- convertToDate(data$Quarter)

#remove first column
data <- select (data,-c(1))

#keep only male data
male_data <- data[ , grepl( "Male" , names( data ) ) ]

# transfer data into numeric
c_data <- data.frame(male_data[, c(1:(ncol(male_data)))] <- sapply(male_data[, c(1:(ncol(male_data)))], as.numeric))

pct <- function(x) {(x/lag(x)-1)}

#calculatet the percentage
pctg <- c_data[,1:(ncol(c_data)-1)] %>% 
        mutate_each(funs(pct))

#add on quater data
row.names(pctg) <- data$date

# find the max value (percentage)
max_num<-max(pctg, na.rm = TRUE)

#find the max value each column
max_column <- apply(pctg, 2, function(x) max(x, na.rm = TRUE))

# remove head or tail whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# find State Name
State <- trim(last(unlist(strsplit(colnames(data)[which(grepl(max_num, max_column))],";"))))

# find row index for states name, then identify the row name
col_num <- which(grepl(max_num, max_column))
From_Date <- rownames(pctg)[which(pctg[ , col_num] == max_num)-1]
To_Date <- rownames(pctg)[which(pctg[ , col_num] == max_num)]


# Pint percentage with 4 decimals
percent <- function(x, digits = 4, format = "f", ...) {
        paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Show the required result
print(paste0(State," has the highest quarterly change rate for its male population between ",From_Date," and ",To_Date, " across all states in Australia, during which its male population has increased by ",percent(max_num),"."))
      
