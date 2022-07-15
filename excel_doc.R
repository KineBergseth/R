rm(list=ls())
library(readxl)
library(dplyr)
library(tidyverse) 
library(epiDisplay)
source("xml_doc.R") # filepath to file containing xml code

file_url <- "p828932869599.xlsx"

# import excel sheet
dataset <- read_excel(
  file_url,
  guess_max = min(1300,n_max = NULL),
  col_types = c("guess") # guess data types based on Excel cell types 
)
#View(dataset)

# Den har fortsatt mange kolonner som inneholder kun tall
# rekursiv apply for å korrigere datatypene til korrekt type
convert_column_types <- function(x) {
  d <- rapply(x, type.convert, classes = "character", how = "replace", as.is = TRUE)
  return(d)
}

#sapply(dataset, class) # for å se orginale kolonne typer
#korriger datatyper
dataset_types <- lapply(convert_column_types(dataset), class) # list with correct datatypes
#dataset_types

char_columns <- c() # store columns with char datatype
for (i in 1:length(dataset_types)){ # iterate over dataset
  if (dataset_types[i] == "character"){
    char_columns <- append(char_columns,names(dataset_types[i]))
    #print(names(dataset_types[i])) # value with name
  } 
}

#char_columns


# create tibble with only character columns
cd <- dataset %>% dplyr::select(char_columns)

# find longest string in each column
# There are NA values in some columns. if the max() function encounters NA 
# values, it will not give an output. So in order to bypass NA values, we 
# remove them with na.rm = True to get maximum value from all columns
longest_char <- lapply(cd, function(x) max(x, na.rm = T))
#longest_char

# finn det første lengste ordet av de lengste ordene vi har samlet:
longest_char[which.max(nchar(longest_char))]

# Er det flere ord av samme lengde, finner denne koden alle
longest_char[which(nchar(longest_char)==(max(nchar(longest_char))))]


# print frequency table and create plot basert på spm navn
# if figure margins too large error, expand plot panel size in RStudio UI

# denne koden bruker en funksjon fra df.R filen som er importert med source øverst

# siden sted heter background_region i excel og background_county i xml,
# bytter jeg navn, så de samsvarer
names(dataset)[names(dataset) == "background_region"] <- "background_county"



# look at background_gender question
get_question("background_gender")
tab1(dataset$background_gender, sort.group = "decreasing", cum.percent = TRUE, graph=TRUE)



#look at background_county question
# precode er ikke lik verdi på xml som i excel arket her :/
#get_question("background_county")
#tab1(dataset$background_county, sort.group = "decreasing", cum.percent = TRUE, graph=TRUE)

#look at background_county question
#get_question("background_yob")
#tab1(dataset$background_yob, sort.group = "decreasing", cum.percent = TRUE, graph=TRUE)

