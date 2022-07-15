rm(list=ls())
library(XML)
library(tidyverse)
library(dplyr)
library(epiDisplay)

file_url <- "p828932869599.xml"
xmlfile <- xmlParse(file_url, encoding="UTF-8")

nodes <- getNodeSet(xmlfile, "//Single | //Multi | //Open | //Grid")



# Spørsmål - lag tibble med div info
info_tbl <- tibble(
  name = map_chr(nodes, function(x) xmlValue(getNodeSet(x, "Name"))),
  q_type = map_chr(nodes, xmlName),
  var_type = map_chr(nodes, function(x) xmlAttrs(x)["VariableType"]), # NA = "Normal" (Ordinære spørsmål)
  text = map_chr(nodes, function(x) xmlValue(getNodeSet(x, "FormTexts/FormText/Text")[1]))
)


# Svaralternativer
answerpath <- ".//SingleAnswers/Answer | .//MultiAnswers/Answer | .//Grid3DAnswers/Answer | .//GridAnswers/GridAnswer"

svar_tbl <- map(nodes,
                function(x) {
                  tibble(
                    value = map_chr(getNodeSet(x, answerpath), function(y) xmlAttrs(y)["Precode"]),
                    label = as.character(xmlValue(getNodeSet(x, answerpath))),
                  )
                }
)



# sett sammen tibles med info + svar (kolonnen svar_tbl er en liste med tibbles)
q_nested <- tibble(info_tbl,svar_tbl)


# search/retrieve question by question name
get_question <- function(question_name){
  search_q <- filter(q_nested, name==question_name)
  
  # hvis man ikke vil bruke entityid som søketreff: 
  #search_q <- slice(q_nested, search_id) # søker etter radnummer
  
  print(search_q) # print question info
  print(deframe(x = search_q$svar_tbl)) # convert nested tibble to printable format
  
}
#filter(q_nested, name=="background_county")
search_name <- "background_county" # name for question we want to retrieve
get_question(search_name)


# print frequency table and create plot
# if figure margins too large error, expand plot panel size in RStudio UI
tab1(q_nested$q_type, sort.group = "decreasing", cum.percent = TRUE, graph=TRUE)
