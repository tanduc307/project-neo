# install.packages("pdftools")

library(pdftools)
library(tidyverse)

url <- c("912B - Sorgum HT.pdf")
# url <- c("http://www.cicad.oas.org/oid/pubs/JamaicaNationalHouseholdDrugSurvey2017ENG.pdf")
url 
raw_text <- purrr::map(url, pdf_text)

View(raw_text)

class(raw_text)

str(raw_text)

raw <- purrr::map(raw_text, ~str_split(.x, "\\n") |> unlist())
    
raw <- reduce(raw_text, c)

class(raw)

table_start <- stringr::str_which(tolower(raw), "moisture")


table_end <- stringr::str_which(tolower(raw), "zinc")



table_end <- table_end[min(which(table_end > table_start))]
    
    # build
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2, }", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|")
    
    data_table
    
}

results <- purrr::map_df(raw_text, clean_table1)
head(results)
