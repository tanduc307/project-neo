# https://www.uprm.edu/ccs-cicsa/how-to-merge-multiple-txt-files-in-r/

library(tidyverse)
library(fs)
library(dplyr)
library(stringr)
# https://stackoverflow.com/questions/4876813/using-r-to-list-all-files-with-a-specified-extension

list.files(path = "D:\\GITHUB\\project-neo\\public\\bib\\springer", 
           full.names = TRUE, 
           recursive = TRUE,
           pattern = "\\.ris$", 
           ignore.case = TRUE) -> list_ris

list.files(path = "data/", 
           full.names = TRUE, 
           recursive = TRUE,
           pattern = "\\.bib$", 
           ignore.case = TRUE) -> list_ris

ghep_file <- function(input) {
  
  ris <- readLines(con = input)
  
  return(ris)
  
}

lapply(list_ris, ghep_file) -> thanks

sapply(thanks, append, "") -> well

unlist(well) -> great

# writeLines(great, con = "great.ris")

writeLines(great, con = "great.bib")