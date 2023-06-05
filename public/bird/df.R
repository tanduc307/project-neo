library(bib2df)
# library(stringr)
library(pdftools)
library(tidyverse)
library(kableExtra)
devtools::install_github("haozhu233/kableExtra")

df <- suppressWarnings(bib2df::bib2df(file = "tree.bib", separate_names = TRUE))

df <- df[-nrow(df), ]

check_na <- function(input) {
  
  any(is.na(input)) -> check_1
  
  return(check_1)
  
}

sapply(df, check_na) -> check_2
class(check_2)

df[!check_2] -> df_1

View(df_1)

df_1 %>% kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>% 
  kable_styling(font_size = 20)