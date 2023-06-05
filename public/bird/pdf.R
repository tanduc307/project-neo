# install.packages("bib2df")
# https://cran.r-project.org/web/packages/bib2df/vignettes/bib2df.html

library(bib2df)
library(stringr)
library(pdftools)

df <- bib2df::bib2df(file = "yes.bib", separate_names = TRUE)


View(df)

get_title_pdf <- function(input) {
  
  strsplit(df$TITLE, split = ":") -> p1
  strsplit(unlist(p1), split = " ") -> p2
  unlist(p2) -> p3
  paste(p3, collapse = " ") -> p4
  stringr::str_squish(p4) -> p5
  stringr::word(p5, start = 1:5) -> p6
  paste(p6, collapse = "_") -> p7
  tolower(paste(df$AUTHOR[[1]][1, ]$first_name, df$YEAR, p7, sep = "_")) -> p8
  paste0(p8, ".pdf") -> p9
  
  return(p9)
  
}

a <- "D:\\GITHUB\\jabref\\pttt_2007_giao_trinh_thong_ke.pdf"
pdftools::pdf_length(a) / 2 -> b
b
pdftools::pdf_length(a) -> c

paste(substr(a, start = 1 , stop = nchar(a) - 4), "1", "to", b, sep = "_") -> d1  
paste0(d1, ".pdf") -> d1

paste(substr(a, start = 1 , stop = nchar(a) - 4), b + 1, "to", c, sep = "_") -> d2  
paste0(d2, ".pdf") -> d2

pdftools::pdf_subset(input = a, output = d1, pages = 1 : b)
pdftools::pdf_subset(input = a, output = d2, pages = (b +1) : c)

# pdftools::pdf_combine(c("a1.pdf", "a2.pdf"), output = "a3.pdf")
