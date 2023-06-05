bibliometrix::biblioshiny()
library(bibliometrix)

file <- "demo.bib"

M <- convert2df(file = file, dbsource = "scopus", format = "bibtex")

write.csv(M, "DEMO.csv")
