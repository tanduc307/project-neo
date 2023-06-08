# bibliometrix::biblioshiny()
library(bibliometrix)

file <- "demo.bib"

M <- bibliometrix::convert2df(file = file, dbsource = "scopus", format = "bibtex")

M[1:220, ] -> N

M[1:220, ] -> E

write.csv(N, "OKP.csv")
# 
# N <- read.csv("CHECK.csv")
# 
# names(N)
# 
jabref <- read.csv("jabref.csv")
# 
# names(jabref)
# 
# # jabref

N[, ] <- NA

jabref$Identifier
row.names(N) <- jabref$Identifier

View(N)

############
N$AU <- jabref$Author

N$AB <- jabref$Custom1

N$SO <- jabref$Journal

N$PP <- jabref$Pages

N$TI <- jabref$Title

N$DT <- "ARTICLE"

N$VL <- jabref$Volume

N$PY <- jabref$Year

################################

N$keywords.plus <- NA

View(N)
