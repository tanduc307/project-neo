library(grImport)
# install.packages("grImport")


PostScriptTrace("ru.ps") # creates .xml in the working directory
# https://cloudconvert.com/svg-to-ps
spiral <- readPicture("ru.ps.xml")

Sys.setenv(R_GSCMD = normalizePath("C:/Program Files (x86)/gs/gs10.01.1/bin/gswin32c.exe")) 

?readPicture





# generate random data
x = runif(n = 10, min = 1, max = 10)
y = runif(n = 10, min = 1, max = 10)

 # base graphics
x11()
plot(x, y, pty = 's', type = 'n', xlim = c(0, 10), ylim = c(0, 10))
xx = grconvertX(x = x, from = 'user', to = 'ndc')
yy = grconvertY(y = y, from = 'user', to = 'ndc')
grid.symbols(spiral, x = xx, y = yy, size = 0.05)

legend("bottomleft", 
       legend = c("ru"),
       pch = c("spiral"),
       col = c("red"), 
       )
# https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/r-create-custom-legend-with-base-plot/