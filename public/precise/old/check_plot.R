library(precisePlacement)
# install.packages("precisePlacement")
par(xpd = NA, oma = 1:4)

plot(1:10, pch = 19)

highlightDeviceRegion()
highlightFigureRegion()
highlightPlotRegion()
highlightDataRegion()

showMarginLines()
showOuterMarginLines()

legend('topleft', c('Data Region', 'Plot Region', 'Figure Region', 'Device Region'),
       text.col = c('darkgreen', 'red', 'orange', 'skyblue'), bty = 'n', text.font = 2,
       xjust = 0.5, yjust = 0.5)