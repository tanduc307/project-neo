# graphics.off()
# windows(width = 5, height = 5)
# set.seed(42)
# par(mar = c(5,5,1,10))
plot(rnorm(50,15,5), rnorm(50,15,3),
     xlim = c(0,30), ylim = c(5,25),
     pch = 19, col = c("red","blue"))

legend(x = par("usr")[1], 
       y = par("usr")[4],
       pch = 19, col = c("red", "blue"),
       legend = c("LEGEND 1", "Second Legend"))



leg <- legend("bottom", pch = 19, col = c("red", "blue"),
              legend = c("LEGEND 1", "Second Legend"),
              plot = FALSE)

legend(x = (leg$rect$left + leg$rect$w) * 1.05, y = leg$rect$top,
       pch = 19, col = c("red", "blue"),
       legend = c("LEGEND 1", "Second Legend"))
