##########################

# https://www.tutorialspoint.com/how-to-find-the-size-of-the-plotting-window-in-r
# https://www.geeksforgeeks.org/create-plot-window-of-particular-size-in-r/

x <- rnorm(10)
y <- rnorm(10, 500, 35)
plot(x, y)
dev.size("in")
dev.size("px")

plot(1:10)
dev.new(width=5, height=4)
plot(1:20)

#########################
dev.off()
### MINIMALIST

dim(z)
plot.matrix:::plot.matrix(z)
dev.new(width = 100, height = 100, unit = "px")
plot(1:10)
dev.size("cm")

par(oldpar)

par(bg = "lightblue") # plot area
plot(1:10)





# plot region
plot(1:10)
# Plot region color
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightyellow") # Color

# Add a new plot
par(new = TRUE)

# Create your plot
plot(1:10) 

?par("usr")