x <- 0:10
y <- 0:10

oldpar <- par(no.readonly = TRUE)

png(filename = "test5.png",
    width = 1000, height = 1000, units = "px", pointsize = 12,
    bg = "white", res = NA)

# par(pin=c(1.9,1.9)) 

# par(din=c(10,10))

plot(y ~ x,
     xlim = c(0, 10),
     ylim = c(0, 10),
     xaxs = "i",
     yaxs = "i")

par(oldpar)

dev.off()

dev.size("in")
dev.size("px")
par("din")[1]
par("din")[2]
par("din")[3]
par("din")[4]

par("pin")[1]
par("pin")[2]
par("pin")[3]
par("pin")[4]

par("usr")

par("plt")

par("mai")

par("mar")


?nmar

dev.size(units = "in")
dev.size(units = "cm")
dev.size(units = "px")





