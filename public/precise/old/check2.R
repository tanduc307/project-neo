plot(rnorm(20), rnorm(20))
text(grconvertX(0.1, "npc", "user"), grconvertY(0.9, "npc", "user"), "a)")
text(grconvertX(0.2, "npc", "user"), grconvertY(0.8, "npc", "user"), "npc")
text(grconvertX(0.3, "ndc", "user"), grconvertY(0.7, "ndc", "user"), "ndc") # the text will be near the outer margin


##########################################

# dtext <- function(x = 0.5, y = 0.5, label, ...) {
#     margins <- par("omi") + par("mai")
#     plotsize <- par("pin")
#     devsize  <- dev.size()
#     usr_space <- par("usr")
#     usr_y <- devsize[2] / plotsize[2] * (diff(usr_space[3:4]))
#     y_min <- usr_space[3] - usr_y * margins[1]/devsize[2]
#     
#     usr_x <- devsize[1] / plotsize[1] * (diff(usr_space[1:2]))
#     x_min <- usr_space[1] - usr_x * margins[2]/devsize[1]
#     
#     text(x = x * usr_x + x_min,
#          y = y * usr_y + y_min,
#          label = label,
#          xpd = NA,
#          ...)
# }
# 
# plot(1:10, 1:10)
# 
# dtext(x = 0.1, y = 0.1, label = "(0.1, 0.1)", cex = 2, col = "red2")
# dtext(x = 0.5, y = 0.5, label = "(0.5, 0.5)", cex = 2, col = "blue2")
# dtext(x = 0.9, y = 0.9, label = "(0.9, 0.9)", cex = 2, col = "green2")
