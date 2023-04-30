rice_country <- readRDS(file = "rice_country.rds")

###########

rice_country -> rice_ready

rice_ready -> rice_check

matrix_rice <- rice_check[, c(1, 3, 4)]

matrix_rice$area <- reorder(matrix_rice$area, matrix_rice$production)

matrix_rice <- matrix_rice |> dplyr::arrange(desc(area), year)

matrix_1 <- reshape(data = matrix_rice,
                    idvar = c("area"),
                    v.names = "production",
                    timevar = "year",
                    direction = "wide") 

colnames(matrix_1)[2:62] <- unique(matrix_rice$year)

matrix_2 <- matrix_1[, 2:62]

rownames(matrix_2) <- matrix_1[, 1]

#########

v <- matrix_2
## convert qua matrix
v <- as.matrix(v)

z <- v/1000000

#################

produce_image(input_data = z[1:120, ],
              file_name = "p60")

produce_image_fixed(input_data = z,
                    file_name = "128-fixed-65")


#################################

produce_image_fixed <- function(input_data, file_name = "128-fixed") {
    
    library(plot.matrix)
    library(RColorBrewer)
    
    ### HÌNH CHUẨN ĐỂ LẤY KÍCH THƯỚC  
    nrow(input_data) -> so_luong_quoc_gia
    
    a <<- as.character(so_luong_quoc_gia)
    
    cao_chuan <<- 4000
    
    library(Cairo)
    
    Cairo::CairoPNG(
        width = 2000,
        height = cao_chuan, 
        file = paste(file_name, ".png", sep = ""),
        bg = "white",
        dpi = 200,
        units = "px" 
    )
    
    oldpar <- par(no.readonly = TRUE)
    
    par(mar = c(4, 9, 6, 4))
    
    par(mgp = c(0, 0.7, 0))
    
    par("cex.axis" = 0.5)
    par(xpd = TRUE) # đưa legend outside plot
    
    plot.matrix:::plot.matrix(input_data,
                              las = 1, 
                              key = list(side = 4, font = 2),
                              col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                              na.col = "grey",
                              main = "",
                              axis.row = list(side = 2, font = 2, tick = FALSE),
                              axis.col = list(side = 3, font = 2),
                              xlab = "", ylab = "",
                              breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                              fmt.key = "%.0f"
    )
    
    title(main = "Các quốc gia sản xuất lúa gạo trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
          adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)
    
    mysubtitle <- bquote(italic('Thứ tự của')~italic(.(a))~italic("quốc gia và vùng lãnh thổ được sắp xếp theo sản lượng sản xuất."))
    
    mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")
    
    # legend(x = "bottom",
    #        y = NULL,
    #        legend = c("Không có dữ liệu", "Không sản xuất"),
    #        fill = c("grey", "#00ffff"),
    #        x.intersp = 1,
    #        y.intersp = 2,
    #        inset = c(0, -0.045), 
    #        box.lty = 0,
    #        horiz = TRUE, 
    #        cex = 0.8,
    #        bg = "transparent")
    
    leg <<- legend(x = "bottom",
                   y = NULL,
                   legend = c("Không có dữ liệu", "Không sản xuất"),
                   fill = c("grey", "#00ffff"),
                   x.intersp = 1,
                   y.intersp = 2,
                   inset = c(0, -0.045),
                   box.lty = 0,
                   horiz = TRUE,
                   cex = 0.8,
                   bg = "transparent",
                   plot = FALSE)
    
    legend(x = leg$rect$left,
           y = leg$rect$top,
           legend = c("Không có dữ liệu", "Không sản xuất"),
           fill = c("grey", "#00ffff"),
           x.intersp = 1,
           y.intersp = 2,
           # inset = c(0, -0.045), 
           box.lty = 0,
           horiz = TRUE, 
           cex = 0.8,
           bg = "transparent")
    
    par(new = TRUE)

    par(mar = c(4, 9, 6, 4))

    par(yaxt = "none")

    par(mgp = c(0, 0.7, 0))
    # 
    # plot.matrix:::plot.matrix(z,
    #                           las = 1,
    #                           key = list(side = 4, font = 2),
    #                           col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
    #                           na.col = "grey",
    #                           main = "",
    #                           axis.col = list(side = 3, font = 2),
    #                           xlab = "", ylab = "",
    #                           breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
    #                           fmt.key = "%.0f"
    # )
    
    plot.matrix:::plot.matrix(input_data,
                              las = 1, 
                              key = list(side = 4, font = 2),
                              col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                              na.col = "grey",
                              main = "",
                              axis.row = list(side = 2, font = 2, tick = FALSE),
                              axis.col = list(side = 1, font = 2),
                              xlab = "", ylab = "",
                              breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                              fmt.key = "%.0f"
    )
    
    
    par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text
    
    # t_1 <- 6.4
    # t_2 <- 6.5
    # 
    # text(grconvertX(t_1, "ndc", "in"), grconvertY(t_2, "ndc", "in"), 
    #      substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7, col = "red")
    # 
    # x_text_1 <<- grconvertX(t_1, "ndc", "in")
    # y_text_1 <<- grconvertY(t_2, "ndc", "in")
    
    t_1 <<- 0.95 # percent in plot
    t_2 <<- 0.95 # percent in plot

    text(grconvertX(t_1, "ndc", "user"), grconvertY(t_2, "ndc", "user"),
         substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7, col = "red")

    x_text_1 <<- grconvertX(t_1, "ndc", "user")
    y_text_1 <<- grconvertY(t_2, "ndc", "user")

    #######################################
    
    user.range <- par("usr")[c(2, 4)] - par("usr")[c(1, 3)]
    
    region.pct <- par("plt")[c(2, 4)] - par("plt")[c(1, 3)]
    
    region.px <- dev.size(units = "px") * region.pct
    
    px.per.xy <- region.px / user.range
    
    x_1 <- 0.1
    y_1 <- 0.97
    
    library(png) 
    library(grid) 
    logor <- readPNG("logo-blue.png")
    grid.raster(logor, x = x_1, y = y_1, width = 0.1)
    
    x_logo <<- convertWidth(unit(x_1, "npc"), "in")
    y_logo <<- convertHeight(unit(y_1, "npc"), "in")
    
    user.range <<- par("usr")[c(2, 4)] - par("usr")[c(1, 3)]
    
    region.pct <<- par("plt")[c(2, 4)] - par("plt")[c(1, 3)]
    
    region.px <<- dev.size(units = "px") * region.pct
    
    px.per.xy <<- region.px / user.range

    par(oldpar)
    
    dev.off()
    
}



###################

produce_image <- function(input_data,
                          file_name) {
    
    library(plot.matrix)
    library(RColorBrewer)

##########################################################################
### HÌNH RE-PRODUCE    
nrow(input_data) -> so_luong_quoc_gia

b <- as.character(so_luong_quoc_gia)

c <- as.numeric(b)
d <- as.numeric(a)

library(Cairo)

Cairo::CairoPNG(
    width = 520 + region.px[1],
    height = 400 + region.px[2]*(c/d), 
    file = paste(file_name, ".png", sep = ""),
    bg = "white",
    dpi = 200,
    units = "px" 
)

oldpar <- par(no.readonly = TRUE)

par(mar = c(4, 9, 6, 4))

par(mgp = c(0, 0.7, 0))

par("cex.axis" = 0.5)
par(xpd = TRUE) # đưa legend outside plot

plot.matrix:::plot.matrix(input_data,
                          las = 1, 
                          key = list(side = 4, font = 2),
                          col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                          na.col = "grey",
                          main = "",
                          axis.row = list(side = 2, font = 2, tick = FALSE),
                          axis.col = list(side = 3, font = 2),
                          xlab = "", ylab = "",
                          breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                          fmt.key = "%.0f"
)

title(main = "Các quốc gia sản xuất lúa gạo trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
      adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)

mysubtitle <- bquote(italic('Thứ tự của')~italic(.(b))~italic("quốc gia và vùng lãnh thổ được sắp xếp theo sản lượng sản xuất."))

mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")

# legend("bottom",
#        legend = c("Không có dữ liệu", "Không sản xuất"),
#        fill = c("grey", "#00ffff"),
#        x.intersp = 1,
#        y.intersp = 2,
#        inset = -0.045,
#        box.lty = 0,
#        horiz = TRUE, 
#        cex = 0.8,
#        bg = "transparent")

legend(x = leg$rect$left,
       y = leg$rect$top,
       legend = c("Không có dữ liệu", "Không sản xuất"),
       fill = c("grey", "#00ffff"),
       x.intersp = 1,
       y.intersp = 2,
       # inset = c(0, -0.045), 
       box.lty = 0,
       horiz = TRUE, 
       cex = 0.8,
       bg = "transparent")

par(new = TRUE)

par(mar = c(4, 9, 6, 4))

par(yaxt = "none")

par(mgp = c(0, 0.7, 0))

plot.matrix:::plot.matrix(input_data,
                          las = 1, 
                          key = list(side = 4, font = 2),
                          col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                          na.col = "grey",
                          main = "",
                          axis.row = list(side = 2, font = 2, tick = FALSE),
                          axis.col = list(side = 1, font = 2),
                          xlab = "", ylab = "",
                          breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                          fmt.key = "%.0f"
)

par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text

t_1 <- 0.95 #percent in plot
t_2 <- ( (400 + region.px[2]*(c/d)) - (cao_chuan - t_2*cao_chuan) ) / (400 + region.px[2]*(c/d))#percent in plot

text(grconvertX(t_1, "ndc", "user"), grconvertY(t_2, "ndc", "user"),
     substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7, col = "red")

#######################################

x_2 <- 0.10

h_1 <- 400 + region.px[2]*(c/d)
h_2 <- h_1 * 20 /4000
h_3 <- (h_2 - (20 - unclass(y_logo)[1]))/h_2

library(png) 
library(grid) 
logor <- readPNG("logo-blue.png")
grid.raster(logor, x = 0.10, y = h_3, width = 0.1)

par(oldpar)

dev.off()

}

######################################





































#################

# convertWidth(unit(0.10, "npc"), "in")
# convertHeight(unit(0.93356401, "npc"), "in")

# usr_1 <- par("usr")[1]
# usr_2 <- par("usr")[2]
# usr_3 <- par("usr")[3]
# usr_4 <- par("usr")[4]
# 
# pin_1 <- par("pin")[1]
# pin_2 <- par("pin")[2]
# pin_3 <- par("pin")[3]
# pin_4 <- par("pin")[4]
# 
# din_1 <- par("din")[1]
# din_2 <- par("din")[2]
# din_3 <- par("din")[3]
# din_4 <- par("din")[4]
# 
# plt_1 <- par("plt")[1]
# plt_2 <- par("plt")[2]
# plt_3 <- par("plt")[3]
# plt_4 <- par("plt")[4]
# 
# mai_1 <- par("mai")[1]
# mai_2 <- par("mai")[2]
# mai_3 <- par("mai")[3]
# mai_4 <- par("mai")[4]
# 
# dev.size("in")































