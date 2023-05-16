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
                              axis.col = list(side = 1, font = 2),
                              xlab = "", ylab = "",
                              breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                              fmt.key = "%.0f"
    )
    
    title(main = "Các quốc gia sản xuất lúa gạo trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
          adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)
    
    mysubtitle <- bquote(italic('Thứ tự của')~italic(.(a))~italic("quốc gia và vùng lãnh thổ được sắp xếp theo sản lượng sản xuất."))
    
    mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")
    
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
    
    par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text
    
    t_1 <<- 0.95 # percent in plot
    t_2 <<- 0.95 # percent in plot
    
    text(grconvertX(t_1, "ndc", "user"), grconvertY(t_2, "ndc", "user"),
         substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7, col = "red")
    
    x_text_1 <<- grconvertX(t_1, "ndc", "user")
    y_text_1 <<- grconvertY(t_2, "ndc", "user")
    
    ########
    
    par(new = TRUE)
    
    par(yaxt = "none")
    
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
    
    #######################################
    
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
    
    ############
    
    par(oldpar)
    
    dev.off()
    
}



################### HÌNH RE-PRODUCE

produce_image <- function(input_data,
                          file_name,
                          thang_do = c(0, 0.000000001, 1, 10, 
                                       round(seq(from = 100, 
                                                 to = round(range(z, na.rm = TRUE)[2]*1.2, digits = -1),
                                                 length.out = 9), digits = -2)),
                          don_vi = "...",
                          tua_de,
                          scale_color = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122")
                          ) {
    
    library(plot.matrix)
    library(RColorBrewer)
    
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
                              col = scale_color,
                              na.col = "grey",
                              main = "",
                              axis.row = list(side = 2, font = 2, tick = FALSE),
                              axis.col = list(side = 1, font = 2),
                              xlab = "", ylab = "",
                              breaks = thang_do,
                              fmt.key = "%.0f"
    )
    
    title(main = tua_de,
          adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)
    
    mysubtitle <- bquote(italic('Thứ tự của')~italic(.(b))~italic("quốc gia và vùng lãnh thổ được sắp xếp theo sản lượng sản xuất."))
    
    mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")
    
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
    
    par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text
    
    t_1 <- 0.95 #percent in plot
    t_2 <- ( (400 + region.px[2]*(c/d)) - (cao_chuan - t_2*cao_chuan) ) / (400 + region.px[2]*(c/d))#percent in plot
    
    text(grconvertX(t_1, "ndc", "user"), grconvertY(t_2, "ndc", "user"),
         substitute(paste(bold(don_vi))), cex = 0.7, col = "red")
    
    #######################################
    
    par(new = TRUE)
    
    par(yaxt = "none")
    
    plot.matrix:::plot.matrix(input_data,
                              las = 1,
                              key = list(side = 4, font = 2),
                              col = scale_color,
                              na.col = "grey",
                              main = "",
                              axis.row = list(side = 2, font = 2, tick = FALSE),
                              axis.col = list(side = 3, font = 2),
                              xlab = "", ylab = "",
                              breaks = thang_do,
                              fmt.key = "%.0f"
    )
    
    #######################################
    
    x_2 <- 0.10
    
    h_1 <- 400 + region.px[2]*(c/d)
    h_2 <- h_1 * 20 /4000
    h_3 <- (h_2 - (20 - unclass(y_logo)[1]))/h_2
    
    library(png) 
    library(grid) 
    logor <- readPNG("logo-blue.png")
    grid.raster(logor, x = x_2, y = h_3, width = 0.1)
    
    par(oldpar)
    
    dev.off()
    
}

######################################