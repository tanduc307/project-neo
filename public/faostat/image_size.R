extract_item <- function(input_item, input_rds, input_region){
    
    # CLEAN DATA
    
    ## 2.1) TÁCH ITEM QUAN TÂM
    
    crop_full <- readRDS(input_rds)
    
    crop_full_clean <- crop_full[ , c(3, 6, 8, 10, 11, 12)]
    
    rice_all <- subset(crop_full_clean, item == input_item)
    
    ## 2.2) TÁCH QUỐC GIA KHỎI THÔNG TIN REGION
    
    read.csv(input_region) -> country_group
    
    region_name <- unique(country_group$Country.Group)
    
    country_name <- unique(rice_all$area)
    
    country_1 <- country_name[!(country_name %in% region_name)]
    
    rice_country <- rice_all[rice_all$area %in% country_1, ]
    
    ## 2.3) SẮP XẾP DATA THEO TRẬT TỰ
    
    library(dplyr)
    
    rice_country <- rice_country %>% dplyr::arrange(area, 
                                                    desc(year),
                                                    desc(element),
                                                    desc(value)
    )
    
    #### BƯỚC 3: CHECK DATA AND CLEAN ####
    
    ## SỬ DỤNG LỆNH TAPPLY ĐỂ TÌM THÔNG TIN CÁC QUỐC GIA KHÔNG SẢN XUẤT GẠO
    
    check_1 <- tapply(rice_country$value, 
                      list(rice_country$area, rice_country$element), 
                      FUN = sum, na.rm = TRUE)
    
    check_2 <- as.data.frame(check_1)
    
    check_2$area <- row.names(check_2)
    
    check_2[, c(4, 1:3)] -> check_2
    
    row.names(check_2) <- NULL
    
    check_2 |> dplyr::arrange(area_harvested, production, yield, area) -> check_3
    
    country_check <- check_3 |> subset(area_harvested == 0)
    
    ## SUBSET NHỮNG QUỐC GIA CÓ SẢN XUẤT GẠO
    
    country_name_1 <- unique(rice_country$area)
    
    country_2 <- country_name_1[!(country_name_1 %in% country_check$area)]
    
    rice_country_2 <- rice_country[rice_country$area %in% country_2, ]
    
    #### BƯỚC 4: RESHAPE DATA ####
    
    rice_country_2 <- rice_country_2 |> subset(select = -unit)
    
    rice_ready <- reshape(data = rice_country_2,
                          idvar = c("year", "area"),
                          v.names = "value",
                          timevar = "element",
                          direction = "wide") 
    
    ## BỎ CỘT YIELD
    
    rice_ready <- rice_ready[, c(1, 2, 3, 5, 6)]
    
    ## BỎ MISSING VALUE, CHỈNH LẠI TÊN CỘT VÀ ROW.NAMES
    
    rice_ready <- na.omit(rice_ready)
    
    attributes(rice_ready)$na.action <- NULL
    
    names(rice_ready)[4] <- "production"
    names(rice_ready)[5] <- "area_harvested"
    row.names(rice_ready) <- NULL 
    
    ## ĐỔI TÊN DATASET
    
    rice_ready -> item_ready
    
    ## TÍNH NĂNG SUẤT, NẾU CÓ NAN HOẶC INF THÌ SET BẰNG 0
    
    item_ready$yield <- round(item_ready$production / item_ready$area_harvested, digits = 2)
    
    item_ready$yield[is.nan(item_ready$yield)] <- 0
    
    item_ready$yield[is.infinite(item_ready$yield)] <- 0
    
    return(item_ready)
    
}

##############

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw_20230422/crop_production_all_data.rds",
                             input_region = "data_raw_20230422/FAOSTAT_data_4-22-2023.csv")



##################





rice_country -> rice_ready

rice_ready -> rice_check

matrix_rice <- rice_check[, c(1, 3, 4)]

length(unique(rice_check$area)) -> so_luong_quoc_gia

a <- as.character(so_luong_quoc_gia)

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

library(plot.matrix)
library(RColorBrewer)
options(scipen = 6, digits = 2)
v <- matrix_2
## convert qua matrix
v <- as.matrix(v)

z <- v/1000000

# svg(filename = "rice_128e.svg"
#     )

library(Cairo)

Cairo::CairoPNG(
    width = 1728, #length
    height = 3456, #width
    file = paste("nameofplot30-mai", ".png", sep = ""),
    # type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement
    dpi = 175,
    units = "px" #you can change to pixels etc 
)

# tiff("rice_128h.tif", res = 300,
#      width = 9, height = 18,
#      units = 'in')

par(mai = c(1, 1, 1, 1))

par(mgp = c(0, 0.7, 0))

par("cex.axis" = 0.5)
par(xpd = TRUE) # đưa legend outside plot

plot.matrix:::plot.matrix(z,
                          las = 1, 
                          key = list(side = 4, font = 2),
                          col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                          na.col = "grey",
                          main = "",
                          axis.row = list(side = 2, font = 2, tick = FALSE),
                          axis.col = list(side = 1, font = 2),
                          xlab = "", ylab = "",
                          breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                          # key= list(side = 4, font = 2, cex.axis = 0.75), 
                          fmt.key = "%.0f",
                          # polygon.key = NULL, 
                          # axis.key = NULL, 
                          # spacing.key=c(3, 2, 2)
                          # border = NA
)

title(main = "Các quốc gia sản xuất lúa gạo trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
      adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)

# mysubtitle <- substitute(paste(italic("Dữ liệu của"), italic(x)), italic("quốc gia và vùng lãnh thổ được sắp xếp theo alphabet")))

# mysubtitle <- substitute('Example map with'~italic(x), list(x=so_luong_quoc_gia))

mysubtitle <- bquote(italic('Thứ tự của')~italic(.(a))~italic("quốc gia và vùng lãnh thổ được sắp xếp theo sản lượng sản xuất."))

mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")

legend("bottom",
       legend = c("Không có dữ liệu", "Không sản xuất"),
       fill = c("grey", "#00ffff"),
       # lty = c(1, 2), cex = 1,
       # pch = c(NA, 21),
       # lwd = 2,
       x.intersp = 1,
       y.intersp = 2,
       inset = -0.045,
       box.lty = 0,
       horiz = TRUE, 
       cex = 0.8,
       bg = "transparent")

par(new = TRUE)

par(yaxt = "none")

par(mgp = c(0, 0.7, 0))

plot.matrix:::plot.matrix(z,
                          las = 1,
                          key = list(side = 4, font = 2),
                          col = c("#00ffff", hsv(0.1, seq(0.15, 1, length.out = 9), 1), "#ff685d", "#ff4e41", "#ff3122"),
                          na.col = "grey",
                          main = "",
                          # axis.row = list(side = 2, font = 2),
                          axis.col = list(side = 3, font = 2),
                          xlab = "", ylab = "",
                          breaks = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
                          # border = NA
                          # key= list(side = 4, font = 2, cex.axis = 0.75), 
                          fmt.key = "%.0f",
                          # polygon.key = NULL, 
                          # axis.key = NULL, 
                          # spacing.key=c(3, 2, 2)
)

par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text

text(x = 66, y = 122, 
     substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7,
     col = "red")

# mycaption <- expression(bold("*Không có dữ liệu: bao gồm không có thông tin và có dữ liệu nhưng bằng 0."))
# mtext(side = 1, line = 1.8, adj = 0, cex = 0.8, mycaption, col = "darkgreen")

library(png) ## dùng đề chèn file ảnh
library(grid) ## canh chỉnh vị trí ảnh
logor <- readPNG("logo-blue.png")
grid.raster(logor, x = 0.10, y = 0.97, width = 0.1)

dev.off()






?Cairo


