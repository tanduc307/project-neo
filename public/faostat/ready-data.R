
#### BƯỚC 1: GET RAW DATA ####

library(FAOSTAT)
data_folder <- "data_raw_20230422"
dir.create(data_folder)

crop_production <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = data_folder)

saveRDS(crop_production, file = "data_raw_20230422/crop_production_all_data.rds")

#### BƯỚC 2: CLEAN DATA ####

## 2.1) TÁCH ITEM QUAN TÂM

crop_full <- readRDS("data_raw/crop_production_all_data.rds")

crop_full_clean <- crop_full[ , c(3, 6, 8, 10, 11, 12)]

rice_all <- subset(crop_full_clean, item == "Rice")

## 2.2) TÁCH QUỐC GIA KHỎI THÔNG TIN REGION

read.csv("data_raw/FAOSTAT_data_3-21-2023.csv") -> country_group

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

ok <- tapply(rice_country$value,
             rice_country$area,
             sum, na.rm = TRUE)

class(ok)
dim(ok)
sort(ok, decreasing = TRUE)
# 
# ok <- data.frame(ok)
# 
# ok |> dplyr::arrange(ok) -> ok

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

rice_country_2

dim(rice_country_2)
dim(rice_country)

#### BƯỚC 4: RESHAPE DATA ####

rice_country_2 <- rice_country_2 |> subset(select = -unit)

rice_ready <- reshape(data = rice_country_2,
                      idvar = c("year", "area"),
                      v.names = "value",
                      timevar = "element",
                      direction = "wide") 

# 1 ton = 10000 hg

(439549 / 147555) * 10000

round((439549 / 147555) * 10000, digits = 0)

29789


## BỎ CỘT YIELD

rice_ready <- rice_ready[, c(1, 2, 3, 5, 6)]

## BỎ MISSING VALUE, CHỈNH LẠI TÊN CỘT VÀ ROW.NAMES

rice_ready[which(!is.na(rice_ready$value.area_harvested)), ]

summary(rice_ready[which(!is.na(rice_ready$value.area_harvested)), ])

rice_ready <- na.omit(rice_ready)

summary(rice_ready)

names(rice_ready)[4] <- "production"
names(rice_ready)[5] <- "area_harvested"
row.names(rice_ready) <- NULL 

options("max.print" = 100000)

str(rice_ready)

#### BƯỚC 5: XEM DATASET HOÀN CHỈNH ####

head(rice_ready, n = 30)
str(rice_ready)
summary(rice_ready)
any(is.na(rice_ready)) # check xem còn giá trị NA? (FALSE LÀ KO CÒN NA)

####################
saveRDS(rice_ready, file = "rice_ready.rds")

aaa <- readRDS(file = "rice_ready.rds")

View(aaa)

###########################
library(kableExtra)

aaa %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    cat(., file = "rice_ready.html")

## save "as (it) is" | "asis"

aaa %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") -> output

readr::write_file(output, "output.html")

save_kable(output, file = "output.png")
save_kable(output, file = "output.html")
save_kable(output, file = "output.pdf")

#### BƯỚC 6: PLOT HEATMAP ####

# https://rpubs.com/tuhocr/huong-dan-ve-heatmap

##### SẮP XẾP MATRIX

rice_ready -> rice_full_1 # gán vào object để tận dụng code template

matrix_rice <- rice_full_1[, c(1, 3, 4)] # chỉ lấy cột area, year, production

library(dplyr)

matrix_rice <- matrix_rice |> dplyr::arrange(area, year)

matrix_1 <- reshape(data = matrix_rice,
                    idvar = c("area"),
                    v.names = "production",
                    timevar = "year",
                    direction = "wide") 

colnames(matrix_1)[2:62] <- unique(matrix_rice$year)

matrix_2 <- matrix_1[, 2:62]

rownames(matrix_2) <- matrix_1[, 1]

##### PLOTTING

library(plot.matrix)
library(RColorBrewer)

v <- matrix_2

## convert qua matrix
v <- as.matrix(v)

z <- v/1000000 # tính theo triệu tấn

par(mar = c(3, 9, 6, 4)) # margin

par(mgp = c(0, 0.7, 0)) # axis title, axis labels and axis line

par("cex.axis" = 0.5)  
par(xpd = TRUE) # đưa legend outside plot

plot.matrix:::plot.matrix(z,
                          las = 1, 
                          key = list(side = 4, font = 2),
                          col = brewer.pal(n = 9, name = "YlOrRd"),
                          na.col = "white",
                          main = "",
                          axis.row = list(side = 2, font = 2, tick = FALSE),
                          axis.col = list(side = 1, font = 2),
                          xlab = "", ylab = "",
                          # breaks = 12
)

title(main = "Các quốc gia sản xuất lúa gạo trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
      adj = 0, col.main = "blue", cex.main = 0.9, line = 3.5)

mysubtitle <- expression(italic("Sắp xếp theo alphabet"))
mtext(side = 3, line = 1.8, adj = 0, cex = 0.8, mysubtitle, col = "red")

legend("topright",
       legend = c("Không có dữ liệu"),
       fill = c("white"),
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
                          col = brewer.pal(n = 9, name = "YlOrRd"),
                          na.col = "white",
                          main = "",
                          # axis.row = list(side = 2, font = 2),
                          axis.col = list(side = 3, font = 2),
                          xlab = "", ylab = "",
)

par(lheight = 1.15) # chỉnh khoảng cách giữa hai dòng text

text(x = 66, y = 120, 
     substitute(paste(bold("Đơn vị: \ntriệu tấn"))), cex = 0.7,
     col = "red")

library(png) ## dùng đề chèn file ảnh
library(grid) ## canh chỉnh vị trí ảnh
logor <- readPNG("logo-blue.png")
grid.raster(logor, x = 0.12, y = 0.97, width = 0.1)

















