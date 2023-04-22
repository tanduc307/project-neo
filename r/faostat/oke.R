library(kableExtra)
library(tidyverse)
library(magick)

kable(mtcars, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(1, color = "red") %>%
  as_image("ssss.pdf")

magick::image_read_pdf("E:\\GITHUB\\project-neo\\r\\faostat\\image.pdf")



# C:\Users\khoa5\AppData\Local\Temp\RtmpeuthZC

magick::image_read_pdf("image.pdf")



kable(mtcars, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(1, color = "red") %>%
  as_image(width = 8)








#########
library(kableExtra)
library(formattable)
library(magick)
library(webshot)
load("top20_rice.Rdata")
# options(width = 200)

top20_rice -> rice_topten

rice_compare <- subset(rice_topten, year == 2020 | year == 2000)

rice_compare 

row.names(rice_compare) <- NULL

rice_compare <- rice_compare[- c(2)]

rice_compare$yield <- round(rice_compare$production / rice_compare$area_harvested,
                            digits = 2)

rice_compare$production <- round(rice_compare$production / 1000,
                                 digits = 0)

rice_compare$area_harvested <- round(rice_compare$area_harvested / 1000,
                                     digits = 0)

rice_wide <- reshape(data = rice_compare,
                     idvar = c("area"),
                     # v.names = c("production"),
                     timevar = "year",
                     direction = "wide") 


rice_wide <- rice_wide[c(1, 5, 2, 6, 3, 7, 4)]

rice_wide$percent_1 <- round(100 * (rice_wide$production.2020 - rice_wide$production.2000) / rice_wide$production.2000, digits = 2)

rice_wide$percent_2 <- round(100 * (rice_wide$area_harvested.2020 - rice_wide$area_harvested.2000) / rice_wide$area_harvested.2000, digits = 2)

rice_wide$percent_3 <- round(100 * (rice_wide$yield.2020 - rice_wide$yield.2000) / rice_wide$yield.2000, digits = 2)

rice_wide <- rice_wide[c(1, 2, 3, 8, 4, 5, 9, 6, 7, 10)]

rank_1 <- order(rice_wide$production.2020, decreasing = TRUE)

rice_wide <- rice_wide[rank_1, ]

row.names(rice_wide) <- NULL

rice_wide -> rice_wide_ok

rice_wide_ok -> rice_wide

###

rice_wide$production.2020 <- color_bar("lightgreen")(rice_wide$production.2020)
rice_wide$production.2000 <- color_bar("lightgreen")(rice_wide$production.2000)

rice_wide$area_harvested.2020 <- color_bar("orange")(rice_wide$area_harvested.2020)
rice_wide$area_harvested.2000 <- color_bar("orange")(rice_wide$area_harvested.2000)

rice_wide$yield.2020 <- color_bar("pink")(rice_wide$yield.2020)
rice_wide$yield.2000 <- color_bar("pink")(rice_wide$yield.2000)

rice_wide$percent_1 <- color_tile("white", "lightgreen")(rice_wide$percent_1)

rice_wide$percent_2 <- color_tile("white", "orange")(rice_wide$percent_2)

rice_wide$percent_3 <- color_tile("white", "pink")(rice_wide$percent_3)

file_image <- list.files("E:\\GITHUB\\project-neo\\r\\faostat\\png", full.names = TRUE)

gsub(".png", "", basename(file_image))[order(match(gsub(".png", "", basename(file_image)), 
                                                   rice_wide$area))] -> file_image_2

file_image_3 <- paste0("png/", file_image_2, ".png")

rice_wide$rank <- 1:20

rice_wide$flag <- ""

rice_wide[c(11, 1, 2:10)] -> rice_wide

# rice_wide[c(12, 1:11)] -> rice_wide

names(rice_wide) <- c("Xếp hạng", "Quốc gia", "Năm 2000", "Năm 2020", "Tỷ lệ \ntăng trưởng (%)",
                      "Năm 2000", "Năm 2020", "Tỷ lệ \ntăng trưởng  (%)",
                      "Năm 2000", "Năm 2020", "Tỷ lệ \ntăng trưởng  (%)")

colnames(rice_wide) <- stringr::str_replace_all(colnames(rice_wide), "\\n", "<br>")

kbl(rice_wide, escape = FALSE, caption = "<center><strong><span style='color: blue'> Top 20 quốc gia sản xuất lúa gạo trên thế giới | Dữ liệu FAOSTAT | Minh họa: tuhocr.com</span></strong></center>",
    format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive",
                                      position = "float_left")) %>%
  # kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "arial") %>%
  # kable_paper("hover", full_width = TRUE) %>%
  column_spec(1, width = "2cm") %>%
  column_spec(2, width = "8cm", border_right = TRUE) %>%
  column_spec(5, border_right = TRUE) %>%
  column_spec(8, border_right = TRUE) %>%
  column_spec(1, image = spec_image(file_image_3 , 96, 72)) %>%
  row_spec(0:20, bold = TRUE) %>%
  row_spec(5, background = "yellow") %>%
  add_header_above(c("", "", "Sản lượng thu hoạch (nghìn tấn)" = 3, "Diện tích canh tác (nghìn ha)" = 3, "Năng suất (tấn/ha)" = 3),
                   bold = TRUE) %>%
  save_kable("test_kable1.pdf")







%>% as_image(width = 12)



kable(mtcars, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(1, color = "red")


kable(mtcars, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(1, color = "red") %>%
  save_kable("test_kable.pdf")




