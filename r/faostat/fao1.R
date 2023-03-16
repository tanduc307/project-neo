library(FAOSTAT)
data_folder <- "data_raw"
dir.create(data_folder) ## tạo folder lưu file trên máy tính
fao_metadata <- FAOsearch()

FAOsearch(dataset = "crop", full = FALSE)

info <- FAOsearch(dataset = "crop", full = TRUE)

info

class(info)

View(info)

info[1, ]

## ĐƯA THÔNG TIN VÀO BẢNG

install.packages("kableExtra")

# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
library(kableExtra)

dt <- mtcars[1:5, 1:6]
dt
kbl(dt)

kbl(info)

info %>%
  kbl() %>%
  kable_styling()

info %>%
  kbl() %>%
  kable_paper("hover", full_width = F)


info %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

info %>%
  kbl() %>%
  kable_classic_2(full_width = F)

info %>%
  kbl() %>%
  kable_minimal()

info %>%
  kbl() %>%
  kable_material(c("striped", "hover"))

info %>%
  kbl() %>%
  kable_material_dark()

kbl(info) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered", "responsive"))

# striped, bordered, hover, condensed and responsive

kbl(info) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

kbl(info) %>%
  kable_paper(bootstrap_options = "striped", full_width = F)

kbl(info) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

kbl(info) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_left")

kbl(info) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

kbl(info) %>%
  kable_styling(fixed_thead = TRUE)

kbl(info) %>%
  kable_paper(full_width = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em", background = "yellow")

kbl(info) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(5:7, bold = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

kbl(info) %>%
  kable_paper("striped", full_width = F) %>%
  row_spec(0, angle = -45)

# https://haozhu233.github.io/kableExtra/use_kableExtra_with_formattable.html

load("rice_topten.Rdata")
dim(rice_topten)
str(rice_topten)

# install.packages("formattable")

library(formattable)
ft_dt <- mtcars[1:5, 1:4]
ft_dt$car <- row.names(ft_dt)
row.names(ft_dt) <- NULL
# ft_dt$mpg <- color_tile("white", "orange")(ft_dt$mpg)
# ft_dt$cyl <- cell_spec(ft_dt$cyl, angle = (1:5)*60, 
#                        background = "red", color = "white", align = "center")
# ft_dt$disp <- ifelse(
#   ft_dt$disp > 200,
#   cell_spec(ft_dt$disp, color = "red", bold = T),
#   cell_spec(ft_dt$disp, color = "green", italic = T)
# )
# ft_dt$hp <- color_bar("lightgreen")(ft_dt$hp)
# ft_dt <- ft_dt[c("car", "mpg", "cyl", "disp", "hp")]
# 
# kbl(ft_dt, escape = F) %>%
#   kable_paper("hover", full_width = F) %>%
#   column_spec(5, width = "3cm") %>%
#   add_header_above(c(" ", "Hello" = 2, "World" = 2))

load("top20_rice.Rdata")

top20_rice -> rice_topten

rice_compare <- subset(rice_topten, year == 2020 | year == 2000)

rice_compare 

# rice_2021_production <- rice_2021[, c(1, 3, 4)]
# 
# rice_2021_production
# 
# ft_dt$mpg <- color_tile("white", "orange")(ft_dt$mpg) # trường hợp này (ft_dt$mpg) là tham số của function formatter trong function color_tile

# color_tile()


#####
# Cách 1:
g <- function() { "hello" }
g()

f <- function() { g } # nếu f là function chứa kết quả trả về của function g
f # thì lệnh gọi tên function f sẽ trả về ruột của f
f() # thì lệnh gọi function f sẽ trả về nội hàm của function g

f()() # muốn gọi function g thông qua function f thì dùng cách này

#####
# Cách 2:
f <- function() { g } ; g <- function() { "hello" } ; f()()

#####
# Cách 3:
f <- function(p) { function(g) g^p }
f(5)(6)


# ft_dt$cyl <- cell_spec(ft_dt$cyl, angle = (1:5)*60, 
#                        background = "red", color = "white", align = "center")

ft_dt$disp <- ifelse(
  ft_dt$disp > 200,
  cell_spec(ft_dt$disp, color = "red", bold = T),
  cell_spec(ft_dt$disp, color = "green", italic = T)
)

ft_dt$hp <- color_bar("lightgreen")(ft_dt$hp)

ft_dt <- ft_dt[c("car", "mpg", "cyl", "disp", "hp")] # xếp thứ tự cột

kbl(ft_dt, escape = F) %>%
  kable_paper("hover", full_width = F) %>%
  column_spec(5, width = "3cm") 

# %>%
#   add_header_above(c(" ", "Hello" = 2, "World" = 2))

############################

# rice_2021
# 
# rice_2021$area_harvested <- color_tile("white", "orange")(rice_2021$area_harvested)
# 
# rice_2021$production <- color_bar("lightgreen")(rice_2021$production)
# 
# kbl(rice_2021, escape = F) %>%
#   kable_paper("hover", full_width = F) %>%
#   column_spec(5, width = "3cm") 


##########

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

rice_wide$area_harvested.2020 <- color_bar("yellow")(rice_wide$area_harvested.2020)
rice_wide$area_harvested.2000 <- color_bar("yellow")(rice_wide$area_harvested.2000)

rice_wide$yield.2020 <- color_bar("pink")(rice_wide$yield.2020)
rice_wide$yield.2000 <- color_bar("pink")(rice_wide$yield.2000)

rice_wide$percent_1 <- color_tile("white", "orange")(rice_wide$percent_1)

rice_wide$percent_2 <- color_tile("white", "orange")(rice_wide$percent_2)

rice_wide$percent_3 <- color_tile("white", "orange")(rice_wide$percent_3)

file_image <- list.files("E:\\GITHUB\\project-neo\\r\\faostat\\png", full.names = TRUE)

rice_wide$flag <- ""

rice_wide[c(1, 11, 2:10)] -> rice_wide



kbl(rice_wide, escape = FALSE) %>%
  kable_paper("hover", full_width = FALSE) %>%
  column_spec(2, image = spec_image(file_image , 40, 30))







ok <- list.files("E:\\GITHUB\\project-neo\\r\\faostat\\png", full.names = TRUE)

ok

basename(ok)

rice_wide$area

rice_wide$area[grep("Democratic People", rice_wide$area)]

gsub(".png", "", basename(ok)) -> file_country

file_country[grep("Democratic People", basename(ok))] <- rice_wide$area[grep("Democratic People", rice_wide$area)]

grep("Democratic People", basename(ok), value = TRUE) 

gsub(".png", "", basename(ok)) %in% rice_wide$area


file_country %in% rice_wide$area

ok
file_country


rice_wide$area
ddd
df <- data.frame(v1 = ok, v2 = file_country, v3 = rice_wide$area)

df

# https://svgtopng.com/

file_country -> x

rice_wide$area -> y

x[order(match(x,y))]








file_image <- list.files("E:\\GITHUB\\project-neo\\r\\faostat\\png", full.names = TRUE)



gsub(".png", "", basename(file_image))[order(match(gsub(".png", "", basename(file_image)), 
                                                   rice_wide$area))] -> file_image_2

file_image_3 <- paste0(file_image_2, ".png")


gsub(".png", "", basename(file_image)) -> file_country

















# https://stackoverflow.com/questions/53549662/how-can-i-add-a-fontawesome-icon-to-a-table-in-rmarkdown




library(fontawesome)
library(knitr)
library(tidyverse)
library(kableExtra)
## note this code throws the following error: Error in 
## as.data.frame.default(x[[i]], optional = TRUE, stringsAsFactors = 
## stringsAsFactors) : cannot coerce class "c("knit_asis", 
## "knit_icon")" to a data.frame

link_location <- "www.google.com"

addLink <- function() {
  paste0("<a href=\"", link_location, "\">", as.character(fa("file-pdf")), "</a>")
}

data_test_1 <- data.frame(file = c('Version 1', 'Version 2', 'Version 3'),
                          last_updated = Sys.Date(),
                          pdf_logo = addLink())

kable(data_test_1, escape = F, align = "c")





















## dòng lệnh này sẽ thực thi truy vấn download full dataset nông nghiệp về máy tính
crop_production <- get_faostat_bulk(code = "QCL", data_folder = data_folder)