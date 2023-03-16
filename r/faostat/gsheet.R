# install.packages('gsheet')

library(gsheet)

a <- gsheet2tbl('1V35q6K1ghcsBiYc2WuRQoTvOP0U2V14FZBWIP-LohSw')


########

install.packages("FAOSTAT")

library(FAOSTAT)
vignette("FAOSTAT", package = "FAOSTAT")

data_folder <- "data_raw"
dir.create(data_folder)

fao_metadata <- FAOsearch()

class(fao_metadata)

FAOsearch(dataset = "crop", full = FALSE)

crop_production <- get_faostat_bulk(code = "QCL",
                                    data_folder = data_folder)

str(crop_production)

saveRDS(crop_production, "data_raw/crop_production_e_all_data.rds")

crop_production <- readRDS("data_raw/crop_production_e_all_data.rds")


FAOchecked.df = FAOcheck(var = FAOquery.df$varName, year = "Year",
                         data = FAO.lst$entity, type = "multiChina",
                         take = "simpleCheck")
##########

WB.lst <- getWDItoSYB(indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"),
                     name = c("totalPopulation", "GDPUSD"),
                     getMetaData = TRUE, printMetaData = TRUE)

Demo <- WB.lst$entity[, c("Country", "Year", "totalPopulation")]
demoResult <- fillCountryCode(country = "Country", data = Demo,
                             outCode = "ISO2_WB_CODE")


View(WB.lst)

########

View(crop_production)

#########

dim(crop_production)

sapply(crop_production, class)

head(crop_production)

table(crop_production$area)


vietnam_data <- subset(crop_production, area == "Viet Nam")

vietnam_data

View(vietnam_data)


vietnam_data_a <- subset(vietnam_data, flag == "A")

dim(vietnam_data_a)

View(vietnam_data_a)

table(vietnam_data_a$item)


## CÂU HỎI NGHIÊN CỨU:

# VIỆT NAM LÀ NƯỚC NÔNG NGHIỆP. VẬY CHÍNH XÁC VIỆT NAM SẢN XUẤT NHỮNG MÓN NÀO LÀ CHỦ LỰC TỪ TRƯỚC ĐẾN GIỜ?



summary(vietnam_data_a)

any(is.na(vietnam_data_a))

head(vietnam_data_a)

vietnam_crop <- vietnam_data_a[, c("item", "element", "year", "unit", "value")]

head(vietnam_crop)

summary(vietnam_crop)

str(vietnam_crop)

unique(vietnam_crop$item)

View(vietnam_crop)


vietnam_crop_2018 <- subset(vietnam_crop, year == 2018)

View(vietnam_crop_2018)

table(vietnam_crop_2018$item)

######

install.packages("plotly")


install.packages("hrbrthemes")
hrbrthemes::import_roboto_condensed()
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp

# save the widget
library(htmlwidgets)
saveWidget(pp, file=paste0( getwd(), "/ggplotlyBubblechart.html"))
?saveWidget

############
vietnam_data_a


vietnam_crop


View(vietnam_crop)

table(vietnam_crop$item)

unique(vietnam_crop$item)

# TÌM UNIQUE QUA TỪNG NĂM

unique(vietnam_crop$year)

library(tidyverse)
library(dplyr)


vietnam_crop_ok
View(vietnam_crop_ok)


dim(vietnam_crop)
dim(vietnam_crop_ok)

unique(vietnam_crop_ok$element)

vietnam_stocks <- 1:10


View(vietnam_crop_2018)


table(vietnam_crop_ok$item)

yes <- subset(vietnam_crop_ok, year == 1961)

seq_along(unique(vietnam_crop_ok$year))


# for(i in unique(vietnam_crop_ok$year)){
#   
#   yes <- subset(vietnam_crop_ok, year == i)
#   
#   print(unique(yes$item))
#   
# }


check_item <- function(x){
  yes <- subset(vietnam_crop_ok, year == x)
  thanks <- unique(yes$item)
  return(thanks)
} 

check_item_year <- lapply(unique(vietnam_crop_ok$year), check_item)
 
names(check_item_year) <- unique(vietnam_crop_ok$year)

check_item_year

# check_item_year[["1978"]]
# 
# check_item(1978)
# check_item(1961)
# 
# identical(check_item_year[["1978"]], check_item(1978))


########### ĐỒ THỊ THỂ HIỆN SỰ ĐA DẠNG VỀ MẶT HÀNG


plot(check_item_year)

length(check_item_year)

sapply(check_item_year, length) -> crop_type

plot(names(crop_type), crop_type,
     xaxs = "i", yaxs = "i",
     xlim = c(1955, 2025),
     ylim = c(0, 100),
     type = "p",
     pch = 19,
     col = 2)

########################### TÁCH RA DỮ LIỆU CÂY TRỒNG

vietnam_crop_1961 <- subset(vietnam_crop_ok, year == 1961)

table(vietnam_crop_1961$item)

vietnam_crop_1961_plant <- subset(vietnam_crop_1961, element != "stocks")
vietnam_crop_1961_stocks <- subset(vietnam_crop_1961, element == "stocks")

dim(vietnam_crop_1961_plant)
dim(vietnam_crop_1961_stocks)
dim(vietnam_crop_1961)

plot(vietnam_crop_1961_plant$value)

table(vietnam_crop_1961_plant$element)

w <- unclass(table(vietnam_crop_1961_plant$item))
w_1 <- w[w == 2]
w_1

which(vietnam_crop_1961$item )
vietnam_crop_1961_plant



plant_1961 <- vietnam_crop_1961_plant[vietnam_crop_1961_plant$item %in% names(w_1), ]


########

vietnam_crop_1998 <- subset(vietnam_crop_ok, year == 1998)

table(vietnam_crop_1998$item)

vietnam_crop_1998_plant <- subset(vietnam_crop_1998, element == "area_harvested")

vietnam_crop_1998$item %in% vietnam_crop_1998_plant$item

plant_1998  <- vietnam_crop_1998[vietnam_crop_1998$item %in% vietnam_crop_1998_plant$item, ]

plant_1998 <- subset(plant_1998, element != "yield")


dim(vietnam_crop_1998_plant)

dim(vietnam_crop_1998_plant)
dim(vietnam_crop_1998)

table(vietnam_crop_1998_plant$element)

w <- unclass(table(vietnam_crop_1998_plant$item))
w_1 <- w[w == 2]
w_1


plant_1998 <- vietnam_crop_1998_plant[vietnam_crop_1998_plant$item %in% names(w_1), ]

table(plant_1998$element)

View(plant_1998)

subset(plant_1998, element == "production")
subset(plant_1998, element == "area_harvested")


plot(subset(plant_1998, element == "production")$value ~ subset(plant_1998, element == "area_harvested")$value)

############################

dim(crop_production)
vietnam_data <- subset(crop_production, area == "Viet Nam")
dim(vietnam_data)
vietnam_data_a <- subset(vietnam_data, flag == "A")
dim(vietnam_data_a)
vietnam_crop <- vietnam_data_a[, c("item", "element", "year", "unit", "value")]
dim(vietnam_crop)
vietnam <- vietnam_crop %>% dplyr::arrange(desc(year), desc(item), desc(element))
dim(vietnam)

View(vietnam)
names(vietnam)
dien_tich_thu_hoach <- subset(vietnam, element == "area_harvested")
dim(dien_tich_thu_hoach)
head(dien_tich_thu_hoach)

##########


library(tidyverse)

head(dien_tich_thu_hoach)


#########
# https://stackoverflow.com/questions/15840926/categorical-bubble-plot-for-mapping-studies
grid 
grid <- subset(grid, count > 0)
radius <- sqrt( grid$count / pi )
symbols(grid$Var1, grid$Var2, radius, inches=0.30, xlab="Research type", ylab="Research area")
text(grid$Var1, grid$Var2, grid$count, cex=0.5)

library(gsheet)
a <- gsheet::gsheet2tbl("1V35q6K1ghcsBiYc2WuRQoTvOP0U2V14FZBWIP-LohSw")
a <- as.data.frame(a)
a -> grid

dien_tich_thu_hoach -> ok

names(grid)
names(ok)
names(ok)[1] <- "Var1"
names(ok)[3] <- "Var2"
names(ok)[5] <- "count"

ok -> grid

head(grid)

grid$Var1 <- reorder(grid$Var1, grid$count, decreasing = TRUE)

library(ggplot2)
p <- ggplot(grid, aes(x = Var1, y = Var2)) +
  geom_point(aes(size = count), shape = 21, 
             fill = "yellow", alpha = 0.5) +
  # geom_text(aes(label = count), size = 0.7) +
  # scale_size_identity() +
  scale_size(range = c(1, 4), name = "Năng suất (tấn)") + 
  theme_classic() +
  ylab("") +
  xlab("") 
p + coord_flip()


unique(grid$Var1)

############

dien_tich_thu_hoach

dim(dien_tich_thu_hoach)
dim(vietnam)


names(vietnam)

vietnam$item %in% dien_tich_thu_hoach$item

thu_hoach_va_san_luong <- vietnam[vietnam$item %in% dien_tich_thu_hoach$item, ]

table(thu_hoach_va_san_luong$element)

thu_hoach_va_san_luong <- subset(thu_hoach_va_san_luong, item != "Vegetables Primary")

dim(thu_hoach_va_san_luong)
# check_1 <- subset(thu_hoach_va_san_luong, element == "production")
# 
# check_2 <- subset(thu_hoach_va_san_luong, element == "area_harvested")
# 
# which(check_1$item %in% check_2$item)
# 
# table(check_1$element)
# 
# table(check_2$element)
# 
# setdiff(check_1$item, check_2$item)
# setdiff(check_2$item, check_1$item)
# 
# 
# check_3 <- subset(check_2, item == "Vegetables Primary")
# 
# ok <- subset(thu_hoach_va_san_luong, item == "Vegetables Primary")

##################

head(vietnam)
str(vietnam)
dim(vietnam)
unique(vietnam$item)

rice <- subset(vietnam, item == "Rice")

rice_ok <- subset(rice, element != "yield")

table(rice_ok$unit)

rice_ok <- subset(rice_ok, select = -unit)

rice_final <- rice_ok %>% tidyr::spread(key = "element", value = "value")


# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

options(scipen = 99)

p <- ggplot(rice_final, aes(x = year, y = production/1000, 
                       size = area_harvested,
                      fill = production/area_harvested)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  scale_size(range = c(1, 15), name = "Diện tích canh tác (ha)") +
  scale_fill_viridis_c(name = "Năng suất (tấn/ha)",
                       option = "D", 
                       trans = "sqrt",
                       direction = 1,
                       na.value = "grey90",
                       guide = "colourbar") +
  guides(size = guide_legend(order = 1),
         fill = guide_colourbar(barwidth = 1, barheight = 10, order = 2)) +
  theme_classic() +
  ylab("Sản lượng thu hoạch (nghìn tấn)") +
  xlab("Năm") +
  # xlim(1955, 2025) +
  # ylim(0, 1.1*max(rice_final$production)/1000) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.15*max(rice_final$production)/1000)) +
  theme(legend.position = "right") +
  theme(axis.title.y = element_text(face="bold", 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.text.x = element_text(face = "bold", color="#993333", 
                                   size = 11),
        axis.text.y = element_text(face = "bold", color="#993333", 
                                   size = 11)) +
  ggtitle("Tình hình sản xuất lúa gạo ở Việt Nam từ 1961–2021 | Nguồn: FAOSTAT | Đồ họa: tuhocr.com") 

p +  geom_hline(yintercept = 48000, colour = "green", lwd = 1.5, lty = 2) +
  annotate(geom = "text", x = 2025, y = 1.035*max(rice_final$production)/1000, 
           label = "Năm 2021 đạt 43,8 nghìn tấn",
           color = "blue") +
  annotate(geom = "text", x = 2013, y = 1.1*max(rice_final$production)/1000, 
           label = "Ngưỡng cực hạn 48 nghìn tấn/năm",
           color = "red") +
  # theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
  coord_cartesian(clip = "off")

?annotate
  
rice_final


library(ggplot2)
library(grid)
library(png)

image <- readPNG("rice.png")
logor <- readPNG("logor.png")
# # rasterImage(image_tuhocr,5,5,7,7)
grid.raster(image, x = 0.65, y = 0.25, width = 0.3)
grid.raster(logor, x = 0.17, y = 0.77, width = 0.1)


#############
library(dplyr)
library(tidyr)
library(DT)
thailand_data <- subset(crop_production, area == "Thailand")
dim(thailand_data)

## subset dữ liệu Flag A từ nguồn chính thức do Thái Lan công bố
thailand_data_a <- subset(thailand_data, flag == "A")
dim(thailand_data_a)

## chọn những cột cần thiết
thailand_crop <- thailand_data_a[, c("item", "element", "year", "unit", "value")]

## sắp xếp theo năm và thông tin nông sản
thailand <- thailand_crop %>% dplyr::arrange(desc(year), desc(item), desc(element))
str(thailand)

## chúng ta có clean dataset để đưa vào phân tích giai đoạn sau
datatable(thailand, options = list(pageLength = 30))

## từ đoạn này trở đi thì dùng code cũ, vì object sẽ tự động pass vào.
rice <- subset(thailand, item == "Rice")
rice_ok <- subset(rice, element != "yield")
rice_ok <- subset(rice_ok, select = -unit)

head(rice_ok)

## spread dataset để ggplot nhận diện dữ liệu
rice_final <- rice_ok %>% tidyr::spread(key = "element", value = "value")

head(rice_final)

summary(rice_final)



rice_final[which(is.na(rice_final$area_harvested)), ]

na.omit(rice_final)

################ VẼ BUBBLE BÀI BẢN GGANIMATE

library(dplyr)
library(tidyr)
library(DT)

crop_production <- readRDS("data_raw/crop_production_e_all_data.rds")
dim(crop_production)
str(crop_production)


# subset dữ liệu Flag A từ nguồn chính thức do các quốc gia công bố
crop_production_a <- subset(crop_production, flag == "A")
dim(crop_production_a)

head(crop_production_a, n = 30)

dplyr::sample_n(crop_production_a, size = 30)


# tìm item RICE
table(crop_production_a$item)
unclass(table(crop_production_a$item))

grep("Rice", names(unclass(table(crop_production_a$item))))

grep("(R(ICE|ice)|rice)", names(unclass(table(crop_production_a$item))), value = TRUE)

grep("(C|c)offee", names(unclass(table(crop_production_a$item))), value = TRUE)

# https://www.javainuse.com/rexgenerator
# https://regexr.com/

# https://stackoverflow.com/questions/13353663/what-is-the-regular-expression-to-allow-uppercase-lowercase-alphabetical-charac


aaa <- subset(crop_production_a, item == "Rice")
dim(aaa)
bbb <- subset(crop_production_a, item == "Rice" | item == "Coffee, green")
dim(bbb)


str(bbb)
table(bbb$item)

####### CHỈ CHỌN DUY NHẤT RICE

rice_production <- subset(crop_production_a, item == "Rice", 
                          select = c("area", "item", "element", "unit", "value", "year"))


identical(unique(crop_production_a$year_code), unique(crop_production_a$year))

dim(rice_production)
str(rice_production)

unique(rice_production$unit)

head(rice_production)

dplyr::sample_n(rice_production, size = 30)

unique(rice_production$element)

table(rice_production$element)

vietnam_rice <- subset(rice_production, area == "Viet Nam")


vietnam_rice <- vietnam_rice %>% dplyr::arrange(desc(year), desc(item), desc(element))

vietnam_rice


rice_production <- rice_production %>% dplyr::arrange(desc(year), desc(item), desc(element), desc(value))

rice_production

grep("Australia", unique(rice_production$area), value = TRUE)

grep("New Zealand", unique(rice_production$area), value = TRUE)


sort(unique(rice_production$area))

aus <- subset(rice_production, area == "Australia" & year == "2021")

nz <- subset(rice_production, area == "Australia and New Zealand" & year == "2021") 


check_1 <- subset(rice_production, area == "Australia")
check_1
check_2 <- subset(rice_production, area == "Australia and New Zealand")
check_2

identical(check_1, check_2)
identical(check_1$value, check_2$value)

check_3 <- subset(rice_production, area == "Australia" & year <= 2019 & year >= 1991)  

check_4 <- subset(rice_production, area == "Australia and New Zealand" & year <= 2019 & year >= 1991)  

identical(check_3$value, check_4$value) 

rice_production

unique(rice_production$area)

"Australia and New Zealand"
"Northern America" 
"European Union (27)"
"South America"

country_group <- read.csv("data_raw/FAOSTAT_data_3-14-2023.csv")

View(country_group)

country_group$Country.Group

unique(country_group$Country.Group) %in% unique(rice_production$area)

unique(rice_production$area) %in% unique(country_group$Country.Group)

unique(rice_production$area)[!(unique(country_group$Country.Group) %in% unique(rice_production$area))]


sort(intersect(unique(country_group$Country.Group), unique(rice_production$area)))

sort(intersect(unique(rice_production$area), unique(country_group$Country.Group)))

identical(sort(intersect(unique(country_group$Country.Group), unique(rice_production$area))),
          sort(intersect(unique(rice_production$area), unique(country_group$Country.Group))))


unique(rice_production$area)

intersect(unique(rice_production$area), intersect(unique(country_group$Country.Group), unique(rice_production$area)))


intersect(intersect(unique(country_group$Country.Group), unique(rice_production$area)), unique(rice_production$area))

setdiff(unique(rice_production$area), intersect(unique(country_group$Country.Group), unique(rice_production$area)))

setdiff(intersect(unique(country_group$Country.Group), unique(rice_production$area)), unique(rice_production$area))

country_clean <- setdiff(unique(rice_production$area), intersect(unique(country_group$Country.Group), unique(rice_production$area)))



rice_production

###### SUBSET THEO DANH SÁCH QUỐC GIA


View(rice_production)

country_clean %in% rice_production$area

rice_production$area %in% country_clean

grep("South-eastern Asia", rice_production$area)

rice



rice_production[rice_production$area %in% country_clean, ] -> rice_country

dim(rice_country)

summary(rice_country)

rice_country_a <- subset(rice_country, element != "yield")
dim(rice_country_a)

table(rice_country$element)
dim(rice_country)

14697 - 2864

options("max.print" = 500000)
rice_country_a
nrow(rice_country_a)

length(attributes(rice_country_a)$row.names)

head(rice_country_a, n = 30)

### ĐỔI TRỤC

rice_country_a %>% dplyr::arrange(area) -> rice_country_b

summary(rice_country_a)

head(check_5)

rice_country_a %>% tidyr::spread(key = "element", value = "value") -> rice_country_final

dim(rice_country_a)
head(rice_country_final)

dim(rice_country_final)

summary(rice_country_a)
summary(rice_country_final)

table(rice_country_final$element)

### TÌM THÔNG TIN NHỮNG DÒNG CHỈ CÓ SẢN LƯỢNG MÀ KO CÓ DIỆN TÍCH

head(rice_country_b, n = 30)
# rice_country_a
# dim(rice_country_a)
# row.names(rice_country_a) <- 1:nrow(rice_country_a)  
# 
# seq_along(rice_country_a)
# nrow(rice_country_a)
# nrow(mtcars)

rice_country_b
dim(rice_country_b)
sum(unclass(table(rice_country_b$element)))

yes <- subset(rice_country_a, element == "production" & element == "area_harvested")



head(rice_country_b, n = 15)
head(rice_country_final, n = 15)


subset(rice_country_final, area == "Viet Nam")



?tidyr::spread


rice_country_a %>% pivot_wider(names_from = "element", values_from = "value")


test <- rice_country_a[1:500, ]

test %>% pivot_wider(names_from = "element", values_from = "value") -> test_1

print(test_1, n = 500)


df <- tibble(x = c("a", "b"), y = c(3, 4), z = c(5, 6))


df %>%
  spread(x, y) %>%
  gather("x", "y", a:b, na.rm = TRUE)


df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))

# month Amy.A Bob.A Amy.B Bob.B
# 1     1     9     8     6     5
# 2     2     7     6     7     6
# 3     3     6     9     8     7



pivot_wider(data = test, 
            # id_cols = element,
            names_from = element, 
            values_from = "value")


rice_harvest <- subset(rice_country_b, element == "area_harvested")
dim(rice_harvest)

rice_production <- subset(rice_country_b, element == "production")
dim(rice_production)


table(rice_country_b$element)

merge(rice_harvest, rice_production, all.y = all)

rice_harvest[1:10, ] -> x
x
rice_production[1:10, ] -> y
y

merge(x, y, by = c("area", "year"))




?merge


clean_rice <- merge(rice_harvest, rice_production, by = c("area", "year"))
dim(rice_harvest)
dim(rice_production)
dim(rice_country_b)
dim(clean_rice)

summary(clean_rice)

head(clean_rice, n = 30)

dim(subset(clean_rice, area == "Viet Nam"))

dim(subset(rice_country_b, area == "Viet Nam"))

any(is.na(clean_rice))

summary(clean_rice)

######


unique(rice_harvest$year)


