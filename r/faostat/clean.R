library(tidyverse)
crop_production <- readRDS("data_raw/crop_production_e_all_data.rds")
# subset dữ liệu Flag A từ nguồn chính thức do các quốc gia công bố
crop_production_a <- subset(crop_production, flag == "A")
rice_production <- subset(crop_production_a, item == "Rice",
select = c("area", "item", "element", "unit", "value", "year"))
rice_production



# bỏ thông tin group quốc gia

country_group <- read.csv("data_raw/FAOSTAT_data_3-14-2023.csv")

country_clean <- setdiff(unique(rice_production$area), intersect(unique(country_group$Country.Group), unique(rice_production$area)))

rice_production[rice_production$area %in% country_clean, ] -> rice_country

rice_tidy <- rice_country %>% dplyr::arrange(area, desc(year), desc(item), desc(element), desc(value))

str(rice_country)
str(rice_tidy)

options("max.print" = 500000)
head(rice_tidy, n = 500)

rice_tidy[500:1000, ]

test_zim <- subset(rice_tidy, area == "Zimbabwe", select = -unit)

test_vi <- subset(rice_tidy, area == "Viet Nam", select = -unit)

##############################
# https://www.datasciencemadesimple.com/reshape-in-r-from-wide-to-long-from-long-to-wide/

ok_vi <- reshape(data = test_vi,
                 idvar = "year",
                 v.names = "value",
                 timevar = "element",
                 direction = "wide")

ok_zim <- reshape(data = test_zim,
                  idvar = "year",
                  v.names = "value",
                  timevar = "element",
                  direction = "wide") 


rice_ready <- subset(rice_tidy, select = -unit) 

rice_full <- reshape(data = rice_ready,
                     idvar = c("year", "area"),
                     v.names = "value",
                     timevar = "element",
                     direction = "wide") 


rice_vi <- subset(rice_full, area == "Viet Nam")

head(rice_vi)

dim(ok_vi)
dim(rice_vi)

identical(ok_vi, rice_vi)
ok_vi == rice_vi

#######

rice_full
names(rice_full)
head(rice_full)


rice_final <- subset(rice_full, select = -value.yield)

head(rice_final)

names(rice_final)[4] <- "production"
names(rice_final)[5] <- "area_harvested"

summary(rice_final)


which(rice_final$production == 212843000)
rice_final[1272, ]

which(rice_final$area_harvested == 46379000)
rice_final[2754, ]


rice_vietnam <- subset(rice_final, area == "Viet Nam")

plot(production ~ area_harvested, rice_vietnam)

head(rice_final)

tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE)

tapply(rice_final$area_harvested, rice_final$area, mean, na.rm = TRUE)

class(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE))
range(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE))

sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE), decreasing = TRUE)

length(sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE)))


names(sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE), decreasing = TRUE))

topten <- names(sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE), decreasing = TRUE))[1:11]

topten <- topten[-2]

rice_final

table(rice_final$area)


### subset từ list vector

rice_final$area %in% topten

rice_final[rice_final$area %in% topten, ] -> topten_ok

View(rice_final)

topten_ok

##########

# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)


# Make a ggplot, but add frame=year: one image per year
ggplot(topten_ok, aes(area_harvested/1000000, production/1000000, size = production/area_harvested, color = area)) +
  geom_point() +
  # scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Năm: {frame_time}', x = 'Diện tích canh tác (triệu ha)', y = 'Sản lượng thu hoạch (triệu tấn)') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")

# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")


################

topten_ok -> rice_final

na.omit(topten_ok) -> rice_final

grep("China, mainland", rice_final)


grep("China, mainland", rice_final$area, value = TRUE)

gsub("China, mainland", replacement = "China", rice_final$area) -> rice_final$area

vi <- subset(rice_final, area == "Viet Nam")
summary(vi)

p <- ggplot(rice_final, aes(
  x = area, y = production / 1000000,
  size = area_harvested / 1000000,
  fill = area,
  label = round(production / 1000000, digits = 1)
)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  geom_text(
    # label = round(production / 1000000, digits = 1), 
    nudge_x = 0, nudge_y = 12,
    check_overlap = TRUE,
    size = 5
  ) +
  scale_size(range = c(1, 9), name = "Diện tích canh tác (triệu ha)", 
             breaks = round(seq(min(rice_final$area_harvested) / 1000000, max(rice_final$area_harvested) / 1000000, length.out = 6),
                            digits = 0)) + 
  guides(fill = FALSE) +
  theme_classic() +
  theme(legend.position = "top", 
        legend.title = element_text(size = 10, face = "bold"),
        legend.background = element_blank(),
        legend.justification = c("left", "top"),
        # legend.box.just = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.5, "lines"),
        legend.spacing.y = unit(0.4, "lines")) 

ok <- p + geom_hline(yintercept = 50, colour = "#148c34", lwd = 1, lty = 2) +
  labs(title = 'Year: {frame_time}', x = NULL, y = "Sản lượng thu hoạch (triệu/tấn)") +
  transition_time(year) +
  ease_aes('linear', interval = 0.001)

animate(ok, 
        # duration = 274, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
        # fps = 60
        nframes = 30
        )

magick::image_write(
  animate(ok, width = 1000, height = 1000, nframes = 30, duration = 30), 
  "test1.gif"
)

# https://stackoverflow.com/questions/52899017/slow-down-gganimate-in-r

a <- df_daily %>%
  ggplot(aes(longitude, latitude)) +
  geom_point(aes(alpha = a)) +
  transition_time(flu_day) +
  ease_aes('linear', interval = 0.001) +
  labs(title = 'Date: {frame_time}')



animate(a, 
        duration = 274, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
        fps  =  [pick how smooth you want],
        nframes = [...or pick it here])










?gganimate






  labs(title = 'Year: {frame_time}', x = 'aaa', y = 'bbb') +
  transition_time(year) +
  ease_aes('linear')






  anim <- ggplot(airquality, aes(Day, Temp)) +
    geom_point(aes(colour = factor(Month))) +
    transition_time(Day)
  
  # Removing a time point will prolong the tweening between neighbouring time
  # points so the time dimension stays linear
  airquality_missing <- airquality[airquality$Day <= 10 | airquality$Day >= 20, ]
  anim1 <- ggplot(airquality_missing, aes(Day, Temp)) +
    geom_point(aes(colour = factor(Month))) +
    transition_time(Day)
  
  # Range can be constrained if needed
  anim2 <- ggplot(airquality, aes(Day, Temp)) +
    geom_point(aes(colour = factor(Month))) +
    transition_time(Day, range = c(10L, 20L))
  
  # The group aesthetic is used to connect elements
  # No grouping
  anim3 <- ggplot(airquality, aes(Day, Temp)) +
    geom_line() +
    transition_time(Month)
  
  # Group by month
  anim4 <- ggplot(airquality, aes(Day, Temp)) +
    geom_line(aes(group = Month)) +
    transition_time(Month) +
    enter_fade() +
    exit_fade()













+
  scale_size(range = c(1, 9), name = "Diện tích canh tác (triệu ha)", 
             breaks = round(seq(min(rice_final$area_harvested)/1000, max(rice_final$area_harvested)/1000, length.out = 6),
                            digits = -2)) + 
  scale_fill_viridis_c(
    name = "Năng suất (tấn/ha)",
    option = "D",
    guide = "colourbar",
    breaks = round(seq(min(rice_final$production / rice_final$area_harvested), 
                       max(rice_final$production / rice_final$area_harvested), length.out = 6), digits = 0)
  ) +
  guides(
    size = guide_legend(order = 1),
    fill = guide_colourbar(barwidth = 1, barheight = 8, order = 2)
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(1955, 2025)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.15 * max(rice_final$production) / 1000000)) +
  
  ## customize theme
  
  theme_classic() +
  ylab("Sản lượng thu hoạch (triệu tấn)") +
  xlab("Năm") +
  
  ## x and y lab
  theme(axis.title.x = element_text(face = "bold", size = 10)) +
  
  theme(axis.title.y = element_text(
    face = "bold",
    margin = margin(t = 1, r = 10, b = 1, l = 10)
  )) +
  
  ## tick text
  
  theme(
    axis.text.x = element_text(
      face = "bold", color = "#993333",
      size = 10
    ),
    axis.text.y = element_text(
      face = "bold", color = "#993333",
      size = 10
    )
  ) +
  
  ## title
  
  labs(title = "Tình hình sản xuất lúa gạo ở Việt Nam từ 1960–2021",
       subtitle = "Nguồn: FAOSTAT [crop dataset, Vietnam, flag A]",
       caption = "Đồ thị: Duc Nguyen | tuhocr.com") +
  
  theme(
    plot.title = element_text(size = 12, 
                              face = "bold", 
                              hjust = 0, margin = margin(t = 10, r = 0, b = 5, l = 0)),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(t = 0, r = 0, b = 5, l = 0)),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 0, r = 0, b = 5, l = 0))
  ) +
  
  ## legend
  
  theme(legend.position = "right", 
        legend.title = element_text(size = 10, face = "bold"),
        legend.background = element_blank(),
        legend.justification = c("right", "bottom"),
        # legend.box.just = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 5),
        legend.spacing.x = unit(0.5, "lines"),
        legend.spacing.y = unit(0.4, "lines")) +
  
  ## margin
  
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 3, r = 14, b = 2, l = 2),
    plot.background = element_rect(fill = "grey90", colour = "black", linewidth = 1)
  )
p


