library(tidyverse)
crop_production <- readRDS("crop_production_e_all_data.rds")
# subset dữ liệu Flag A từ nguồn chính thức do các quốc gia công bố
crop_production_a <- subset(crop_production, flag == "A")
rice_production <- subset(crop_production_a, item == "Rice",
                          select = c("area", "item", "element", "unit", "value", "year"))


# bỏ thông tin group quốc gia

country_group <- read.csv("FAOSTAT_data_3-14-2023.csv")

country_clean <- setdiff(unique(rice_production$area), intersect(unique(country_group$Country.Group), unique(rice_production$area)))

rice_production[rice_production$area %in% country_clean, ] -> rice_country

rice_tidy <- rice_country %>% dplyr::arrange(area, desc(year), desc(item), desc(element), desc(value))

options("max.print" = 500000)

### SHAPING

rice_ready <- subset(rice_tidy, select = -unit)

rice_full <- reshape(data = rice_ready,
                     idvar = c("year", "area"),
                     v.names = "value",
                     timevar = "element",
                     direction = "wide") 

rice_final <- subset(rice_full, select = -value.yield)

names(rice_final)[4] <- "production"
names(rice_final)[5] <- "area_harvested"

tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE)
sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE), decreasing = TRUE)

###### topten

topten <- names(sort(tapply(rice_final$production, rice_final$area, mean, na.rm = TRUE), decreasing = TRUE))[1:21]
topten <- topten[-2]

rice_final[rice_final$area %in% topten, ] -> topten_ok

topten_ok -> top20_rice

save(top20_rice, file = "top20_rice.Rdata")

#### VẼ ĐỒ THỊ

na.omit(topten_ok) -> rice_topten

gsub("China, mainland", replacement = "China", rice_topten$area) -> rice_topten$area

gsub("Viet Nam", replacement = "Việt Nam", rice_topten$area) -> rice_topten$area

gsub("Thailand", replacement = "Thái Lan", rice_topten$area) -> rice_topten$area

gsub("China", replacement = "Trung Quốc", rice_topten$area) -> rice_topten$area


gsub("Japan", replacement = "Nhật Bản", rice_topten$area) -> rice_topten$area

gsub("India", replacement = "Ấn Độ", rice_topten$area) -> rice_topten$area

rice_topten$area <- reorder(rice_topten$area, rice_topten$production, decreasing = TRUE)

rice_topten

save(rice_topten, file = "rice-top.Rdata")

# install.packages("gganimate")
library(gganimate)
library(magick)
library(gifski)
library(viridis)
library(grid)
library(png)
library(ggimage)
# install.packages("ggtext")
# library(ggtext)
# install.packages("gifski")

# logor <- image_read_svg('rsvg.svg', width = 100)

p <- ggplot(rice_topten, aes(
  x = area, y = production / 1000000,
  size = area_harvested / 1000000,
  colour = area,
  label = round(production / 1000000, digits = 1)
)) +
  geom_point(alpha = 0.5, aes(color = area),
             shape = 21,
             color = "black"
               ) +
  geom_text(
    # label = round(production / 1000000, digits = 1), 
    nudge_x = 0, nudge_y = 15,
    check_overlap = TRUE,
    size = 5
  ) +
  scale_size(range = c(1, 20), name = "Diện tích canh tác (triệu ha)", 
             breaks = round(seq(min(rice_topten$area_harvested) / 1000000, max(rice_topten$area_harvested) / 1000000, length.out = 6),
                            digits = 0),
             guide = guide_legend(direction = "horizontal", title.position = "top",
                                  label.position="top", label.hjust = 0.5, label.vjust = 0.5,
                                  label.theme = element_text(angle = 0))) + 
  guides(fill = "none") +
  
  # scale_color_viridis(discrete = TRUE, name = "Region", option = "viridis") +
  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  theme_classic() +
  ylab("Sản lượng thu hoạch (triệu tấn)") +
  xlab(NULL) +
  
  # theme(axis.title.x = element_text(face = "bold", size = 15)) +
  
  theme(axis.title.y = element_text(
    face = "bold", size = 15,
    margin = margin(t = 1, r = 10, b = 1, l = 10)
  )) +
  
  theme(
    axis.text.x = element_text(
      face = "bold", color = "#993333",
      size = 15, margin = margin(t = 5, r = 0, b = 5, l = 0),
      angle = 45, vjust = 1, hjust = 1
    ),
    axis.text.y = element_text(
      face = "bold", color = "#993333",
      size = 15
    )
  ) +
  labs(title = "Top 10 quốc gia sản xuất lúa gạo trên thế giới giai đoạn 1960–2021 | Năm: {frame_time}",
    caption = c("Nguồn: FAOSTAT [crop dataset, flag A]", "Đồ thị: Duc Nguyen | tuhocr.com")) +
  
  theme(
    plot.title = element_text(size = 17, 
                              face = "bold", 
                              hjust = 0.5, vjust = -8, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    # plot.subtitle = element_text(size = 14, hjust = 1, margin = margin(t = 0, r = 0, b = 5, l = 0)),
    plot.caption = element_text(size = 14, face = "bold", hjust = c(0, 1), margin = margin(t = 0, r = 0, b = 5, l = 0))
  ) +
  
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 15),
    plot.background = element_rect(fill = "grey90", colour = "black", linewidth = 1)
  ) +
  
  theme(legend.position = c(0.60, 0.5), legend.direction = "horizontal",
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_blank(),
        legend.justification = c("left", "top"),
        # legend.box.just = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.5, "lines"),
        legend.spacing.y = unit(0.4, "lines"),
        legend.text = element_text(size = 16, face = "bold")) +
  geom_hline(yintercept = 50, colour = "#148c34", lwd = 1, lty = 2)  
  # annotate(
  #   geom = "text", x = 8, y = 60,
  #   label = 'atop(bold("Ngưỡng 50 triệu tấn/năm"))',
  #   color = "blue", size = 5, parse = TRUE
  # # ) +
  # geom_label(aes(x = 5, y = 240,
  #                label = "Top 10 quốc gia sản xuất lúa gạo trên thế giới giai đoạn 1960–2021"),
  #            # fill = "#f5ec42",
  #            # label.size = 0.15,
  #            size = 8)

  # geom_richtext(aes(x = 0.5, y = 0.5, 
  #                   label = "**This should be bold**",
  #                   col = "red"),  
  #               fill = NA,
  #               label.color = NA)
# annotation_raster(logor, ymin = 180, ymax = 220, xmin = 8, xmax = 
#                     10) 


# image <- readPNG("riceok.png")
# logor <- readPNG("logor.png")




ok <- p +
  # ggimage::geom_image(
  #   aes(
  #     x = rank,
  #     y = value,
  #     image = file.path("logor.png"))
  #     # size = ...
  #   ) +
  transition_reveal(year) +
  shadow_trail(distance = 0.01,
               alpha = 0.5,
               shape = 2) +
  ease_aes('cubic-in-out')
  
# grid.raster(image, x = 0.4, y = 0.7, width = 0.2)
# grid.raster(logor, x = 0.8, y = 0.8, width = 0.1)

# {frame_time}
ok
# | Năm: {frame_time}
animate(ok, fps = 4, width = 800, height = 600)

anim_save("output/21.gif")


grid.raster(image, x = 0.4, y = 0.7, width = 0.2) +
  grid.raster(logor, x = 0.8, y = 0.8, width = 0.1) +
# install.packages("av")
library(av)
anim_save("output/3.mp4",
          renderer = av_renderer())
# https://stackoverflow.com/questions/47025729/how-to-add-multiple-captions-in-ggplot2-outside-of-the-main-graph-area

# title = "Top 10 quốc gia sản xuất lúa gạo trên thế giới giai đoạn 1960–2021 | Năm: xxxx",





#################### CHÈN HÌNH SVG VÀO

library(magick) 
library(gapminder)
library(ggplot2)
library(rsvg)
library(gganimate)
library(lubridate)

tiger <- image_read_svg('rsvg.svg', width = 
                          400)

xmin <- ymd("1965/01/01")
xmax <- ymd("2005/01/01")
ymin <- 30
height <- round(as.numeric(xmax-xmin)/356, 0)
ymax <- ymin + height

gapminder %>%
  mutate(year = ymd(year, truncated = 2L)) %>%
  ggplot() +
  aes(year, lifeExp, size = pop, colour = country) +
  annotation_raster(tiger, ymin = ymin, ymax = ymax, xmin = xmin, xmax = 
                      xmax) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  labs(title = 'Year: {frame_time}', x = 'Year', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear') +
  anim_save("animated_tiger_timeseries.gif")








library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# https://paldhous.github.io/ucb/2018/dataviz/week14.html

# animate(ok, 
#         # duration = 274, # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds
#         # fps = 60
#         nframes = 30
# )
# 
# # remotes::install_github("thomasp85/gganimate@v0.1.1")
# gganimate(ok, "aaa.gif", title_frame = "Top 10 quốc gia sản xuất lúa gạo trên thế giới", 
#           ani.width = 800, ani.height = 800,
#           res = 300)
# 
# # 
# install.packages('animation')


ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_richtext(aes(x = 4, y = 25, 
                    label = "**This should be bold**<br>and this not",
                    col = "red"),  
                fill = NA,
                label.color = NA) +
  geom_vline(xintercept = 3.2, colour = "red") +
  theme(legend.position = "none")
