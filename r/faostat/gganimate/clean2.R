load("gganimate/rice-all-flag.RData")

options("max.print" = 50000)
rice_topten

str(rice_topten)

tapply(rice_topten$production, rice_topten$year, length)

topten <- read.delim("rice.txt")

str(topten)

tapply(topten$production, topten$year, length)

# install.packages("xlsx")

# xlsx::write.xlsx(rice_topten, "okay.xlsx")

rice_topten <- read.delim("rice.txt")

rice_topten$area <-reorder(rice_topten$area, rice_topten$production, decreasing = TRUE)

# rice_ok -> rice_topten
# round(rice_topten$production/1000000, digits = 1) 

# rice_topten$ok_round <- round(rice_topten$production/1000000, digits = 1)
# 
# rice_topten$ok_round <- stri_sub(rice_topten$ok_round, 1, 5)
# 
# rice_topten$ok_round <- as.numeric(rice_topten$ok_round)
# 
# rice_topten$harvest <- round(rice_topten$area_harvested/1000000, digits = 1)

# stri_sub(rice_topten$ok_round, 1, 5)

options("max.print" = 50000)

library(stringi)
library(gganimate)
library(magick)
library(gifski)
library(viridis)
library(grid)
library(png)
library(ggimage)
library(cowplot)

plot(1:100)

options(scipen = 1, digits = 1)
p1 <- ggplot(rice_topten, aes(
  x = area, y = production,
  size = area_harvested,
  # colour = area,
  fill = area,
  label = production
)) +
  geom_point(alpha = 0.5, aes(color = area),
             shape = 21,
             color = "black"
  ) +
  geom_text(label = as.character(rice_topten$production),
            nudge_x = 0, nudge_y = 19,
    check_overlap = TRUE,
    size = 6, fontface = "bold"
  ) +
  scale_size(range = c(5, 20), name = "Diện tích canh tác (triệu ha)",
             guide = guide_legend(direction = "horizontal", title.position = "top",
                                  label.position="top", label.hjust = 0.5, label.vjust = 0.5,
                                  label.theme = element_text(angle = 0))) + 
    # scale_color_viridis(discrete = TRUE, name = "Region", option = "viridis") +
  guides(fill = "none") +
  
  # scale_color_manual(values = "blue") +
  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  theme_classic() +
  ylab("Sản lượng thu hoạch (triệu tấn)") +
  xlab(NULL) +
  
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
  labs(caption = c("Nguồn: FAOSTAT [crop dataset, flag A]", "Đồ thị: Duc Nguyen | tuhocr.com")) +
  
  theme(
    plot.title = element_text(size = 17, 
                              face = "bold", 
                              hjust = 0.5, vjust = -20, margin = margin(t = 5, r = 0, b = 0, l = 0),
                              lineheight = 1.3),
    plot.caption = element_text(size = 14, face = "bold", hjust = c(0, 1), margin = margin(t = 0, r = 0, b = 5, l = 0))
  ) +
  
  theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 0, r = 20, b = 20, l = 15),
    plot.background = element_rect(fill = "grey90", colour = "black", linewidth = 1)
  ) +
  
  theme(legend.position = c(0.60, 0.6), legend.direction = "horizontal",
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_blank(),
        legend.justification = c("left", "top"),
        # legend.box.just = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.5, "lines"),
        legend.spacing.y = unit(0.4, "lines"),
        legend.text = element_text(size = 16, face = "bold")) +
  geom_hline(yintercept = 50, colour = "#148c34", lwd = 1, lty = 2) + 
    annotate(
      geom = "text", x = 8, y = 57,
      label = 'atop(bold("Ngưỡng 50 triệu tấn/năm"))',
      color = "blue", size = 5, parse = TRUE
    ) +
# p2 <- ggdraw(p1) + 
#   draw_image("logor.png", x = 0.35, y = 0.3, scale = 0.1)
# 
# p2

  labs(title = "Top 10 quốc gia sản xuất lúa gạo trên thế giới giai đoạn 1960–2021 \nNăm: {frame_time}") +
  # transition_time(year) +
  transition_time(year) +
  # shadow_trail(distance = 0.05,
  #              alpha = 0.1,
  #              shape = 21,
  #              exclude_layer = ok) +
  shadow_wake(wake_length = 0.2, size = 3, alpha = 0.05, colour = "grey97") +
  ease_aes('linear')

animate(p1, fps = 8,
        # nframes = 130,
        detail = 7,
        width = 800, height = 600)

anim_save("61.gif") 
 

# https://github.com/thomasp85/gganimate/issues/94


# https://stackoverflow.com/questions/51919498/latest-gganimate-how-to-have-a-fixed-plot-in-the-background





# https://stackoverflow.com/questions/53852428/insert-image-png-ggplot2-cowplot
















