source(file = "extract_item.R")
source(file = "ready_data.R")
source(file = "produce_heatmap.R")

#############################
### CHẠY TRƯỚC HEATMAP RICE_COUNTRY 128 QUỐC GIA ĐỂ LẤY THÔNG SỐ CHUẨN

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

###########

z <- ready_data(input_data = rice_country, ty_le = 1000000)

#################

produce_image_fixed(input_data = z,
                    file_name = "rice_country_128")

####
produce_image(input_data = z[1:10, ],
              file_name = "rice_country_top_10")


#################################

coffee_country <- extract_item(input_item = "Coffee, green",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

z <- ready_data(input_data = coffee_country, ty_le = 1000)


################ MAI THÊM THÔNG SỐ VÀO HEATMAP ĐỂ RE-PRODUCE RA LẠI CHO ĐÀNG HOÀNG.


############## BAO GỒM CẢ SCALE MÀU


produce_image(input_data = z,
              file_name = "coffee_country_v9",
              don_vi = "Đơn vị: \nnghìn tấn",
              tua_de = "Các quốc gia sản xuất cà phê trên thế giới (1961–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com",
              scale_color = c("#00ffff", hsv(0.15, seq(0.1, 0.9, length.out = 7), 1), 
                              "#ffd966", "#f1c232", "#e69138", "#b45f06", "#783f04"))





source(file = "produce_heatmap_enhance.R")











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































