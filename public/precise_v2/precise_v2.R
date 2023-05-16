source(file = "extract_item.R") ## load function để trích xuất dữ liệu theo item
source(file = "ready_data.R") ## load function để xử lữ liệu phù hợp vẽ heatmap
source(file = "produce_heatmap_enhance_v2.R") 

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

z <- ready_data(input_data = rice_country, ty_le = 1000000)

produce_image_fixed(input_data = z,
                    file_name = "rice_country_128y1")

produce_image(input_data = z[1:30, 10:60],
              file_name = "rice_1a",
              don_vi = "Đơn vị: \ntriệu tấn",
              tua_de = "Các quốc gia sản xuất lúa gạo trên thế giới (...–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com"
              ) 

produce_image(input_data = z[, 30:60],
              file_name = "rice_2a",
              don_vi = "Đơn vị: \ntriệu tấn",
              tua_de = "Các quốc gia sản xuất lúa gạo trên thế giới (...–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com"
) 

produce_image(input_data = z[1:30, 30:60],
              file_name = "rice_1a",
              don_vi = "Đơn vị: \ntriệu tấn",
              tua_de = "Các quốc gia sản xuất lúa gạo trên thế giới (...–2021) | Nguồn: FAOSTAT | Đồ họa: tuhocr.com"
)

produce_image(input_data = z,
              file_name = "rice_8l",
              don_vi = "Đơn vị: \ntriệu tấn",
              tua_de = "Các quốc gia sản xuất lúa gạo trên thế giới (...–2021)",
              thang_do = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
) 


produce_image(input_data = z[, 30:61],
              file_name = "rice_8l",
              don_vi = "Đơn vị: \ntriệu tấn",
              tua_de = "Các quốc gia sản xuất lúa gạo trên thế giới (1990–2021)",
              thang_do = c(0, 0.0000000001, 1, 10, 30, 60, 90, 100, 130, 160, 200, 230, 260),
              tua_de_sub = bquote(italic('Thứ tự của')~italic(.(as.character(nrow(z[, 30:61]))))~italic("quốc gia được sắp xếp từ lớn đến nhỏ."))
) 

################################