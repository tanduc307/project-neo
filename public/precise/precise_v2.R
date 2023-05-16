source(file = "extract_item.R") ## load function để trích xuất dữ liệu theo item
source(file = "ready_data.R") ## load function để xử lữ liệu phù hợp vẽ heatmap
source(file = "produce_heatmap_enhance.R") 

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

z <- ready_data(input_data = rice_country, ty_le = 1000000)

produce_image_fixed(input_data = z,
                    file_name = "rice_country_128")