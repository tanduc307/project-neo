
crop_full <- readRDS("data_raw/crop_production_all_data.rds")

dim(crop_full)

head(crop_full)

View(crop_full)

library(kableExtra)

crop_full[c(1:1000, 3760216:3761216), ] %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: red'>Dataset Crop QCL từ FAOSTAT gồm 37 triệu dòng và 14 cột (cột đầu là STT) | Minh họa: tuhocr.com</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này chỉ thể hiện 1000 giá trị đầu và 1000 giá trị cuối" = 14), bold = TRUE, color = "white", background = "darkgreen")-> output

save_kable(output, file = "crop_full123.html")

#######################

source(file = "extract_item.R")

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

rice_country
dim(rice_country)

length(unique(rice_country$area))


library(kableExtra)

cbind(" " = 1:7174, rice_country) %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: blue'>Dataset [item == 'Rice'] từ FAOSTAT gồm 7174 dòng và 7 cột | Minh họa: tuhocr.com</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này đã được làm sạch, thể hiện tình hình sản xuất lúa gạo ở 128 quốc gia và vùng lãnh thổ trên thế giới từ 1961 đến 2021. \nSắp xếp theo thứ tự từ lớn đến nhỏ tính theo sản lượng thu hoạch. Đơn vị: production (tấn), area_harvested (ha), yield (tấn/ha)." = 7), escape = TRUE, bold = TRUE, color = "darkgreen", background = "yellow")-> output

save_kable(output, file = "rice_country.html")

##############################

coffee_country <- extract_item(input_item = "Coffee, green",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

coffee_country
dim(coffee_country)
length(unique(coffee_country$area))

library(kableExtra)

cbind(" " = 1:4904, coffee_country) %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: blue'>Dataset [item == 'Coffee, green'] từ FAOSTAT gồm 4904 dòng và 7 cột | Minh họa: tuhocr.com</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này đã được làm sạch, thể hiện tình hình sản xuất cà phê ở 85 quốc gia và vùng lãnh thổ trên thế giới từ 1961 đến 2021. \nSắp xếp theo thứ tự từ lớn đến nhỏ tính theo sản lượng thu hoạch. Đơn vị: production (tấn), area_harvested (ha), yield (tấn/ha)." = 7), bold = TRUE, color = "white", background = "brown")-> output

save_kable(output, file = "coffee_country.html")

##################

piper_country <- extract_item(input_item = "Pepper (Piper spp.), raw",
                               input_rds = "data_raw/crop_production_all_data.rds",
                               input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

length(unique(piper_country$area))
dim(piper_country)

cbind(" " = 1:1974, piper_country) %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: blue'>Dataset [item == 'Pepper (Piper spp.), raw'] từ FAOSTAT gồm 1974 dòng và 7 cột | Minh họa: tuhocr.com</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này đã được làm sạch, thể hiện tình hình sản xuất hồ tiêu ở 44 quốc gia và vùng lãnh thổ trên thế giới từ 1961 đến 2021. \nSắp xếp theo thứ tự từ lớn đến nhỏ tính theo sản lượng thu hoạch. Đơn vị: production (tấn), area_harvested (ha), yield (tấn/ha)." = 7), bold = TRUE, color = "white", background = "darkgreen")-> output

save_kable(output, file = "piper_country.html")

#################

chilly_dry_country <- extract_item(input_item = "Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw",
                              input_rds = "data_raw/crop_production_all_data.rds",
                              input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

length(unique(chilly_dry_country$area))
dim(chilly_dry_country)

cbind(" " = 1:3344, chilly_dry_country) %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: blue'>Dataset [item == 'Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw'] từ FAOSTAT gồm 3344 dòng và 7 cột</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này đã được làm sạch, thể hiện tình hình sản xuất ớt (khô) ở 74 quốc gia và vùng lãnh thổ trên thế giới từ 1961 đến 2021. \nSắp xếp theo thứ tự từ lớn đến nhỏ tính theo sản lượng thu hoạch. Đơn vị: production (tấn), area_harvested (ha), yield (tấn/ha)." = 7), bold = TRUE, color = "red", background = "#F5F5F5")-> output

save_kable(output, file = "chilly_dry_country.html")

#################

coconut_country <- extract_item(input_item = "Coconuts, in shell",
                                   input_rds = "data_raw/crop_production_all_data.rds",
                                   input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

length(unique(coconut_country$area))
dim(coconut_country)

cbind(" " = 1:4644, coconut_country) %>%
    kbl(escape = FALSE, caption = "<center><strong><span style='color: blue'>Dataset [item == 'Coconuts, in shell'] từ FAOSTAT gồm 3344 dòng và 7 cột | Minh họa: tuhocr.com</span></strong></center>",
        format = "html") %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") %>% 
    add_header_above(c("Dataset này đã được làm sạch, thể hiện tình hình sản xuất dừa khô ở 91 quốc gia và vùng lãnh thổ trên thế giới từ 1961 đến 2021. \nSắp xếp theo thứ tự từ lớn đến nhỏ tính theo sản lượng thu hoạch. Đơn vị: production (tấn), area_harvested (ha), yield (tấn/ha)." = 7), bold = TRUE, color = "white", background = "#A84532")-> output

save_kable(output, file = "coconut_country.html")







