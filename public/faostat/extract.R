#### BƯỚC 1: GET RAW DATA ####

# library(FAOSTAT)
# data_folder <- "data_raw_20230422"
# dir.create(data_folder)
# 
# crop_production <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = data_folder)
# 
# saveRDS(crop_production, file = "data_raw_20230422/crop_production_all_data.rds")

#### BƯỚC 2: FUNCTION HÓA EXTRACT DATA

# "data_raw_20230422/FAOSTAT_data_4-22-2023.csv"

extract_item <- function(input_item, input_rds, input_region){
    
    # CLEAN DATA
    
    ## 2.1) TÁCH ITEM QUAN TÂM
    
    crop_full <- readRDS(input_rds)
    
    crop_full_clean <- crop_full[ , c(3, 6, 8, 10, 11, 12)]
    
    rice_all <- subset(crop_full_clean, item == input_item)
    
    ## 2.2) TÁCH QUỐC GIA KHỎI THÔNG TIN REGION
    
    read.csv(input_region) -> country_group
    
    region_name <- unique(country_group$Country.Group)
    
    country_name <- unique(rice_all$area)
    
    country_1 <- country_name[!(country_name %in% region_name)]
    
    rice_country <- rice_all[rice_all$area %in% country_1, ]
    
    ## 2.3) SẮP XẾP DATA THEO TRẬT TỰ
    
    library(dplyr)
    
    rice_country <- rice_country %>% dplyr::arrange(area, 
                                                    desc(year),
                                                    desc(element),
                                                    desc(value)
    )
    
    #### BƯỚC 3: CHECK DATA AND CLEAN ####
    
    ## SỬ DỤNG LỆNH TAPPLY ĐỂ TÌM THÔNG TIN CÁC QUỐC GIA KHÔNG SẢN XUẤT GẠO
    
    check_1 <- tapply(rice_country$value, 
                      list(rice_country$area, rice_country$element), 
                      FUN = sum, na.rm = TRUE)
    
    check_2 <- as.data.frame(check_1)
    
    check_2$area <- row.names(check_2)
    
    check_2[, c(4, 1:3)] -> check_2
    
    row.names(check_2) <- NULL
    
    check_2 |> dplyr::arrange(area_harvested, production, yield, area) -> check_3
    
    country_check <- check_3 |> subset(area_harvested == 0)
    
    ## SUBSET NHỮNG QUỐC GIA CÓ SẢN XUẤT GẠO
    
    country_name_1 <- unique(rice_country$area)
    
    country_2 <- country_name_1[!(country_name_1 %in% country_check$area)]
    
    rice_country_2 <- rice_country[rice_country$area %in% country_2, ]
    
    #### BƯỚC 4: RESHAPE DATA ####
    
    rice_country_2 <- rice_country_2 |> subset(select = -unit)
    
    rice_ready <- reshape(data = rice_country_2,
                          idvar = c("year", "area"),
                          v.names = "value",
                          timevar = "element",
                          direction = "wide") 
    
    ## BỎ CỘT YIELD
    
    rice_ready <- rice_ready[, c(1, 2, 3, 5, 6)]
    
    ## BỎ MISSING VALUE, CHỈNH LẠI TÊN CỘT VÀ ROW.NAMES
    
    rice_ready <- na.omit(rice_ready)
    
    attributes(rice_ready)$na.action <- NULL
    
    names(rice_ready)[4] <- "production"
    names(rice_ready)[5] <- "area_harvested"
    row.names(rice_ready) <- NULL 
    
    rice_ready -> item_ready
    
    item_ready$yield <- round(item_ready$production / item_ready$area_harvested, digits = 2)
    
    return(item_ready)

}

#### BƯỚC 3: Rice

rice_country <- extract_item(input_item = "Rice",
             input_rds = "data_raw_20230422/crop_production_all_data.rds",
             input_region = "data_raw_20230422/FAOSTAT_data_4-22-2023.csv")


#### BƯỚC 4: HTML

library(kableExtra)

rice_country %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") -> output

save_kable(output, file = "rice_country.html")

#### BƯỚC 5: RVEST
# https://cran.r-project.org/web/packages/rvest/vignettes/rvest.html

library(rvest)
library(xml2)
library(httr)

html <- read_html("https://studyr.netlify.app/faostat/rice_country.html")

html %>% 
    html_node("table") %>% 
    html_table() -> rice_html

rice_html <- as.data.frame(rice_html)

identical(rice_html, rice_country)



















