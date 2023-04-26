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
    
    ## ĐỔI TÊN DATASET
    
    rice_ready -> item_ready
    
    ## TÍNH NĂNG SUẤT, NẾU CÓ NAN HOẶC INF THÌ SET BẰNG 0
    
    item_ready$yield <- round(item_ready$production / item_ready$area_harvested, digits = 2)
    
    item_ready$yield[is.nan(item_ready$yield)] <- 0
    
    item_ready$yield[is.infinite(item_ready$yield)] <- 0
    
    item_ready$area <- reorder(item_ready$area, item_ready$production)
    
    item_ready <- item_ready %>% dplyr::arrange(desc(area), 
                                                desc(year),
                                                desc(production),
                                                desc(area_harvested),
                                                desc(yield)
    )
    
    return(item_ready)
    
}