rice_country$yield <- round(rice_country$production / rice_country$area_harvested, digits = 2)

head(rice_country, n = 100)

table(rice_country$yield)

summary(rice_country)

options("max.print" = 100000)
inf_check <- subset(rice_country, yield == Inf)

nan_check <- rice_country[is.nan(rice_country$yield), ]


rice_country$yield[is.nan(rice_country$yield)] <- 0

rice_country$yield[is.infinite(rice_country$yield)] <- 0

rice_country[1388:1403, ]

################################################################


summary(bananas_country)

bananas_country


################

crop_full <- readRDS("data_raw/crop_production_all_data.rds")

names(crop_full)


head(crop_full, n = 100)

read.csv("data_raw_20230422/FAOSTAT_data_4-22-2023.csv") -> country_group

country_group

head(country_group, n = 200)

table(crop_full$item, crop_full$element)


ok <- as.data.frame(table(crop_full$item, crop_full$element))

library(kableExtra)

ok_5 %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", 
                                        "hover", 
                                        "condensed", 
                                        "bordered", 
                                        "responsive")) %>%
    kable_classic(full_width = FALSE, html_font = "arial") -> output

save_kable(output, file = "ok_5.html")

names(ok)

library(dplyr)

ok |> dplyr::arrange(Freq) -> ok_1

ok_1

ok_2 <- ok_1 |> subset(Freq != 0)

butter_goat_milk <- crop_full |> subset(item == "Butter of goat milk")

ok_2 |> dplyr::arrange(Var1, Var2, Freq) -> ok_3

sort(table(ok_3$Var1))

ok_3

class(ok_3)

dim(ok_3)

crop_1 <- ok_3 |> subset(Var2 == "area_harvested")

ok_3

head(ok_3, n = 100)

ok_4 <- reshape(data = ok_3,
                    idvar = c("Var1"),
                    v.names = "Freq",
                    timevar = "Var2",
                    direction = "wide") 


table(crop_full$element)

as.data.frame(table(crop_full$element))


table(!is.na(ok_4$Freq.area_harvested))

table(!is.na(ok_4$Freq.production))

table(!is.na(ok_4$Freq.area_harvested) & !is.na(ok_4$Freq.production))

ok_4[!is.na(ok_4$Freq.area_harvested) & !is.na(ok_4$Freq.production), ] -> ok_5

as.character(ok_5$Var1) -> crop_item


other_citrus_fruit <- crop_full |> subset(item == "Other citrus fruit, n.e.c.")











