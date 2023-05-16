crop_full <- readRDS(file = "data_raw/crop_production_all_data.rds")

piper <- crop_full |> subset(item == "Pepper (Piper spp.), raw")

faso <- piper |> subset(area == "Burkina Faso")



chilly_green <- crop_full |> subset(item == "Chillies and peppers, green (Capsicum spp. and Pimenta spp.)")

vn <- chilly_green |> subset(area == "Viet Nam")


View(faso)

z

class(z)

dim(z)

class(range(z, na.rm = TRUE))

range(z, na.rm = TRUE)[1]

round(range(z, na.rm = TRUE)[2]*1.2, digits = -1)


c(0, 0.000000001, 1, 10, 100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)

seq(from = 100, to = round(range(z, na.rm = TRUE)[2]*1.2, digits = -1),
    length.out = 9)
?seq



c(0, 0.000000001, 1, 10, 
  round(seq(from = 100, 
      to = round(range(z, na.rm = TRUE)[2]*1.2, digits = -1),
      length.out = 9), digits = -2))


c(0, 0.000000001, 1, 10, 
  seq(from = 100, 
      to = round(range(z, na.rm = TRUE)[2]*1.2, digits = -1),
      length.out = 9))