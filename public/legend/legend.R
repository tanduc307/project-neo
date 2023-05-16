source(file = "extract_item.R")

rice_country <- extract_item(input_item = "Rice",
                             input_rds = "data_raw/crop_production_all_data.rds",
                             input_region = "data_raw/FAOSTAT_data_3-21-2023.csv")

vietnam <- rice_country |> subset(area == "Viet Nam")

thailand <- rice_country |> subset(area == "Thailand")

egypt <- rice_country |> subset(area == "Egypt")

china <- rice_country |> subset(area == "China")

japan <- rice_country |> subset(area == "Japan")

plot(yield ~ year, data = egypt,
     type = "l", 
     col = "blue", 
     lwd = 3, 
     pch = 17,
     ylim = c(0, 12)
     )

lines(yield ~ year, data = thailand,
      type = "l", 
      col = "red", 
      lwd = 3, 
      pch = 17
      )

lines(yield ~ year, data = vietnam,
      type = "l", 
      col = "darkgreen", 
      lwd = 3, 
      pch = 17
)

lines(yield ~ year, data = china,
      type = "l", 
      col = "purple", 
      lwd = 3, 
      pch = 17
)

lines(yield ~ year, data = japan,
      type = "l", 
      col = "#f4c430", 
      lwd = 3, 
      pch = 17
)


legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

################ THAY ĐỔI VỊ TRÍ LEGEND, CŨNG NHƯ CHÈN IMAGE FLAG LUÔN.


legend("bottomleft", 
       legend = c("Line 1", "Line 2"),
       col = c("red", "blue"), 
       lty = 1:2, 
       cex=0.8)











# Make a basic graph
plot(mtcars$mpg ~ mtcars$wt, 
     type = "b", 
     bty = "l", 
     col = "blue", 
     lwd = 3, 
     pch = 17
     )

lines(a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

# Add a legend
legend("bottomleft", 
       legend = c("Group 1", "Group 2"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))