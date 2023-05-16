ready_data <- function(input_data,
                       ty_le) {
    
    input_data -> rice_country
    
    rice_country -> rice_ready
    
    rice_ready -> rice_check
    
    matrix_rice <- rice_check[, c(1, 3, 4)]
    
    matrix_rice$area <- reorder(matrix_rice$area, matrix_rice$production)
    
    matrix_rice <- matrix_rice |> dplyr::arrange(desc(area), year)
    
    matrix_1 <- reshape(data = matrix_rice,
                        idvar = c("area"),
                        v.names = "production",
                        timevar = "year",
                        direction = "wide") 
    
    colnames(matrix_1)[2:62] <- unique(matrix_rice$year)
    
    matrix_2 <- matrix_1[, 2:62]
    
    rownames(matrix_2) <- matrix_1[, 1]
    
    #########
    
    v <- matrix_2
    ## convert qua matrix
    v <- as.matrix(v)
    
    z <- v/ty_le
    
    return(z)
    
}