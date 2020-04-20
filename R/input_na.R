#' A function to convert to NA in column in dataframe
#'
#' This function allows you to change a particular value in a col to NA
#' The function can be used for any type of data structure
#' @param x = your input col, needs to be in the format df$x
#' @param y = value/or values that needs to be changed to NA
#' @param y = if more than 1 value, then use c(a,b,c) format
#' @keywords missing
#' @export
#' @examples

#' x <- c(1,2,-9)
#' z <- input_na(x,-9)


input_na <- function(x, y){

  ifelse(x %in% y, NA, x)
}



