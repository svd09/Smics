#' A function to convert to NA in column in dataframe
#'
#' @title Convert values to NA
#' @description Convert any specified values to NA in list, column
#' @param x your input col, needs to be in the format df$x
#' @param y value/or values that needs to be changed to NA
#' @param y if more than 1 value, then use c(a,b,c) format
#' @keywords missing
#' @export
#' @examples \dontrun{
#' x <- c(1,2,-9)
#' z <- input_na(x,-9)
#' }
#' @examples \dontrun{
#' this can also be used for more than 1 value
#' x <- c(1,2,3,4,5,-9,-99)
#' z <- input_na(x, c(-9,-99))
#' }

input_na <- function(x, y){

  ifelse(x %in% y, NA, x)
}



