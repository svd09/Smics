#' @title Summary statistics with histogram
#'
#' @description summary statistics with histogram of a variable.
#' @param x list,vector,column of a dataframe
#' @keywords
#' @import pastecs
#' @examples \dontrun{
#' x = c(1,2,3,4,5,6,7,8,9,10)
#' proc_desc(x)
#' }
proc_desc <- function(x){
  require(pastecs)
  result <- round(pastecs::stat.desc(x),2)
  hist(x, col = "skyblue")
  result
}
