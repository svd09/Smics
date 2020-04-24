#' @title A function to provide descriptive statistics.
#'
#' @description Provides descriptive statistics as SPSS format.
#' @param x column in a dataframe
#' @seealso \code{\link[gmodels]{CrossTable}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this
#' x = c(1,2,3,4,5,6,7,8,9,10)
#' proc_tab(x)
#' }



proc_tab <- function(x){

  CrossTable(x, format = "SPSS")


}
