#' A function for EDA of continuous variables
#'
#' This function provides a histogram and important values of your variable.
#' @param x = your input variable
#' @keywords
#' @export
#' @examples
#' please install pastecs library before using this function.
#' x = c(1,2,3,4,5,6,7,8,9,10)
#' proc_desc(x)



proc_tab <- function(x){
  # require libary(gmodels)
  CrossTable(x, format = "SPSS")

}
