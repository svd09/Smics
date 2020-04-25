#' @title A function to create personalized KM curve for 1 group
#' @author Salil Deo
#' @description Provides KM curve for 1 group with CI band
#' @param s survfit object
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @import ggplot2
#' @import broom
#' @param color string providing color label
#' @seealso \code{\link[survival]{survfit}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this
#' library(survival)
#' s = survfit(Surv(time, status) ~ 1, data = lung)
#' figure <- proc_kmcurve2(s = s, xlab = "follow-up", ylab = "proportion surviving",
#' color = "blue")
#' gets an object figure is a ggplot2 object; it can be further modified if needed.
#' }

proc_kmcurve1 <- function(s,xlab,ylab,color){
  require(ggplot2)
  require(broom)
  require(my_theme)
  df <- tidy(s) # get the tidy summary suvfit object
  finalt <- max(df$time) # maximum time set for graph
  # start preparing the graph

  g <- ggplot(data = df, aes(x = time, y = estimate)) +
    geom_line() +
    geom_ribbon(data = df, aes(ymax = conf.high, ymin = conf.low), alpha = 0.2,fill = color)
  g2 <- g + xlab(xlab) + ylab(ylab) + ylim(0,1)
  g3 <- g2 + my_theme()
  g3
}


