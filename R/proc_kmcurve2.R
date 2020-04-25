#' @title A function to create personalized KM curve for 2 or more groups
#' @author Salil Deo
#' @description Provides KM curve for 2/more groups with CI bands
#' @param s survfit object
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @import ggplot2
#' @import broom
#' @seealso \code{\link[survival]{survfit}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this
#' # needs my_theme in th e .env
#' library(survival)
#' s = survfit(Surv(time, status) ~ sex, data = lung)
#' figure <- proc_kmcurve2(s = s, xlab = "follow-up", ylab = "proportion surviving")
#' gets an object figure is a ggplot2 object; it can be further modified if needed.
#' }


proc_kmcurve2 <- function(s,xlab,ylab){
  require(ggplot2)
  require(broom)

  df <- broom::tidy(s) # get the tidy summary suvfit object
  finalt <- max(df$time) # maximum time set for graph
  # start preparing the graph

  g <- ggplot(data = df, aes(x = time, y = estimate, color = strata)) +
    geom_line() +
    geom_ribbon(data = df, aes(ymax = conf.high, ymin = conf.low, fill = strata), alpha = 0.2)
  g2 <- g + xlab(xlab) + ylab(ylab) + ylim(0,1)
  g3 <- g2 + my_theme()
  g3
}
