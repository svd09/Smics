#' @title A function to create personalized KM curve for 1 group
#' @author Salil Deo
#' @description Label survival values for KM curve
#' @param s survfit object
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param times time for labelling survival values provide as list
#' @import ggplot2
#' @import broom
#' @param color string providing color label
#' @seealso \code{\link[survival]{survfit}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this
#' # needs my_theme already in .env
#' library(survival)
#' s = survfit(Surv(time, status) ~ 1, data = lung)
#' figure <- proc_kmcurve2(s = s, xlab = "follow-up",
#' ylab = "proportion surviving",
#' color = "blue", times = c(300,500))
#' gets an object figure is a ggplot2 object; it can be further modified if needed.
#' }

proc_km_label <- function(s,xlab,ylab,color,times){
  require(ggplot2)
  require(broom)

  df <- tidy(s) # get the tidy summary suvfit object

  # start preparing the graph

  g <- ggplot(data = df, aes(x = time, y = estimate)) +
    geom_line() +
    geom_ribbon(data = df, aes(ymax = conf.high, ymin = conf.low), alpha = 0.2,fill = color)
  g2 <- g + xlab(xlab) + ylab(ylab) + ylim(0,1)
  g3 <- g2
  g3



  proc_percent <- function(x){
    y <- round((x*100),2)
    y
  }



  t <- summary(s,times = times)

  times2 <- t$time
  values <- t$surv
  # est <- paste0((round(values,2)*100),"%",sep = "")
  est <- proc_percent(values)

  df2 <- data.frame(times2, values,est)


  g4 <- g3 + geom_label(data = df2, aes(x = times2, y = values,
                                        label = est))

  g4

}


