#' @title A function to create personalized KM curve for 2 or more groups
#' @author Salil Deo
#' @description Label survival values for KM curve
#' @param s survfit object
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param times time to label survival values provide as list
#' @import ggplot2
#' @import broom
#' @seealso \code{\link[survival]{survfit}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this

#' library(survival)
#' s = survfit(Surv(time, status) ~ sex, data = lung)
#' figure <- proc_kmcurve2(s = s, xlab = "follow-up",
#' ylab = "proportion surviving", times = c(200,500))
#' gets an object figure is a ggplot2 object; it can be further modified if needed.
#' }


proc_km_label2 <- function(s,xlab,ylab,times){
  require(ggplot2)
  require(broom)


  df <- broom::tidy(s) # get the tidy summary suvfit object

  # start preparing the graph

  g <- ggplot(data = df, aes(x = time, y = estimate, color = strata)) +
    geom_line() +
    geom_ribbon(data = df, aes(ymax = conf.high, ymin = conf.low, fill = strata), alpha = 0.2)
  g2 <- g + xlab(xlab) + ylab(ylab) + ylim(0,1) +
    theme(legend.position = "none")
  g2



  proc_percent <- function(x){
    y <- round((x*100),2)
    y
  }



  t <- summary(s,times = times)

  times2 <- t$time
  values <- t$surv
  est <- paste0((round(values,2)*100),"%",sep = "")
  #est <- proc_percent(values)

  strata <- t$strata

  df2 <- data.frame(times, surv, strata)

  g3 <- g2 + geom_label(data = df2, aes(x = times2, y = values,
                                        label = est))

  g3

}
