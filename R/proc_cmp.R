#' @title A function to create competing risk curve
#'     for 1 group with user specified labels
#' @author Salil Deo
#' @description Competing risk curve with 1 group
#' @param c a competing risk object with cuminc function
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param times time to label survival values provide as list
#' @import ggplot2
#' @import broom
#' @import cmprsk
#' @import survival
#' @seealso \code{\link[survival]{survfit}}
#' @seealso \code{\link[cmprsk]{cuminc}}
#' @return NULL
#' @examples \dontrun{
#' # do not run this
#' library(cmprsk)
#' c = cmprsk""cuminc(df$time, df$event)
#' after obtaining the cmprsk object get figure with labelling at desired
#'     specific time points
#' figure <- proc_cmp(c = c, times = c(2000,4000), color = 'red)
#' gets an object figure is a ggplot2 object; it can be further modified if needed.
#' }




proc_cmp <- function(c, times, color){
  require(survival)
  require(cmprsk)
  require(ggplot2)
  require(dplyr)
  require(magrittr)
  require(ggthemes)

  # c is from the cmprsk object
  # create dataframe of the cmprsk object for event of interest

  df <- c[[3]] %>% tbl_df()



  df$value <- df$est
  df$se <- sqrt(df$var)
  df$low <- df$value - 1.96*df$se
  df$high <- df$value + 1.96*df$se

  df$lci <- with(df, ifelse(low < 0, 0, low))
  df$uci <- with(df, ifelse(high < 0, 0, high))

  # create percent values

  df$rate <- df$value*100
  df$perc_lci <- df$lci*100
  df$perc_uci <- df$uci*100

  # draw the curve

  p <- ggplot(data = df, aes(x = time, y = rate)) +
       ggplot2::geom_line(fill = color) +
    ggplot2::geom_ribbon(data = df, aes(ymin = perc_lci,
       ymax = perc_uci),fill = color, alpha = 0.2) +
    ggplot2::theme(legend.position = 'none')


  p2 <- p + theme_minimal()


  # create labels for the graph

  times <- times

  t <- timepoints(c, times = times)

  test <- t$est

  est1 <- test[3]
  est2 <- t$est[6]


  est1 <- round(est1*100,2)
  est2 <- round(est2*100,2)


  times <- times

  est <- c(est1,est2)

  dft <- data.frame(times, est)

  p3 <- p2 + geom_label_repel(data = dft, aes(x = times, y = est,
                        label = paste0(est, '%', sep = "")))

  p3
}



