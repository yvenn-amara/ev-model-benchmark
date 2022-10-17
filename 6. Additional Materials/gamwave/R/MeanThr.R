#' MeanThr.R
#' 
#' \code{MeanThr} Function used to compute the mean of a thresholded half normal r.v.e
#' This is an internal function of package gamwave which allows to compute a confidence bands for the 
#' wavelet based components of amn additive model. 
#'
#' @param theta the mean
#' @param thr the threshold
#' @return the mean threshold 
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
MeanThr <-function(theta, thr){
out <- dnorm(thr + theta) + dnorm(thr - theta) + theta * (pnorm(thr + theta) - pnorm(thr - theta))
return(out)
}