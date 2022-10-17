#' LevelTest.R
#'
#' \code{LevelTest} Function to perform the test of wavelet coefficients at a single level
#' Internal function useful for computing  a confidence bands for the 
#' wavelet based components of amn additive model. 
#'
#' @param wc vector of wavelet coefficients at that level.
#' @param theta boundary of the null hypothesis
#' @param thr threshold for the rejection region
#' @return 1 if the null is rejected / = 0 if not rejected
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
LevelTest<-function(wc, theta, thr, n){
wc <- abs(wc)
N  <- length(wc)
sum_thr = sum(wc[wc > thr],na.rm=T) - N * MeanThr(theta, thr)
if (thr <= 1) out = (sum_thr > sqrt((1+theta^2) * N * log(n))) # 1
if (thr>1) out = (sum_thr > sqrt(0.25 * N * log(n)) * (theta + sqrt(2.5 * log(n)))) # 0.25, 2.5
return(as.integer(out))
}