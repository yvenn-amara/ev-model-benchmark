#' c_const.R
#'
#' \code{c_const} Utility internal function  
#'
#' @param j integer (desired level)
#' @param sigma_phi sd of phi
#' @param sigma sd of noise
#' @return c_const
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
c_const <- function(j, sigma_phi, sigma){
out <- (sigma_phi / sigma)* 2^(j/2)
return(out)
}