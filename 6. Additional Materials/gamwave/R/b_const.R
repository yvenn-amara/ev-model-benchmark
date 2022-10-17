#' b_const.R
#'
#' \code{b_const} Utility internal function  
#'
#' @param j integer (desired level)
#' @param v_phi variance of phi
#' @return b_const
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
b_const <- function(j, v_phi){
out <- a_const(j) - (log(pi) + log(log(2)) + log(j) - 0.5 * log(1+v_phi)) / (2 * a_const(j))
return(out)
}
