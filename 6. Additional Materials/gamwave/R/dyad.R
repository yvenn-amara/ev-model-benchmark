#' dyad.R
#'
#' \code{dyad} Utility function  that returns location indices at level j
#'
#' @param j integer (desired level)
#' @return all location indices at scale j
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
dyad<-function(j) { 
i=(2^(j)+1):(2^(j+1))
return(i)
}