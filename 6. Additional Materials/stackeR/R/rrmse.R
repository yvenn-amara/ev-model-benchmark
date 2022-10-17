#' Relative Root Mean Square Error
#'
#' compute the Relative Root Mean Square Error of a prediction vector
#'
#' @param y the observations to be predicted
#' @param ychap the predictions
#' @param digits the precision in number of digits
#'
#' @return a positive real number the RRMSE
#'
#' @examples
#' y<-rnorm(10,5,1)
#' ychap<-rep(0,10)
#' rrmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#' 

rrmse<-function(y,ychap,digits=3)
{
  eps=y-ychap
  return(signif(sqrt(mean(eps^2,na.rm=TRUE))/mean(y),digits=digits))
}
