#' Root Mean Square Error
#'
#' compute the Root Mean Square Error of a prediction vector
#'
#' @param y the observations to be predicted
#' @param ychap the predictions
#' @param digits the precision in number of digits
#'
#' @return a positive real number the RMSE
#'
#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' rmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export

rmse<-function(y,ychap,digits=3)
{
  return(signif(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=digits))
}

