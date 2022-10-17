#' Mean Absolute Percentage Error
#'
#' compute the Mean Absolute Percentage Error of a prediction vector
#'
#' @param y the observations to be predicted
#' @param ychap the predictions
#' @param digits the precision in number of digits
#'
#' @return MAPE in percentage
#'
#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' mape(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export


mape<-function(y,ychap,digits=3)
{
  return(signif(100*mean(abs(y-ychap)/abs(y),na.rm=TRUE),digits=digits))
}