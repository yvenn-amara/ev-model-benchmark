#' rollHW
#'
#' rolling forecasts for Holt Winters exponential smoothing models (forecast::HoltWinters)
#'
#' @param hw.model a Holt Winters exponential smoothing model, from forecast::HoltWinters()
#' @param ynew the time serie to predict
#' @param horizon the forecast horizon
#' 
#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' rmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'
#'

rollHW<-function(hw.model,ynew,horizon=1)
{
  prevHW<-array(0,dim=length(ynew))
  prevHW[1]<-forecast(hw.model, h=horizon)$mean[horizon]
  
  for(i in 1:(length(ynew)-1))
  {
    ts2<-ts(c(hw.model$x,ynew[1:i]),frequency=frequency(fit.hw$x))
    refit<-HoltWinters(ts2)
    prevHW[i+1]<-forecast(refit, h=horizon)$mean[horizon]
  }
  return(prevHW)
}


