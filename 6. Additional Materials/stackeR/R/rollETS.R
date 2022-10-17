#' rollETS
#'
#' rolling forecasts for exponential smoothing models (forecast::ets)
#'
#' @param ets.model an exponential smoothing model, from forecast::ets()
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
rollETS<-function(ets.model,ynew,horizon=1)
{
  prevETS<-array(0,dim=length(ynew))
  prevETS[1]<-forecast(ets.model, h=horizon)$mean[horizon]
  
  for(i in 1:(length(ynew)-1))
  {
    ts2<-c(ets.model$x,ynew[1:i])
    refit<-ets(ts2, model=ets.model)
    prevETS[i+1]<-forecast(refit, h=horizon)$mean[horizon]
  }
  return(prevETS)
}
