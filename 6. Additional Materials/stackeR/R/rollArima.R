#' rollArima
#'
#' rolling forecasts for ARIMA models
#'
#' @param arima.model an arima model, from forecast::auto.arima()
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
rollArima<-function(arima.model,ynew,horizon=1)
{
  prevARIMA<-array(0,dim=length(ynew))
  prevARIMA[1]<-forecast(arima.model, h=horizon)$mean[horizon]
  
  for(i in 1:(length(ynew)-1))
  {
    ts2<-c(arima.model$x,ynew[1:i])
    refit<-Arima(ts2, model=fit.arima)
    prevARIMA[i+1]<-forecast(refit, h=horizon)$mean[horizon]
  }
  return(prevARIMA)
}






