#' predicthybrid
#'
#' hybrid model forecasting
#'
#' @param hybrid an hybrid model fitted object
#' @param data dataset on which we forecast
#'
#' @return a vector with the forecast
#' @export
#'
predict.hybrid <- function(hybrid, newdata){

  ## wavelet part forecasting
  hybrid$gmw$forecast <- predict.gamwasp(hybrid$gmw, newdata)$forecast

  ## spline part forecasting
  hybrid$gsp$forecast <- predict(hybrid$gsp, newdata)

  ## hybrid forecasting
  forecast <- hybrid$gsp$forecast+hybrid$gmw$forecast

  return(forecast)
}
