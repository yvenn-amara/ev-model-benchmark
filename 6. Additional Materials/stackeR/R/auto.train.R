#' auto.train
#'
#' automatic training of a model with error handling
#'
#' @param x the estimation dataset, a matrix X of covariate observations used for the estimation step
#' @param y a vecteur Y of the target observation used for the estimation step
#' @param model a character string, name of the model to compute
#' @param trControl control parameter for the training of the models, see \code{\link{caret:train}}
#' 
#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' rmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'
#'
auto.train<-function(x,y,model,trControl)
{
  param<-list()
  param$trControl<-trControl
  param$x<-x
  param$y<-y
  param$method<-model
  capture.output(mod<-tryCatch({do.call(caret::train,param)}, error = function(e){NULL}))
  print(model)
  return(mod)
}