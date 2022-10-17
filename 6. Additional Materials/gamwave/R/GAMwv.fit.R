#' GAMwv.fit
#'
#' Fit a generalized additive model with wavelet function components and compute the design matrix automatically
#'
#' @param formula a wavelet formula
#' @param xnew the x value vector over which to evaluate the fitted model
#' @param Hybrid logical parameter: TRUE if hybrid wavelets are used ; FALSE if not (default = FALSE)
#' @param conv.thresh convergence stopping value for relative difference in backfitting iterations
#' @param max.iter maximum number of backfitting iteration
#' @param data dataset
#' @import stackeR wavethresh
#'
#' @return a generalized additive model with wavelet based components
#'
#' @examples
#' # An example of smooth functions (to be compared with splines)
#' f1 <- function(x) return(3*sin(2*pi*x^3))
#' f2 <- function(x) return(15* x *exp(-x**2))
#' n <- 200
#' set.seed(1)
#' x1 <- runif(n)
#' x1 <- (x1-min(x1))/(max(x1)-min(x1))
#' x2 <- runif(n)
#' x2 <- (x2-min(x2))/(max(x2)-min(x2))
#' g1 <- f1(x1)-mean(f1(x1))
#' g2 <- f2(x2)-mean(f2(x2))
#' y <- g1+g2+ rnorm(n)*.5
#' A <- cbind(x1,x2)
#' numlev = floor(log2(n))-1
#' offset = -1 # fit with standard wavelets or with hybrid wavelets (uncomment the appropriate lines)
#' gmw <- GAMwv(y, A,numlev=(numlev-1), penalty="SCAD", offset=offset)
#' gmw <- GAMwv(y, A,numlev=numlev, penalty="SCAD") # for comparing with mgcv's gam
#' gmw <- GAMwv.fit(y ~ w(x1) + w(x2), penalty="SCAD", offset=offset, data=cbind(y,A))
#' gmw <- GAMwv.fit(y ~ w(x1) + w(x2), numlev=numlev, penalty="SCAD", Hybrid=F, data=cbind(y,A))# for comparing with mgcv's gam
#'
#' # TO DO : test that it's only w and no s inside
#'
#' @export
#'
GAMwv.fit <- function(formula, numlev=floor(log2(nrow(data))-2), Hybrid=F, penalty="SCAD", filternum=5, family="DaubLeAsymm", resolution=16384,
                      conv.thresh=1.e-10, offset=0, max.iter = ncol(data)*5, trace=F, nfolds = 10, data){

  ## formula
  formula <- as.formula(formula)

  ## response
  y_wavelet <- data[, interpret.formula(formula)$response]

  ## explanatory dataset creation
  datawavelet <- data.frame(wavelet.var(formula, data))

  ## estimation of the model using gamwave
  gmw <- GAMwv(y=y_wavelet, datawavelet, numlev, Hybrid, penalty, filternum, family, resolution,
               conv.thresh, offset, max.iter, trace, nfolds)
  Z<-gmw$Z # added by anestis 18/10/2017
  gmw$formula <- formula
  gmw$data <- data
  gmw$response <- interpret.formula(formula)$response
  gmw$Z <- Z

  return(gmw)
}
