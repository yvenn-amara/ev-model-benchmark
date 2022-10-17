#' NewGAMwv.fit
#'
#' Fit a generalized additive model with wavelet function components and compute the design matrix automatically
#'
#' @param formula a wavelet formula
#' @param ngrid the size of the regular dyadic over which to evaluate the fitted model for CI (when \code{local=F})
#' @param local logical parameter: TRUE if hybrid wavelets are used ; FALSE if not (default = FALSE)
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
#' gmw <- NewGAMwv(y, A,numlev= numlev, local=T, penalty="MCP", offset=offset)
#' gmw <- NewGAMwv.fit(y ~ w(x1) + w(x2), penalty="MCP", local=T, offset=offset, data=cbind(y,A))
#'
#' # TO DO : test that it's only w and no s inside
#'
#' @export
#'
NewGAMwv.fit <- function(formula, numlev=floor(log2(nrow(data))-2), local=F, penalty="SCAD", filternum=5, family="DaubLeAsymm", resolution=16384,
                      ngrid=256, conv.thresh=1.e-10, offset=0, max.iter = ncol(data)*5, trace=F, nfolds = 10, data){

  ## formula
  formula <- as.formula(formula)

  ## response
  y_wavelet <- data[, interpret.formula(formula)$response]

  ## explanatory dataset creation
  datawavelet <- data.frame(wavelet.var(formula, data))

  ## estimation of the model using gamwave
  gmw <- NewGAMwv(y=y_wavelet, datawavelet, numlev, local, penalty, filternum, family, resolution, ngrid,
               conv.thresh, offset, max.iter, trace, nfolds)
  
  return(gmw)
}
