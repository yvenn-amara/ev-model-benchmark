#' PosefitWaw
#'
#' fit a function by penalised wavelet regression
#' using a gamma Lasso penalisation (package gamlr)
#'
#' @param x the observed design values
#' @param y the y observed values
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes)
#' @param range.x the min and max values of x over which the basis functions are computed  
#' @param numLevels number of levels for the wavelet decomposition
#' @param filterNumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family. It selects the smoothness of wavelet that you want to use in the decomposition.
#' @param resolution the highest resolution wavelets in the expansion (defaults to 16384)
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @param gamma penalty for gamma lasso 
#'
#'
#' @return a list containing
#' 
#' \item{fHat}{a  vector containing \eqn{\widehat{f}(x)}}
#' \item{beta}{a vctor containing the estimated wavelet regression coefficients}
#'
#' @examples
#' require(mgcv)
#'n <- 300 
#'set.seed(1) 
#'x <- sort(runif(n)) 
#'ng <- 1001
#'xnew <- seq(0,1,length=ng)
#'numLevels <- floor(log2(n))-2
#'range.x <- c(0,1)
#'f<-fTrue(x); sd=sqrt(var(f)); snr=4; sigma=sd/snr;
#'y <- f + rnorm(n) 
#'fhat<-PosefitWav(x,y,xnew=xnew,range.x=range.x,numLevels=numLevels,filterNumber=5, resolution=16384,family="DaubLeAsymm",gamma=4)$fHat
#'
#' plot(x,y,xlim=range(xnew),ylim=range(fhat),bty="l",type="n",xlab="True(black);
#' wavelet fit (red); gam(mag)", ylab="y",main="Wavelet fit")
#'points(x,y,pch=19)
#'lines(x,f,col="black",lwd=2)
#'lines(xnew,fhat,col='red',lwd=2)
#'lines(x,gam(y~s(x))$fitted.values,col="magenta",lwd=2)
#' 
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export




PosefitWav<-function(x,y,xnew=NULL,range.x=NULL,numLevels=6,filterNumber=5,
                 resolution=16384,family="DaubExPhase",gamma=4)
{
  if (missing(range.x) || is.null(range.x)) range.x<-range(x)
  if (missing(numLevels)) numLevels<-floor(log2(length(x)))-2
  Z <- WavD(x,numLevels=numLevels,filterNumber=filterNumber,family=family)
  require(gamlr)
  
  fit <- gamlr(Z, y, gamma=gamma, lambda.min.ratio=1e-3)
  ind<-which(AICc(fit)==min(AICc(fit)))
  betaHat<-c(fit$alpha[ind],fit$beta[,ind])
  
  
  if(!is.null(xnew)) Z <- WavD(xnew,numLevels=numLevels,filterNumber=filterNumber,family=family) 
  
  if(!is.null(xnew)) {
  	Xg <- cbind(rep(1,length(xnew)))
  } else {
  Xg <- cbind(rep(1,length(x)))
  }
  Cg <- cbind(Xg,Z)
  
  l<-list()

  l$fHat<- Cg%*%betaHat
  l$beta<-betaHat
  return(l)
}


