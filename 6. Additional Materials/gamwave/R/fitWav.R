#' fitWaw
#'
#' fit a function by penalised wavelet regression
#'
#' @param x the observed design values
#' @param y the y observed values
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes)
#' @param range.x the min and max values of x over which the basis functions are computed  
#' @param numLevels number of levels for the wavelet decomposition
#' @param filterNumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family. It selects the smoothness of wavelet that you want to use in the decomposition.
#' @param resolution the highest resolution wavelets in the expansion (defaults to 16384)
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @param penalty type of penalty used for the penalised regression, could be "lasso", "SCAD" or "MCP"
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
#'fHatMCP<-fitWav(x,y,xnew=xnew,range.x=range.x,numLevels=numLevels,filterNumber=5,
#'                resolution=16384,family="DaubLeAsymm",penalty="MCP")$fHat
#'fHatSCAD<-fitWav(x,y,xnew=xnew,range.x=range.x,numLevels=numLevels,filterNumber=5,
#'                  resolution=16384,family="DaubExPhase",penalty="SCAD")$fHat
#'fHatLasso<-fitWav(x,y,xnew=xnew,range.x=range.x,numLevels=numLevels,filterNumber=5,
#'                  resolution=16384,family="DaubExPhase",penalty="lasso")$fHat

#'numIntKnots <- 25 
#'intKnots <- quantile(unique(x),seq(0,1,length=numIntKnots+2))[-c(1,numIntKnots+2)]
#'seqLambda<-seq(0.00001,0.01,length=100)
#'fHatsplines<-fitSplines(x,y,xnew=xnew,range.x,intKnots,drv=0,seqLambda=seqLambda)
#'
#' plot(x,y,xlim=range(xnew),ylim=range(fHatSCAD),bty="l",type="n",xlab="True(black);
#' MCP(red);SCAD(blue);lasso(yellow);splines(green);gam(mag)", ylab="y",main="Wavelet fit")
#'points(x,y,pch=19)
#'lines(x,f,col="black",lwd=2)
#'lines(xnew,fHatMCP,col='red',lwd=2)
#'lines(xnew,fHatSCAD,col='blue',lwd=2,lty=2)
#'lines(xnew,fHatLasso,col='grey',lwd=2,lty=2)
#'lines(xnew,fHatsplines$fHat,col='green',lwd=2)
#'lines(x,gam(y~s(x))$fitted.values,col="magenta",lwd=2)
#' 
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export




fitWav<-function(x,y,xnew=NULL,range.x=NULL,numLevels=6,filterNumber=5,
                 resolution=16384,family="DaubExPhase",penalty="MCP")
{
  if (missing(range.x) || is.null(range.x)) range.x<-range(x)
  if (missing(numLevels)) numLevels<-floor(log2(length(x)))-2
  Z <- WavD(x,numLevels=numLevels,filterNumber=filterNumber,family=family)
  
  if(penalty=="MCP")
    {
    cvfit<-ncvreg::cv.ncvreg(Z,y,penalty="MCP")
    fit<-cvfit$fit
    betaHat<-fit$beta[,cvfit$min]
    }
  
  if(penalty=="SCAD")
  {
      cvfit<-ncvreg::cv.ncvreg(Z,y,penalty="SCAD")
      fit<-cvfit$fit
      betaHat<-fit$beta[,cvfit$min]
  }
  
  if(penalty=="lasso")
  {
      cvfit<-ncvreg::cv.ncvreg(Z,y,penalty="lasso")
      fit<-cvfit$fit
      betaHat<-fit$beta[,cvfit$min]
  }
  
  
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


