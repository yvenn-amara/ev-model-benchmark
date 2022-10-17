#' fit.local
#'
#' Direct scatterplot hybrid gam_spline-wavelet smoothing handling the boundaries.
#'
#' @param x the x observed values (caution: x is scaled to always vary between 0 and 1).
#' @param y the y observed values
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes)
#' @param mnew size of the xnew (if xnew=NULL then mnew=100)
#' @param numlevels number of levels of the decomposition. If NULL numlevels=floor(log2(n))-2 (default NULL) 
#' @param penalty type of penalty used for the penalised regression, could be "lasso", "SCAD" or "MCP"
#' @param range.x the min and max values of x over which the basis functions are computed  
#' @param filterNumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family. It selects the smoothness of wavelet that you want to use in the decomposition.
#'
#' @return a list containing the resulting smoother evaluated at x and xnew 
#' \item{yhat}{a vector containing the estimated function on x, i.e. \eqn{\widehat{f}(x)}}
#' \item{betaHat}{a vector of estimated regression parameters}
#' \item{Cn}{the wavelet design matrix based on xnew}
#' \item{fit}{a vector containing the estimated function on xnew, i.e. \eqn{\widehat{f}(xnew)}}
#' \item{x}{the vector of x-values x}
#' \item{y} {the vector of observed values}
#' \item{xnew}{if xnew is null a grid of length mnew of equidistant values within the range of x; otherwise the given grid xnew}
#
#' @examples
#' # Obtain scatterplot data corresponding to environmental
#' # data from the R package `lattice'. Set up plotting 
#' # grid, knots and smoothing parameter:

#'require(lattice) ; attach(environmental) 
#'set.seed(1) 
#'x <- radiation ; y <- ozone^(1/3)
#'x<-(x-min(x))/(max(x)-min(x))
#'o<-sort(x)
#'fit<-fit.local(x,y,xnew=NULL, mnew=length(x), range.x=NULL, numlevels=2,filterNumber = 5,penalty="MCP")
#'fhatmgcv<-gam(y~s(x))$fitted.values
#' # Display the fit:
#' par(mfrow=c(1,1))
#' plot(x,y,xlim=range(x),bty="l", xlab="radiation",
#'   ylab="cuberoot of ozone",main="Local (boundary corrected) fit.")
#'lines(fit$x,fit$yhat,lwd=2,col="black")
#'lines(fit$xnew,fit$fit,lwd=2,col="blue") #plot on a different grid
#'lines(x[o],fhatmgcv[o],lwd=2,col="green")
#'detach(environmental)
#'
#' # A simulated example
#'n <- 400
#'set.seed(1)
#'x <- sort(runif(n))
#'numLevels <- floor(log2(n))-2
#'f<-fTrue(x); sd=sqrt(var(f)); snr=3.5; sigma=sd/snr;
#'y <- f + sigma*rnorm(n)
#'fit<-fit.local(x,y,xnew=NULL, mnew=length(x), range.x=NULL, numlevels=NULL,filterNumber = 5,penalty="MCP")
#'par(mfrow=c(1,1))
#' plot(x,y,xlim=range(x),ylim=range(y),bty="l",xlab="local fit in x (green); local fit in xnew (blue); true function in xnew (black)",
#' ylab="f(x)",main="Local (boundary corrected) fit")
#'lines(fit$xnew,fTrue(fit$xnew), col="black",lwd=2)
#'lines(fit$xnew,fit$fit,lwd=2,col="blue")
#'lines(fit$x,fit$yhat,lwd=2)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

fit.local <- function(x,y,xnew=NULL,range.x=NULL, mnew=length(x),numlevels=NULL,filterNumber = 5, family = "Coiflets", penalty="MCP"){
    require(mgcv)
    require(ncvreg)
    n<-length(y) # sample size
 	  # sorting explanatory variables (x's)
	#  aux <- sort(x,index.return=T)
	#  x<-aux$x
  #  yold<-y[aux$ix]
    yold<-y
    if (is.null(numlevels)) numlevels<-floor(log2(n))-2
    if (missing(range.x) || is.null(range.x)) range.x<-range(x)
    num.knots <- max(5,min(floor(length(unique(x))/4),35))
	  tmp <- gam(yold~s(x,bs="ps",k=num.knots))
    ###  Compute curve on a new grid if necessary ####
    Z <- WavD(x, range.x = range(x), numLevels = numlevels, filterNumber = filterNumber,family = family)
	  f1<-tmp$fitted.values
    resid<-yold-f1
    cvfit<-cv.ncvreg(Z,resid,penalty=penalty,nlambda=80,nfolds=10)
    fit<-cvfit$fit
    betauHat<-fit$beta[,cvfit$min+1]
    XX <- cbind(rep(1,length(y)))
    C <- cbind(XX,Z)
    g1 <- C%*%betauHat
    fhat <- f1+g1
    if(is.null(xnew)) {
     	  xl<-range(x)[1]
     	  xr<-range(x)[2]
		    xnew<-seq(xl, xr, length = mnew)
		    newd<-data.frame(x=xnew)
	      f1new<-predict(tmp,newdata=newd,type="response")
		    Znew <- WavD(xnew, range.x = range(xnew), numLevels = numlevels, filterNumber = filterNumber,family = family)
		    XXn <- cbind(rep(1,mnew))
        Cn <- cbind(XXn,Znew)
        g1new <- Cn%*%betauHat
        fhatnew<-as.vector(f1new)+as.vector(g1new)
	     }		  
    if(!is.null(xnew)) 
      {
          newd<-data.frame(x=xnew)
          f1new<-predict(tmp,newdata=newd,type="response")
		      Znew <- WavD(xnew, range.x = range(xnew), numLevels = numlevels, filterNumber = filterNumber,family = family)
		      XXn <- cbind(rep(1,mnew))
          Cn <- cbind(XXn,Znew)
          g1new <- Cn%*%betauHat
          fhatnew<-as.vector(f1new)+as.vector(g1new)
    }
    	
    return(list(yhat=fhat,fit=fhatnew,x=x,xnew=xnew, y=yold))
}