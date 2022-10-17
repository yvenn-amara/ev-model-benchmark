#' PosefitHybrid
#'
#' Direct scatterplot hybrid gam-wavelet smoothing handling the boundaries
#' The wavelet estimation is performed with Gamma-Lasso regression (package gamlr)
#'
#' @param x the x observed values (caution: x is scaled to always vary between 0 and 1).
#' @param y the y observed values
#' @param eps convergence tolerance parameter (default: eps=0.001)
#' @param numlevels number of levels of the decomposition. If NULL numlevels=floor(log2(n))-2 (default NULL) 
#' @param maxiter maximum number of iterations (Defaults to 30).
#' @param gamma penalty concavity tuning parameter; see package(gamlr). Zero (default) yields the lasso, and higher values correspond to a more concave penalty. 
#'
#' @return fit the resulting smoother evaluated at x 

#' @examples
#' # Obtain scatterplot data corresponding to environmental
#' # data from the R package `lattice'. Set up plotting 
#' # grid, knots and smoothing parameter:

#'require(lattice) ; attach(environmental) 
#'set.seed(1) 
#'x <- radiation ; y <- ozone^(1/3)
#'x<-(x-min(x))/(max(x)-min(x))
#'fhat<-PosefitHybrid(x,y,eps=0.001,maxiter=50,gamma=4) 
#'fhatmgcv<-gam(y~s(x))$fitted.values
#' # Display the fit:
#' par(mfrow=c(1,1))
#' plot(x,y,xlim=range(x),bty="l",type="n",xlab="radiation",
#'   ylab="cuberoot of ozone",main="Hybrid (boundary corrected) fit.")
#' aux<-sort(x,index.return=TRUE); o<-aux$ix
#'lines(x[o],fhat$fit[o],col='blue',lwd=2)
#'points(x,y,lwd=1)
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
#' fhat<-PosefitHybrid(x,y,eps=0.001,maxiter=50,numlevels=numLevels,gamma=4)
#'#fhatMCP<-fitHybrid(x,y,eps=0.001,maxiter=50,numlevels=numLevels,penalty="MCP")
#'#fhatLasso<-fitHybrid(x,y,eps=0.001,maxiter=50,numlevels=numLevels,penalty="lasso")
#' fhatmgcv<-gam(y~s(x))$fitted.values
#'par(mfrow=c(1,1))
#' plot(x,y,xlim=range(x),ylim=range(fhatmgcv),bty="l",type="n",xlab="mgcv (green); Hybrid(blue); true (red)",
#'  ylab="f(x)",main="Hybrid (boundary corrected) fit")
#' aux<-sort(x,index.return=TRUE); o<-aux$ix
#'lines(x[o],fhatSCAD$fit[o],col='blue',lwd=2)
#'#lines(x[o],fhatMCP$fit[o],col='blue',lty=3,lwd=2)
#'#lines(x[o],fhatLasso$fit[o],col='blue',lty=2,lwd=2)
#'points(x,y,pch=16,cex=0.4)
#'lines(x[o],fhatmgcv[o],lwd=2,col="green")
#'lines(x,f,col="red",lty=2,lwd=2)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

PosefitHybrid <- function(x,y,eps=0.001,maxiter=30,numlevels=NULL,gamma=8){
		 require(mgcv)
		 require(gamwave)
		 require(gamlr)
	     tol <- 1
		 range.x <- c(0,1)
		 if (is.null(numlevels)) numlevels<-floor(log2(length(x)))-2
	     yold <- y
		 fhatold<-gam(y~s(x))$fitted.values
	     rold <- as.vector(yold-fhatold)
	     iter <- 0
	     while((tol > eps)& (iter < maxiter)){
		 ff1<-gam(yold-rold~s(x))
		 A1<-model.matrix(ff1)
		 f1<-ff1$fitted.values
	     resid <- yold-f1
		 Z <- WavD(x, range.x = range(x), numLevels = numlevels, filterNumber = 5,family = "Coiflets")
		fit <- gamlr(Z, resid, gamma=gamma, lambda.min.ratio=1e-3)
		ind<-which(AICc(fit)==min(AICc(fit)))
		betauHat<-c(fit$alpha[ind],fit$beta[,ind])
		 X <- cbind(rep(1,length(x)))
		 C <- cbind(X,Z)
	     g1 <- C%*%betauHat
	     fhatnew <- f1+g1
	     tol <- mean(abs(fhatnew-fhatold))
	     rold <- g1
	     yold <- y
	     fhatold <- fhatnew
#	     cat(iter, "- tol:", tol, "\n")
	     iter <- iter+1
	     }
return(list(fit=fhatnew,beta=betauHat))
}