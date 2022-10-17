#' PoseGAMwv
#'
#' fit a wavelet based additive model with backfitting with 
#' Gamma-Lasso regression(package gamlr)
#'
#' @param y the y observed values
#' @param A regressor matrix (without constants)
#' @param xnew the x value over which to evaluate the fitted model
#' @param Hybrid logical parameter: TRUE if hybrid wavelets are used ; FALSE if not (default = FALSE)
#' @param conv.thresh convergence stopping value for relative difference in backfitting iterations
#' @param max.iter maximum number of backfitting iteration
#' @param numlev number of levels for the wavelet decomposition
#' @param filternum selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family.
#' It selects the smoothness of wavelet that you want to use in the decomposition
#' @param resolution the highest resolution wavelets in the expansion
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @param penalty type of penalty used for the penalised regression, could be "MCP" or "SCAD" or "lasso"
#' @param offset integer offset for the estimated penalty parameter (fine tunning)
#'
#' @return a gamwasp object containing
#' 
#' \item{alpha0}{a real number estimate of the mean}
#' \item{betaHat}{a vector of estimated regression parameters}
#' \item{Shat}{the estimated non-linear effects}
#' \item{type}{the type of basis functions used for the decomposition, "wavelet" or "splines"}
##'
#' @examples
#' # An example of smooth functions (to be compared with splines)
#'f1 <- function(x) return(3*sin(2*pi*x^3))
#'f2 <- function(x) return(15* x *exp(-x**2))
#'
#'n <- 200
#'set.seed(1)
#'x1 <- runif(n)
#'x1<-(x1-min(x1))/(max(x1)-min(x1))
#'x2 <- runif(n)
#'x2<-(x2-min(x2))/(max(x2)-min(x2))
#'g1<-f1(x1)-mean(f1(x1)) 
#'g2<-f2(x2)-mean(f2(x2))
#'
#'y <- g1+g2+ rnorm(n)*.5 
#'
#'A<-cbind(x1,x2)
#'numlev=floor(log2(n))-1
#'offset=-1
#' # fit with standard wavelets or with hybrid wavelets (uncomment the appropriate lines)
#'#gmw<-GAMwv(y,A,numlev=(numlev-1),penalty="SCAD",offset=offset)
#'gmw<-GAMwv(y,A,numlev=numlev,penalty="SCAD",Hybrid=TRUE)
#' # for comparing with mgcv's gam
#' gfit<-gam(y~s(x1)+s(x2)); plotdata<-plot(gfit,pages=1)
#' par(mfrow=c(1,2))
#' o<-order(x1)
#' plot(x1[o],y[o]-g2[o],type="n",xlab="True (dashed); gam (green), wavelet (purple)",ylab="y-g2(.)")
#' lines(x1[o],g1[o],lty='dashed',lwd=2)
#' points(x1[o],y[o]-g2[o],pch=19,cex=.5)
#' lines(x1[o],gmw$Shat[o,1],col='purple',lwd=2)
#'lines(plotdata[1][[1]]$x,plotdata[1][[1]]$fit,lty=2,lwd=2,col="green")
#' o<-order(x2)
#' plot(x2[o],y[o]-g1[o],type="n",xlab="True (dashed); gam (green), wavelet (purple)",ylab="y-g1(.)")
#' lines(x2[o],g2[o],lty='dashed',lwd=2)
#' points(x2[o],y[o]-g1[o],pch=19,cex=.5)
#' lines(x2[o],gmw$Shat[o,2],col='purple',lwd=2)
#' lines(plotdata[2][[1]]$x,plotdata[2][[1]]$fit,lty=2,lwd=2,col="green")
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export




PoseGAMwv <- function(y,A,numlev=6,Hybrid=FALSE, gamma=gamma,filternum=5,family="DaubLeAsymm",resolution=16384, conv.thresh=1.e-10, offset=0, max.iter=ncol(A)*5)
{
  n=length(y)
  p=ncol(A)
  continue=T
  alpha0<-mean(y)
  Shat<-matrix(0,nrow=nrow(A),ncol=ncol(A))
  betaHat<-matrix(0,nrow=2^(numlev), ncol=ncol(A))
  res=y-alpha0
  iter=0
  while(continue&(iter<max.iter)){
    iter=iter+1
    Shat0=Shat
    for(pp in 1:p){
      resp<-res+Shat[,pp]  
      x<-(A[,pp]-min(A[,pp]))/(max(A[,pp])-min(A[,pp]))
      range.x <- c(0,1)
      X<-rep(1,length(x))
	  if (!Hybrid){
		 Z <- WavD(x,range.x=range.x,numLevels=numlev,family=family,filterNumber=filternum)
 		fit <- gamlr(Z, resp, gamma=gamma, lambda.min.ratio=1e-3)
 		ind<-which(AICc(fit)==min(AICc(fit)))
 		betaHat[,pp]<-c(fit$alpha[ind+offset],fit$beta[,ind+offset])
       }	 
	  if (Hybrid) {
		  fit<- PosefitHybrid(x,y,eps=0.001,maxiter=30,gamma=gamma,numlevels=numlev)
		  Shatp<-fit$fit
		  betaHat[,pp]<-fit$beta
	  }
	  if (!Hybrid){ 
      Cmat <- cbind(X,Z)
      Shatp<- Cmat%*%betaHat[,pp]
      }
      res=resp-Shatp
      Shat[,pp]=Shatp
   }
    alpha0new=mean(res+alpha0)
    res=res+alpha0-alpha0new
    alpha0=alpha0new
    continue=((mean(abs(Shat-Shat0))/mean(abs(Shat)))>
                conv.thresh)
  }
  out=NULL
  out$alpha0=alpha0
  out$betaHat=betaHat
  out$Shat=Shat
  out$type<-"wavelet"
  out$filternum<-filternum
  out$family<-family
  out$numlev<-numlev
  out$fitted<-alpha0+apply(Shat,1,sum)
  class(out)<-"gamwasp"
  return(out)
}