#' GAMsp
#'
#' Fits additive models with O'Sullivan splines and backfitting
#'
#' @param y the y observed values
#' @param A regressor matrix (without constants)
#' @param lambda a vector of penalisation parameters
#' @param numIntKnots number of internal knots
#' @param conv.thresh convergence stopping value for relative difference in backfitting iterations
#' @param max.iter maximum number of backfitting iteration

#' @return a gamwasp object containing
#' \item{alpha0}{a real number estimate of the mean}
#' \item{intKnots}{a vector of internal knots}
#' \item{betaHat}{a vector of estimated regression parameters}
#' \item{Shat}{the estimated non-linear effects}
#' \item{lambda}{the (CV optimised or not) penalised parameter}
#' \item{fitted}{fitted values of the target}
#' \item{type}{the type of based function used for the decomposition, "wavelet" or "splines"}
#' 
#' @examples
#' # A simple example with 2 smooth additive components
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
#'y <- g1+g2+rnorm(n)*.5 
#'
#'A<-cbind(x1,x2)
#'numIntKnots<-20
#'lambda.min<-10^(-7)
#'lambda.max<-1
#'nlambda<-50
#'seqLambda <- exp(seq(log(lambda.max), log(lambda.min * lambda.max), len = nlambda))
#'#seqLambda<-seq(10^(-7),10^(-2),length=nlambda)
#'gsp<-GAMsp(y,A,seqLambda,numIntKnots=numIntKnots)
#'#For comparisons with mgcv's gam
#' require(mgcv)
#' fitgam<-gam(y~s(x1)+s(x2))
#' plotdata<-plot(fitgam,pages=1)
#'par(mfrow=c(1,2))
#' # for compararison with mgcv's gam fit
# gfit<-gam(y~s(x1)+s(x2));plotdata<-plot(gfit,pages=1)
#'o<-order(x1)
#'plot(x1[o],g1[o],type='l',ylim=range(y-g2),lty='dashed',lwd=2)
#'points(x1[o],y[o]-g2[o],pch='.',cex=4)
#'lines(x1[o],gsp$Shat[o,1],col='purple')
#'lines(plotdata[1][[1]]$x,plotdata[1][[1]]$fit,col="green",lwd=2,lty=2)
#'o<-order(x2)
#'plot(x2[o],g2[o],type='l',ylim=range(y-g1),lty='dashed',lwd=2)
#'points(x2[o],y[o]-g1[o],pch='.',cex=4)
#'lines(x2[o],gsp$Shat[o,2],col='purple')
#'lines(plotdata[2][[1]]$x,plotdata[2][[1]]$fit,col="green",lwd=2,lty=2)
#'
#' # A second example with 7 smooth additive components
#'set.seed(1549)
#'n<-300
#'p<-7
#'a<--2.5
#'b<-2.5
#'sigma<-1.5 # snr≈4
#'out<-simulateGAMsp(n=n,p=p,a=a,b=b,sigma=sigma)
#'X<-out$X
#'G<-out$ftrue
#'y<-out$y
#'for (i in (1:p)) G[,i]<-G[,i]-mean(G[,i])
#'A<-X
#'numIntKnots<-20
#'seqLambda<- 10^(seq(-7, 3, by = 0.2))
#'gsp<-GAMsp(y,A,seqLambda,numIntKnots=numIntKnots)
#'par(mfrow=c(3,3))
#'o<-order(X[,1])
#'plot(X[o,1],G[o,1],type='l',col="black",lwd=2)
#'lines(X[o,1],gsp$Shat[o,1],col='purple',lwd=2,lty=2)
#'o<-order(X[,2])
#'plot(X[o,2],G[o,2],type='l',lwd=2)
#'lines(X[o,2],gsp$Shat[o,2],col='purple',lwd=2,lty=2)
#'o<-order(X[,3])
#'plot(X[o,3],G[o,3],type='l',lwd=2)
#'lines(X[o,3],gsp$Shat[o,3],col='purple',lwd=2,lty=2)
#'o<-order(X[,4])
#'plot(X[o,4],G[o,4],type='l',lwd=2)
#'lines(X[o,4],gsp$Shat[o,4],col='purple',lwd=2,lty=2)
#'o<-order(X[,5])
#'plot(X[o,5],G[o,5],type='l',lwd=2)
#'lines(X[o,5],gsp$Shat[o,5],col='purple',lwd=2,lty=2)
#'o<-order(X[,6])
#'plot(X[o,6],G[o,6],type='l',lwd=2)
#'lines(X[o,6],gsp$Shat[o,6],col='purple',lwd=2,lty=2)
#'o<-order(X[,7])
#'plot(X[o,7],G[o,7],type='l',lwd=2)
#'lines(X[o,7],gsp$Shat[o,7],col='purple',lwd=2,lty=2)
#'
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export



GAMsp<- function(y,A,lambda,numIntKnots,conv.thresh=1.e-10, 
                  max.iter=ncol(A)*10){
  ## Fits additive models using O'Sullivan splines and backfitting
  n=length(y)
  p=ncol(A)
  intKnots<-matrix(0,nrow=numIntKnots,ncol=p)
  continue=T
  alpha0<-mean(y)
  Shat<-matrix(0,nrow=nrow(A),ncol=ncol(A))
  betaHat<-matrix(0,nrow=numIntKnots+4, ncol=ncol(A))
  res=y-alpha0
  iter=0
  lambda<-rep(0,ncol(A))
  
  while(continue&(iter<max.iter)){
    Shat0=Shat
    
    for(pp in 1:p){
      resp<-res+Shat[,pp]  
      x<-(A[,pp]-min(A[,pp]))/(max(A[,pp])-min(A[,pp]))
      X<-cbind(rep(1,n),x) 
      intKnots[,pp] <- quantile(unique(x),
                                seq(0,1,length=numIntKnots+2))[-c(1,numIntKnots+2)]
      range.x <- c(0,1)
      if(length(seqLambda)>1)
	  fHatsplines<-fitSplines(x=x,y=resp,range.x=range.x,intKnots=intKnots[,pp],drv=0,seqLambda=seqLambda)
      if(length(seqLambda)==1) fHatsplines<-fitSplines(x=x,y=resp,range.x=range.x,intKnots=intKnots[,pp],drv=0,lambda=seqLambda)
      
      betaHat[,pp]<-fHatsplines$betaHat
      Shatp<- fHatsplines$fHat
      res=resp-Shatp
      Shat[,pp]=Shatp
      lambda[pp]<-fHatsplines$lambda
    }

    alpha0new=mean(res+alpha0)
    res=res+alpha0-alpha0new
    alpha0=alpha0new
    continue=((mean(abs(Shat-Shat0))/mean(abs(Shat)))>conv.thresh)
    iter=iter+1
  }
  out=NULL
  out$intKnots=intKnots
  out$betaHat=betaHat
  out$Shat=Shat
  out$alpha0=alpha0
  out$fitted<-alpha0+apply(Shat,1,sum)
  out$lambda<-lambda
  out$type<-"splines"
  class(out)<-"gamwasp"
  return(out)
}



