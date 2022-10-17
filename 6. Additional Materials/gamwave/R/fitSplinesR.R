#' fitSplines
#'
#' fit a function by penalised wavelet regression
#'
#' @param x the x observed values
#' @param y the y observed values
#' @param xnew the x value over which calculate \eqn{\widehat{f}(x)} 
#' @param range.x the min and max value over which calculated the basis
#' @param intKnots 
#' @param drv ...
#' @param lambda the penalisation parameter of the ridge regression
#'
#' @return a list containing
#' 
#' \item{fHat}{an array containing \eqn{\widehat{f}(x)}}
#' \item{betaHat}{an array containing the estimated regression coefficients}
#' \item{lambda}{the (CV optimised or not) penalised parameter}
#' \item{errCV}{the CV error corresponding to the seqLambda}

#' @examples
#'n <- 200 
#'set.seed(1) 
#'x <- sort(runif(n)) 
#'ng <- 1001
#'xnew <- seq(0,1,length=ng)
#'numLevels <- 6
#'range.x <- c(0,1)
#'f<-fTrue(x)
#'y <- f + rnorm(n) 
#'fHatMCP<-fitWav(x,y,xnew=xnew,range.x=range.x,numLevels=numLevels,filterNumber=5,
#'                resolution=16384,family="DaubExPhase",penalty="MCP")
#'fHatscoop<-fitWav(x,y,xnew=xnew,range.x=range.x,numLevels=6,filterNumber=5,
#'                  resolution=16384,family="DaubExPhase",penalty="scoop")
#'numIntKnots <- 25 
#'intKnots <- quantile(unique(x),seq(0,1,length=numIntKnots+2))[-c(1,numIntKnots+2)]
#'seqLambda<-seq(10^-7,10^-6,length=100)
#'fHatsplines<-fitSplines(x,y,xnew=xnew,range.x,intKnots,drv=0,seqLambda=seqLambda)
#'
#'plot(x,y,pch='.',cex=4)
#'lines(x,f)
#'lines(xnew,fHatMCP,col='red')
#'lines(xnew,fHatscoop,col='blue')
#'lines(xnew,fHatsplines$fHat,col='green')
#' 
#'plot(fHatsplines$errCV,type='b',pch=20)
#' 
#' @author Anestis Antoniadis <anestisa@gmail.com>
#' @author Yannig  Goude <yannig.goude@edf.fr>
#' @export


  
fitSplines<-function(x,y,xnew=NULL,range.x,intKnots,drv=0,lambda=NULL,seqLambda=NULL)
  {
    l<-list()
    Z<- ZOSull(x,range.x=range.x,intKnots=intKnots) 
    
    X <- cbind(rep(1,length(x)),x)
    Cmat <- cbind(X,Z)
    
    
    CTC <- crossprod(Cmat) 
    CTy <- crossprod(Cmat,y)
    Dmat <- diag(c(0,0,rep(1,ncol(Z))))
    
    
    if(!is.null(seqLambda))
    {
      HRidge<-lapply(seqLambda,function(x){crossprod(t(Cmat),solve(CTC+x*Dmat))%*%t(Cmat)})
      errCV<-lapply(HRidge,function(x){mean((y-x%*%y)^2/(1-diag(x))^2)})
      l$errCV<-unlist(errCV)
      lambda<-seqLambda[which.min(l$errCV)]
      
    }
    
    l$betaHat <- solve(CTC+lambda*Dmat,CTy)
    
    if(!is.null(xnew)) 
      {
        Z<- ZOSull(xnew,range.x=range.x,intKnots=intKnots) 
        X <- cbind(rep(1,length(xnew)),xnew)
        Cmat <- cbind(X,Z)
    }
    
    l$lambda<-lambda
    l$fHat<- Cmat%*%l$betaHat

    return(l)
  }
  
  
  

