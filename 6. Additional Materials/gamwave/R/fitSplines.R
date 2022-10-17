#' fitSplines
#'
#' Direct scatterplot semi-parametric regression smoothing with O'Sullivan penalized splines
#'
#' @param x the x observed values
#' @param y the y observed values
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes)  
#' @param range.x the min and max values of x over which the basis functions are evaluated
#' (x is allowed to take values outside the interior knots).
#' @param intKnots the interior knots for the B-splines basis functions (all splines are of order 4 (cubic))
#' @param drv an integer with values between 0 and 3. The derivative of the given order \code{drv} is evaluated at the x positions. Defaults to zero.
#' @param lambda is the spline smoothing parameter (set to NULL if it is find by GCV within the vector seqLambda)
#' @param seqLambda is a vector of smoothing parameters which must be provided if lambda is not provided. 

#'
#' @return a list containing
#' 
#' \item{fHat}{an array containing \eqn{\widehat{f}(x)}}
#' \item{betaHat}{an array containing the estimated regression coefficients}
#' \item{lambda}{the (CV optimised or not) penalised parameter}
#' \item{errCV}{the CV error corresponding to the seqLambda}

#' @examples
#' # Obtain scatterplot data corresponding to environmental
#' # data from the R package `lattice'. Set up plotting 
#' # grid, knots and smoothing parameter:

#'require(lattice) ; attach(environmental) 
#'set.seed(1) 
#'x <- radiation ; y <- ozone^(1/3)
#'range.x=range(x)
#'ng <- 101
#' a <- 0 ; b <- 350 ; xnew <- seq(a,b,length=101) 
#'numIntKnots <- 20
#'intKnots <- quantile(unique(x),seq(0,1,length=numIntKnots+2))[-c(1,numIntKnots+2)]
#'names(intKnots) <- NULL
#'seqLambda<-seq(100,100000,length=100)
#'fHatsplines<-fitSplines(x,y,xnew=xnew,range.x,intKnots,drv=0,seqLambda=seqLambda)
#' # Display the fit:
#' par(mfrow=c(1,2))
#' plot(x,y,xlim=range(xnew),bty="l",type="n",xlab="radiation",
#'   ylab="cuberoot of ozone",main="(a) direct fit; gcv 
#'     choice of smooth. par.")
#'lines(xnew,fHatsplines$fHat,col='green',lwd=2)
#'points(x,y,lwd=1)
#'plot(fHatsplines$errCV,type='b',pch=20)
#'detach(environmental)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

fitSplines<-function(x,y,xnew=NULL,range.x,intKnots,drv=0,lambda=NULL,seqLambda=NULL)
  {
    l<-list()
    Z<- SplineD(x,range.x=range.x,intKnots=intKnots) 
    
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
        Z<- SplineD(xnew,range.x=range(xnew),intKnots=intKnots) 
        X <- cbind(rep(1,length(xnew)),xnew)
        Cmat <- cbind(X,Z)
    }
    
    l$lambda<-lambda
    l$fHat<- Cmat%*%l$betaHat

    return(l)
  }
  
  
  

