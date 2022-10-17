#' fitPSplines
#'
#' Direct scatterplot semi-parametric regression smoothing with Eilers P-splines
#' @param x the abcissae
#' @param y the response
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes)  
#' @param nseg the number interior knots for the B-splines basis functions (all splines are of order 4 (cubic (deg=3)))
#' @param pord an integer with values  1 or 2. Order of difference penalty (Defaults to 2).
#' @param seqLambda is a vector of smoothing parameters. if seqLambda is not provided  a default grid is used. 

#'
#' @return a list containing
#' 
#' \item{fHat}{an array containing \eqn{\widehat{f}(x)}}
#' \item{betaHat}{an array containing the estimated P-splines regression coefficients}
#' \item{lambda}{the (CV optimised or not) penalised parameter}
#' \item{errCV}{the CV errors corresponding to the seqLambda}
#' \item{lseqLambda}{the log10(seqLambda)}
#' @examples
#' # Obtain scatterplot data corresponding to environmental
#' # data from the R package `lattice'. Set up plotting 
#' # grid, knots and smoothing parameter:

#'require(lattice) ; attach(environmental) 
#'x <- radiation ; y <- ozone^(1/3)
#'range.x=range(x)
#'ng <- 200
#'xnew <- seq(range.x[1],range.x[2],length=ng) 
#'nseg <- 15
#'seqLambda<-10^seq(-1,5,by=0.2)
#'fit<-fitPSplines(x,y,xnew=xnew,nseg=nseg,seqLambda=seqLambda)
#' # Display the fit:
#' par(mfrow=c(1,2))
#' plot(x,y,xlim=range(xnew),bty="l",type="n",xlab="radiation",
#' ylab="cuberoot of ozone",main="fit with gcv",col.main="green4")
#'lines(xnew,fit$fHat,col='green',lwd=2)
#'points(x,y,pch=19,cex=0.5,col="grey")
#'plot(fit$lseqLambda, fit$errCV, xlab = 'log10(lambda)', ylab = 'CV',type="l")
#'points(fit$lseqLambda[which.min(fit$errCV)], fit$errCV[which.min(fit$errCV)],pch=19)
#'detach(environmental)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

fitPSplines<-function(x,y,xnew=NULL,nseg=10,pord=2,seqLambda=NULL)
  {
    l<-list()
	#   Using cross-validation to optimize smooth
    #   Using cross-validation to optimize smooth
    if(!is.null(seqLambda))
    { lla<-seqLambda
      pn<-lapply(lla,function(u){fitPsplineNormal(x, y, xnew=NULL, nseg = nseg, pord = pord, lambda = u, plot =F, se = 0)})
	  errCV<-NULL
      for (k in 1:length(lla)) errCV<-c(errCV,pn[[k]]$cv)
      l$errCV<-errCV
      lambda<-lla[which.min(l$errCV)]
    }
    if (is.null(seqLambda)){
	 lla<-10^seq(-1, 4, by = 0.2)
     pn<-lapply(lla,function(u){fitPsplineNormal(x, y, xnew=NULL, nseg = nseg, pord = pord, lambda = u, plot =F, se = 0)})
   	 errCV<-NULL
      for (k in 1:length(lla)) errCV<-c(errCV,pn[[k]]$cv)
      l$errCV<-errCV
      lambda<-lla[which.min(l$errCV)]
     }
 
	optfit<- fitPsplineNormal(x, y, xnew=NULL, nseg = nseg, pord = pord, lambda = lambda, plot =F, se = 0)  
    l$betaHat <- optfit$beta
    l$fHat <- optfit$fHat
    if(!is.null(xnew)) l$fHat <- fitPsplineNormal(x, y, xnew=xnew, nseg = nseg, pord = pord, lambda = lambda, plot =F, se = 0)$munew 
    l$lambda<-lambda
    l$lseqLambda<-log10(lla)
    return(l)
  }
  
  
  

