#' simulateGAMsp
#'
#' simulates an additive noisy function to be fitted with gam
#'
#' @param n the length of y
#' @param p the number of additive functions (without the constant)
#' @param sigma sd of noise, s/n ratio = sd
#' @param a lower bound of the support of the additive components (the same for all)
#' @param b upper bound of the support of the additive components (the same for all)
#' @return a simulated object containing
#' \item{X}{the scaled design matrix}
#' \item{y}{the noisy signal}
#' \item{ftrue}{a matrix whose columns are the true additive components}
#' 
#' @examples
#'n<-200
#'p<-7
#'a<--2.5
#'b<-2.5
#'sigma<-1.5
#'out<-simulateGAMsp(n=n,p=p,a=a,b=b,sigma=sigma)
#'X<-out$X
#'G<-out$ftrue
#'y<-out$y
#'par(mfrow=c(3,3))
#'o<-order(X[,1])
#'plot(X[o,1],G[o,1],ylim=range((y-7)/3),type='l',col="red",lwd=2)
#'points(X[o,1],G[o,1]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,2])
#'plot(X[o,2],G[o,2],ylim=range((y-7)/3),type='l',col="red",lwd=2)
#'points(X[o,2],G[o,2]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,3])
#'plot(X[o,3],G[o,3],ylim=range((y-7)/1.5),type='l',col="red",lwd=2)
#'points(X[o,3],G[o,3]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,4])
#'plot(X[o,4],G[o,4],ylim=range((y-5)/2),type='l',col="red",lwd=2)
#'points(X[o,4],G[o,4]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,5])
#'plot(X[o,5],G[o,5],ylim=range((y-7)/3),type='l',col="red",lwd=2)
#'points(X[o,5],G[o,5]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,6])
#'plot(X[o,6],G[o,6],type='l',ylim=range((y-7)/3),col="red",lwd=2)
#'points(X[o,6],G[o,6]+sigma*rnorm(n),col="grey",pch=19)
#'o<-order(X[,7])
#'plot(X[o,7],G[o,7],type='l',ylim=range((y-7)/3),col="red",lwd=2)
#'points(X[o,7],G[o,7]+sigma*rnorm(n),col="grey",pch=19)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

simulateGAMsp<-function(n, p=7,a=-2.5,b=2.5,sigma=1.5){
	out<-list()
    X<-a+(b-a)*matrix(runif(n*p),nrow=n,ncol=p)
	ftrue<-matrix(0,nrow=n,ncol=p)
	ftrue<-cbind(-2*sin(2*X[,1]),X[,2]^2, 2*sin(X[,3])/(2 - sin(X[,3])), exp(-X[,4]), X[,5]^3+1.5*(X[,5]-1)^2, X[,6],3*sin(exp(-0.5*X[,7])))
	true_f<-apply(ftrue,1,sum)
    y<-true_f + rnorm(n, mean = 0, sd = sigma)
    X <- scaleData(X)
	out$X<-X
	out$ftrue<-ftrue
	out$y<-y
	return(out)
}
