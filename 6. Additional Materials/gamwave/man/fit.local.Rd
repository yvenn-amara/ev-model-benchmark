% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.local.R
\name{fit.local}
\alias{fit.local}
\title{fit.local}
\usage{
fit.local(x, y, xnew = NULL, range.x = NULL, mnew = length(x),
  numlevels = NULL, filterNumber = 5, family = "Coiflets",
  penalty = "MCP")
}
\arguments{
\item{x}{the x observed values (caution: x is scaled to always vary between 0 and 1).}

\item{y}{the y observed values}

\item{xnew}{a new xgrid  over which we evaluate the fitted function (for display purposes)}

\item{range.x}{the min and max values of x over which the basis functions are computed}

\item{mnew}{size of the xnew (if xnew=NULL then mnew=100)}

\item{numlevels}{number of levels of the decomposition. If NULL numlevels=floor(log2(n))-2 (default NULL)}

\item{filterNumber}{selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family. It selects the smoothness of wavelet that you want to use in the decomposition.}

\item{penalty}{type of penalty used for the penalised regression, could be "lasso", "SCAD" or "MCP"}
}
\value{
a list containing the resulting smoother evaluated at x and xnew 
\item{yhat}{a vector containing the estimated function on x, i.e. \eqn{\widehat{f}(x)}}
\item{betaHat}{a vector of estimated regression parameters}
\item{Cn}{the wavelet design matrix based on xnew}
\item{fit}{a vector containing the estimated function on xnew, i.e. \eqn{\widehat{f}(xnew)}}
\item{x}{the vector of x-values x}
\item{y} {the vector of observed values}
\item{xnew}{if xnew is null a grid of length mnew of equidistant values within the range of x; otherwise the given grid xnew}
}
\description{
Direct scatterplot hybrid gam_spline-wavelet smoothing handling the boundaries.
}
\examples{
# Obtain scatterplot data corresponding to environmental
# data from the R package `lattice'. Set up plotting 
# grid, knots and smoothing parameter:
require(lattice) ; attach(environmental) 
set.seed(1) 
x <- radiation ; y <- ozone^(1/3)
x<-(x-min(x))/(max(x)-min(x))
o<-sort(x)
fit<-fit.local(x,y,xnew=NULL, mnew=length(x), range.x=NULL, numlevels=2,filterNumber = 5,penalty="MCP")
fhatmgcv<-gam(y~s(x))$fitted.values
# Display the fit:
par(mfrow=c(1,1))
plot(x,y,xlim=range(x),bty="l", xlab="radiation",
  ylab="cuberoot of ozone",main="Local (boundary corrected) fit.")
lines(fit$x,fit$yhat,lwd=2,col="black")
lines(fit$xnew,fit$fit,lwd=2,col="blue") #plot on a different grid
lines(x[o],fhatmgcv[o],lwd=2,col="green")
detach(environmental)

# A simulated example
n <- 400
set.seed(1)
x <- sort(runif(n))
numLevels <- floor(log2(n))-2
f<-fTrue(x); sd=sqrt(var(f)); snr=3.5; sigma=sd/snr;
y <- f + sigma*rnorm(n)
fit<-fit.local(x,y,xnew=NULL, mnew=length(x), range.x=NULL, numlevels=NULL,filterNumber = 5,penalty="MCP")
par(mfrow=c(1,1))
plot(x,y,xlim=range(x),ylim=range(y),bty="l",xlab="local fit in x (green); local fit in xnew (blue); true function in xnew (black)",
ylab="f(x)",main="Local (boundary corrected) fit")
lines(fit$xnew,fTrue(fit$xnew), col="black",lwd=2)
lines(fit$xnew,fit$fit,lwd=2,col="blue")
lines(fit$x,fit$yhat,lwd=2)

}
\author{
Anestis Antoniadis <anestisa@gmail.com>

Yannig  Goude <yannig.goude@edf.fr>
}

