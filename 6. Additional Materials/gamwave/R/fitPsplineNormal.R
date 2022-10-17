#' fitPsplineNormal
#'
#' Direct scatterplot semi-parametric regression smoothing with Eilers P-splines and a single smoothing paprameter
#'
#' @param x abcissae of data
#' @param y response
#' @param xnew a new xgrid  over which we evaluate the fitted function (for display purposes). If xnew=NULL then xnew=sort(x). 
#' @param range.x the min and max values of x over which the basis functions are evaluated
#' @param nseg the number interior knots for the B-splines basis functions (all splines are of order 4 (cubic (deg=3)))
#' @param pord  order of difference penalty (defaults to 2).
#' @param lambda smoothness parameter
#' @param plot logical parameter. if plot=T the data and fit are ploted. 
#' @param se the width of standard error bands (computed and ploted only when se > 0)
#
#'
#' @return a list containing
#' 
#'   \item{xnew}{the new xgrid  over which we evaluate the fitted function}
#'   \item{fHat}{a vector containing \eqn{\widehat{f}(x)}}
#'   \item{beta}{vector of estimated P-splines coefficients}
#'   \item{xnew}{the xnew grid for plotting: values are sorted}
#'   \item{munew}{the estimate evaluated on xnew}
#'   \item{lambda}{the penalty parameter used in the fit}
#'   \item{cv}{the cross-validation error associated to lambda}
#'   \item{effdim}{the effective dimension}
#'   \item{ed.resid}{the estimated degrees of freedom for the fit}
#'   \item{sigma}{the estimated dispersion associated to the fit}

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
#'lambda<-11000
#' # Display the fit:
#' par(mfrow=c(1,2))
#'fHatsplines<-fitPsplineNormal(x,y,xnew=xnew,nseg=nseg,lambda=lambda,se=2,plot=T)
#' # Another plot to check the outputs
#' plot(x,y,xlim=range(xnew),bty="l",type="n",xlab="radiation",
#'   ylab="cuberoot of ozone",main="Direct fit with a given value of smooth parameter", 
#'  col.main="green4")
#'lines(fHatsplines$xnew,fHatsplines$munew,col='blue',lwd=2)
#'points(x,y,pch=19,col="grey",cex=0.5)
#'points(fHatsplines$x,fHatsplines$fHat,col="blue")
#'detach(environmental)
#'
#' @author Paul Eilers and Brian Marx, 2007
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

fitPsplineNormal= function(x, y, xnew=NULL, nseg = 10, pord = 2, lambda = 1, 
                   plot = T, se = 2)
{

# Paul Eilers and Brian Marx, 2007

    # Compute B-spline basis
    m = length(x)
	xl<-range(x)[1]
	xr<-range(x)[2]
	range.x<-range(x)
    B<-SplineP(x,range.x=range.x,nseg=nseg)

    # Construct penalty stuff for the given lambda
    n = dim(B)[2]
    P = sqrt(lambda) * diff(diag(n), diff = pord)
    nix = rep(0, n - pord)

    # Fit
	# The R function lsfit() performs linear regression. By default it uses an intercept. This should not be used here. 
    f = lsfit(rbind(B, P), c(y, nix), intercept = F)
    h = hat(f$qr)[1:m]
    beta = f$coef
    mu = B %*% beta

    # Cross-validation and dispersion
    r = (y - mu ) / (1 - h)
    cv = sqrt(sum(r ^2))
    sigma = sqrt(sum((y - mu) ^2) / (m - sum(h)))
###  Compute curve on a new grid if necessary ####
    if(is.null(xnew)) {
		  xnew<-seq(xl, xr, length = m)
		  Bnew = SplineP(xnew, range.x=range(x), nseg = nseg)
		  munew<-Bnew %*% beta
	     }		  
    if(!is.null(xnew)) 
      {
        Bnew = SplineP(xnew, range.x=range.x, nseg = nseg)
		munew<-Bnew %*% beta 
    }
    
 
    # Plot data and fit
	if (plot) {
	  plot(x, y, main = 'Penalized splines (P-splines)', xlab ='' , ylab = '',pch=19,col="grey",cex=0.5,col.main="green4")
	  lines(xnew, munew, col = 'blue',lwd=2)
	}
		

    # Error bands ("Bayesian estimate")
	if (plot & se > 0) {
	 ### Error bands ("Bayesian estimate")
	 Covb = solve(t(B) %*% B+ t(P) %*% P)
	 Covz = sigma ^ 2 * Bnew %*% Covb %*% t(Bnew)
	 seb = se * sqrt(diag(Covz))
	 lines(xnew, munew + seb, lty = 2, col = 'blue')
	 lines(xnew, munew - seb, lty = 2, col = 'blue')
	}
    
    # Return list
    pp = list(fHat = mu, beta=beta, x = x, lambda = lambda, xnew = xnew, munew = munew,
               cv = cv, effdim = sum(h), ed.resid = m - sum(h), sigma = sigma)
    class(pp) = "pspfit"
    return(pp)
}
