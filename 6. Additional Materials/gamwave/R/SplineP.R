#' SplineP
# Computes the B-spline bases (it is essentially the same as bbase)
#'
#' @param x the x grid over which each B-spline basis components is evaluated
#' @param bdeg the degree of the B-splines. By default they are cubic (bdeg=3): they consist 
#' of segments that are polynomials of degree three. Linear (or other degree) B‐splines 
#' are obtained by changing the parameter bdeg.
#' @param nseg number of internal equispaced knots. The default B‐spline bases has \code{nseg + deg} basis functions. 
#'  If it is too much for your data a smaller number for nseg will get nicer results.
#'
#'
#'@return B the resulting B spline bases (a matrix of size n x (nseg + deg))
#'
#' @references
#' Eilers, P. H. C. and Marx, B. D. (1996). Flexible smoothing with B-splines and penalties. \emph{Statistical Science},\strong{11}, pages 89–102.
#'
#'
#' @examples
#'
#' ## S1: Generate x on a linear grid of length 101
#'n<-101
#'x = seq(0, 1, length = n)
#' B <- SplineP(x)
#' matplot(x, B, type = 'l', lty = 1, lwd = 2,main="Individual cubic Bsplines")
#' #  Less B-splines
#' B = SplineP(x, nseg = 5)
#' matplot(x, B, type = 'l', lty = 1, lwd = 2,main="Individual cubic Bsplines")
#'
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export


SplineP <- function(x,range.x,nseg=10,bdeg=3)
{
   # Check legality of range.x.
  
   if (!missing(range.x))
   {
      if (length(range.x)!=2) stop("range.x must be of length 2.")
      if (range.x[1]>range.x[2]) stop("range.x[1] exceeds range.x[1].")
      if (range.x[1]>min(x)) stop("range.x[1] must be <= than min(x).")
      if (range.x[2]<max(x)) stop("range.x[2] must be >= than max(x).")

   }
   

   # Set defaults for `range.x' and `intKnots'

   if (missing(range.x))
      range.x <- c(1.05*min(x)-0.05*max(x),1.05*max(x)-0.05*min(x))
   xl<-range.x[1]
   xr<-range.x[2]
   if (missing(nseg)) nseg <- min(length(unique(x)),15)
   
   # Obtain the design matrix.

    B <- bbase(x, xl = xl, xr = xr, nseg = nseg, deg = bdeg)


   # Add the `range.x' and 'nseg' as attributes
   # of the return object.

   attr(B,"range.x") <- range.x
   attr(B,"nseg") <- nseg

   # Return B matrix with 2 attributes.

   return(B)
}

########## End of SplineP ##########

