#' bbase
#'
# Computes the B-spline bases
#'
#' @param x the x grid over which each B-spline basis components is evaluated
#' @param deg the degree of the B-splines. By default they are cubic (deg=3): they consist 
#' of segments that are polynomials of degree three. Linear (or other degree) B‐splines 
#' are obtained by changing the parameter deg.
#' @param nseg number of internal equispaced knots. The default B‐spline bases has \code{nseg + deg} basis functions. 
#  If it is too much for your data a smaller number for nseg will get nicer results.
#'
#'@return B the resulting B spline bases (a matrix of size n x (nseg + deg))
#
#' @author Paul Eilers and Brian Marx
#' @export

bbase <- function(x, xl = min(x), xr = max(x), nseg = 10, deg = 3){
# Construct B-spline basis
    dx <- (xr - xl) / nseg
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    P <- outer(x, knots, tpower, deg)
    n <- dim(P)[2]
    D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    B }
	

