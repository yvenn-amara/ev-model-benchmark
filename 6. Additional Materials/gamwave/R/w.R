#' w.R
#'
#' \code{w} Function used in definition of wavelet terms within gamwave model formula. 
#' The function does not evaluate a wavelet - it exists purely to help set up a model using wavelet.
#'
#' @param x	covariate
#' @param by covariate to multiple by x
#'
#' @return wavelet information
#' \item{alpha0}{a real number estimate of the mean}
#' \item{betaHat}{a vector of estimated regression parameters}
#' \item{Shat}{the estimated non-linear effects}
#' \item{type}{the type of basis functions used for the decomposition, "wavelet" or "splines"}
#' 
#' @examples 
#' set.seed(0)
#' n<-200;sig2<-4
#' x1 <- runif(n, 0, 1)
#' fac<-c(rep(1,n/2),rep(2,n/2)) # create factor
#' w(x1) 
#' w(x1, by=fac)
#' 
#' @export
#' 
w <- function (x, by = NA) {

  ## by variable can't be a dot
  by.var <- deparse(substitute(by), backtick = TRUE, width.cutoff = 500)
  if (by.var == ".")
    stop("by=. not allowed")

  ## x can't be a dot
  vars <- as.list(substitute(x))
  term <- deparse(vars[[1]], backtick = TRUE, width.cutoff = 500)
  if (term[1] == ".")
    stop("w(.) not yet supported.")

  full.call <- paste("w(", term[1], sep = "")

  label <- paste(full.call, ")", sep = "")
  ret <- list(term = term, by = by.var, label = label)
  class(ret) <- "wavelet.spec"
  ret
}
