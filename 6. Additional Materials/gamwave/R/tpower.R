#' tpower
#'
# Truncated p-th power function
#'
#' @param x the x value where the truncated power function is evaluated.
#' @param t the truncation parameter
#' @param p the power exponent
#'
#' @author Paul Eilers
#' @export

tpower <- function(x, t, p)

    (x - t) ^ p * (x > t)
