#' fTrue
#'
#' Compute a noiseless signal with smooth and unsmooth parts
#'
#' @param x input vector at which the function is evaluated
#'
#' @return a vector containing the evaluations of fTrue at x
#'
#' @examples
#' n <- 300
#' x<-seq(0,1,length=n)
#' x<-sort(x)
#' y<-fTrue(x)
#' plot(x,y,col="dodgerblue",type="l",cex=0.3,
#'     main="A function with smooth and unsmooth parts",
#'     col.main="green4",xlab="x",ylab="f(x)")
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export



fTrue <- function(x)
{
  ans <- sqrt(x*(1-x))*sin((2*pi*0.8)/(x+0.2))
  ans <- ans + 0.4*sign(x>0.13)
  ans <- ans - 0.7*sign((x>0.32)*(x<0.38))
  bumpHts <- c(0.43,0.42)
  bumpLocs <- c(0.65,0.91)
  bumpWds <- c(0.03,0.015)
  for (i in 1:2)
    ans <- ans + (bumpHts[i]*
                    pmax(0,(1-abs((x-bumpLocs[i])/bumpWds[i])))^4)
  return(18*ans)
}