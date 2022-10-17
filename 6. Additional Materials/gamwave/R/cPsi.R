#' cPsi.R
#'
#' \code{Cpsi} Function used to evaluate the constant c_psi used in the bias part of the confidence band size
#' This is an internal function of package gamwave which allows to compute a confidence bands for the 
#' wavelet based components of amn additive model. Typically users will want to modify the defaults 
#' if model fitting deosn't seem appropriate.
#'
#' @param filternumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family.
#' It selects the smoothness of wavelet that you want to use for estimation
#' The boundary handling is \code{bc="periodic"}, asumming therefore that the analyzed functions are  periodic on their interval of definition.  
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @param beta the highest smoothness index bound for the function to be estimated
#' @import wavethresh
#' @return the constant c_psi 
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
cPsi <- function(family="DaubExPhase", filternumber=5, beta=2.2){
require(wavethresh)   
wav<-filter.select(filter.number=filternumber, family=family, constant=1)
par<-length(wav$H)
J<-max(ceiling(log2(par)) + 5, 17)
n<-2^J
x <- (1:1:n)/n
L <- ceiling(J/2)
# wc <- rep(0, n)
# wc[2^L] <- 1
lowest.level<-L
wd.lev <- lowest.level-1
zwd <- wd(rep(0, length = n), filter.number = filternumber, family = family, bc = "periodic")
pickout <- rep(0, 2^wd.lev)
pickout[2^wd.lev]<-1
zwd <- putD(zwd, level = wd.lev, v = pickout)
zwr<-wr(zwd, start.level = 0)
wc<- -zwr
xx<-x[wc!=0]
xx[xx > 0.5] <- xx[xx>0.5]-1
xx <- xx * (2^(L-1))
step <- abs(xx[1] - xx[2])
xx1 <- abs(xx)^beta
xx2 <- abs(xx)^(2*beta)
xx<-pmax(xx1,xx2)
wc <- wc[wc!=0]
wc = wc * (2^(-(L-1)/2))
out = sum(abs(wc*xx)) * step
multi<-1
for (i in (1:floor(2*beta))) multi = multi * (2*beta + 1 - i)
out <- max(1, out * multi)
return(out)
}