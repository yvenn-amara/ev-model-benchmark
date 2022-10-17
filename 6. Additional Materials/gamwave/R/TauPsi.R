#' TauPsi.R
#'
#' \code{TauPsi} Function used to evaluate the constant tau_psi used in the bias part of the confidence band size
#' This is an internal function of package gamwave which allows to compute a confidence bands for the 
#' wavelet based components of amn additive model. 
#'
#' @param filternumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family.
#' It selects the smoothness of wavelet that you want to use for estimation
#' The boundary handling is \code{bc="periodic"}, asumming therefore that the analyzed functions are  periodic on their interval of definition.  
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @import wavethresh
#' @return the constant tau_psi 
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
TauPsi <- function(family="DaubExPhase", filternumber=5){
J <- max(ceiling(log2(filternumber)) + 5, 17)
n <- 2^J
L <- ceiling(J/2)
wc1<-wd(rep(0, length = n), filter.number = filternumber, 
	             family = family, bc = "periodic")
idx<-seq(wc1$fl.dbase$first.last.c[L+1,2]+2,wc1$fl.dbase$first.last.c[L+2,2]+1,by=1)
wc<-NULL
for (i in (1:2^L)) wc<-c(wc,list(wc1))
wr1<-NULL
eye<-diag(rep(1,2^L))
wd.lev <- L
for (i in (1:2^L)) {
	pickout<-as.vector(eye[i,])
	wc[[i]] <- putD(wc[[i]], level = wd.lev, v = pickout)
	aux<-wr(wc[[i]], start.level = 0)
	wr1<-c(wr1,list(aux))
}
wcaux<-matrix(0,ncol=n,nrow=2^L)
for (i in (1:2^L)) wcaux[i,]<- -wr1[[i]]
res<-apply(abs(wcaux),2,sum)
# we need sqrt(n) scaling to account for sampling density
res <- res * sqrt(n)
out <- max(res) * 2^(-L/2)
return(out)
}