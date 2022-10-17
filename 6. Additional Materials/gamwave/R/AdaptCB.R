#' AdaptCB.R
#'
#' \code{AdaptCB} Function used to evaluate  confidence bands for the 
#' wavelet based components of an additive model. 
#'
#' @param obs raw observed data
#' @param alpha 1-alpha = confidence level
#' @param sig logical; if TRUE noise sd sigma is known; default FALSE
#' @param sigma if sig is TRUE the sd of noise. Otherwise NULL.
#' @param bmin the smallest degree of smoothness of the underlying function
#' @param Mmax the largest Lipschitz constant of the underlying function
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @import wavethresh
#' @return a list containing
#' \item{hfwidth}{the half width of the confidence band}
#' \item{sigmahat}{estimated or given noise standard deviation}
#' \item{jhat}{projection level of final estimate}
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'
AdaptCB<-function(obs, sig=FALSE, sigma=NULL, alpha=0.05, family="DaubExPhase", bmin=0.7, Mmax=2.1){
# Step 0: transform everything to the wavelet domain
require(wavethresh)
L = 2;
# The two tables below have been computed with Bull's mathematica program
# Cascade.m
if  (family=="DaubExPhase") {
    sigma_psi2_a <- c(1.251716, 1.276330, 1.250928, 1.222637, 1.199772)
    v_psi_a      <- c(0.221993, 0.197328, 0.266316, 0.275519, 0.391629)
    tau_psi_a    <- c(2.398754, 2.561150, 2.522424, 2.568761, 2.764600)
    deg = max(12, 2 * ceiling(2*bmin))
    if (deg > 20) print('Maximum degree allowed for the Daubechies class is 20. Try smaller bmin!')
    filternumber=deg/2
    sigma_psi2 <- sigma_psi2_a[deg/2-6+1]
    v_psi   <- v_psi_a[deg/2-6+1]
    tau_psi <- tau_psi_a[deg/2-6+1]
}

if  (family=="DaubLeAsymm") {
    sigma_psi2_a = c(1.361961, 1.253835, 1.286722, 1.232334, 1.243114)
    v_psi_a      = c(0.106518, 0.248681, 0.173642, 0.302351, 0.255337)
    tau_psi_a    = c(1.832399, 1.756489, 1.831377, 1.940050, 1.834827)
    deg = max(8, ceiling(2*bmin))
    filternumber=deg
    if (deg > 10) print ('Maximum degree allowed for the Symmlet class is 10. Try smaller bmin!')
    sigma_psi2 = sigma_psi2_a[deg-6+1] ;
    v_psi = v_psi_a[deg-6+1]
    tau_psi = tau_psi_a[deg-6+1]
}

wav<-filter.select(filter.number=filternumber, family=family, constant=1)
par<-length(wav$H) 
c_psi = cPsi(family, filternumber, bmin)
n = length(obs)
J=log2(n)
wc_obs<-wd(obs, filter.number=filternumber, min.scale=2, bc="periodic")
if (sig)  sigmahat <- sigma /sqrt(n/(2*(par-2)))
if (!sig) sigmahat= sqrt( sum(diff(obs)^2)/(8*n/(2*(par-2))))
# Estimate sigmahat (other options)
# sigmahat <- 1.4826 * mad(accessD(wc_obs,level=J-1))*sqrt(2*n*log(n))
# sigmahat = 1.4826 * mad(accessD(wc_obs,level=J-1))
# sigmahat = 1.4826 * mad(accessD(wc_obs,level=nlev+2))
# sigmahat=sqrt(sum(diff(obs)^2)/(2*n/(par-1)-2))
# sigmahat=sqrt(sum(diff(obs)^2)/(2*n/(par-2)-2)) 

# Step 1: find jmin and jmax (see theory)

jmin = floor(log2(n / log(n) / sigmahat^2) / (4*bmin + 1))
jmax = ceiling(log2(c_psi^2 * Mmax^2 * n / log(n) / sigmahat^2) / (2*bmin + 1))
jmin = max(L, jmin) 
jmax = max(jmin, min(jmax, J-1))

# Step 2: find je = right level for estimation
idx<-NULL
zdx<-NULL
idx = tail(abs(wc_obs$D) >= sigmahat * sqrt(2 * log(n)), n=1)
if (!idx) zdx = 2^jmin-1
if (idx) zdx<-length(wc_obs$D)+1
# idx = find(abs(wc_obs) >= sigmahat * sqrt( (sqrt(3) + sqrt(2))^2 * log(n)), 1, 'last');

je = ceiling(log2(zdx)) - 1
je = min(jmax, max(je, jmin))

# Step 3: find jhat
jhat = je 
nu = (log2(log(n)) + 4* (log2(Mmax) - log2(1-2^(-bmin))))/(4*bmin+1)
jt = min(J-1, floor(2 * jmin + nu))
# nu = 0;
if (je < jmax){
    for (j in (je:(jmax-1))){
        rr = min(2^(-bmin-0.5), (2* sigmahat^2 * log(n) / n)^(1/j/2))
        for (l in (j:jt)){
            theta = sqrt(n) / sigmahat * (2* sigmahat^2 * log(n) / n)^(1/2) * rr^(l-j);
            if (theta < 1/sqrt(log(n))) rej = LevelTest(wc_obs$D[dyad(l)]/sigmahat, theta, 1, n)
            if (theta >= 1/sqrt(log(n))) rej = LevelTest(wc_obs$D[dyad(l)]/sigmahat, theta , theta + sqrt(log(2^l)/2), n)
            if (rej) {
                jhat = j+1
                break
            }
        }
    }
}

# Step 4: compute half-width of the confidence interval
alpha = min(alpha, 1-alpha);
x_alpha = -log(-log(1-alpha / (jmax - jmin + 1)))
rate = min(2^(-bmin), sqrt(2) * (2 * sigmahat^2 * log(n) / n)^(1/jhat/2))
multi=0
if (jt>jhat) multi = sum(rate^(0:(jt - jhat - 1)))
bias = 1.01 * tau_psi * 2^((jhat+1)/2) * (2* sigmahat^2 * log(n) / n)^((jhat+1)/jhat/2) * multi
bias = min(bias, tau_psi * c_psi * Mmax * 2^(-bmin*(jhat+1)) / (1 - 2^(-bmin)))
if (jhat == jmax) x_alpha = -log(-log(1-alpha))
sd = sigmahat * c_const(jhat, sqrt(sigma_psi2),  sqrt(n)) * (x_alpha / a_const(jhat) + b_const(jhat, v_psi))
hfwidth = (sd + bias)*qnorm(1-alpha/2)
out<-list(hfwidth=hfwidth, sigmahat=sigmahat, jhat=jhat)
return(out) 

}
