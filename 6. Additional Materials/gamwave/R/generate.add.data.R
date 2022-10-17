#'generate.add.data
#'
#' Simulates an additive noisy function to be fitted with hybrid wavelets and splines
#'
#' @param n number of observations (length of data)
#' @param ds total number of smooth components
#' @param dw total number of  no-smooth components
#' @param ps number of effective smooth components
#' @param pw number of effective no-smooth components
#' @param sigma standard deviation of noise
#' @param seed the seed number
#' @param active list of of active variables
#' @return a list with the following components
#' \item{d}{total number of  components}
#' \item{p}{total number of effective components}
#' \item{active}{the used set of active variables}
#' \item{not_active}{the complement set of active}
#' \item{A}{the design matrix of all d regressors}
#' \item{g}{the matrix whose columns contain the centered active additive functions} 
#'   evaluated on the corresponding entries of A
#' \item{ytrue}{the true additive signal (centered)}
#' \item{sigma}{the sd used for the simulations} 
#' \item{y}{the simulated observations}
#' 
#'@examples
#'n<-300
#'ds<-4
#'dw<-4
#'ps<-2
#'pw<-3
#'sigma<-1
#'seed<-1
#'active<-c(1,2,4,5,6)
#'out<-generate.add.data(n=n,ds=ds,dw=dw,ps=ps,pw=pw,sigma=sigma,seed=seed,active=active)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export


generate.add.data<-function(n=300,ds=4,dw=4,ps=2,pw=3,sigma=1,seed=1, active=c(1,2,4,5,6)){
	# I function
	wavedoppler <- function(x, snr){
	  y = snr * sqrt(x * (1 - x)) * sin((2.1 * pi)/(x + 0.05))/0.289
	  return(y)
	}
	# II function
	waveblocks <- function(x, snr){
	  pos = c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
	  hgt = c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
	  y = rep(0, length(x));
	  for(j in 1:length(pos))
	    y = y + ((1 + sign(x - pos[j])) * hgt[j])/2;
	  end
	  y = (snr * y)/1.914;
	  return(y)
	}
	# III function
	wavebumps <- function(x, snr){
	  pos = c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
	  hgt = c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
	  wth = c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)
	  y = rep(0, length(x))
	  for(j in 1:length(pos))
	    y = y + hgt[j]/(1 + abs(x - pos[j])/wth[j])^4
	  end
	  y=(snr * y)/0.665
	  return(y)
	}
	# IV function
	waveheavisine <- function(x, snr){
	  y = snr * (4 * sin(4 * pi * x) - sign(x - 0.3) - sign(0.72 - x))/2.97
	  return(y)
	}
	# V function
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
	# VI functiom
	f1 <- function(x) return(20*sin(2*pi*x^3))
	# VII function
	f2<-  function(x) return(80* x *exp( -x**2))
	# VIII function
	f3<-  function(x) return( -20*x**2)
	
    l<-list()
	set.seed(seed)
  	d <- ds+dw
	p<-ps+pw
	not_active=setdiff(c(1:d),active)
	A <- matrix(0,nrow=n,ncol=d)
	for(i in active){
  		aux=runif(n)
  	  	A[,i]<-(aux-min(aux))/(max(aux)-min(aux))
	}
	for(i in not_active){
  		aux=runif(n);
  	  	A[,i]<-(aux-min(aux))/(max(aux)-min(aux))
	}
 # definition and centering of the functions (see signals.R)
	g=matrix(data=0,nrow=n,ncol=d)
	g[,1]<-wavebumps(A[,1],5)-mean(wavebumps(A[,1],5)) 
	g[,2]<-waveblocks(A[,2],7)-mean(waveblocks(A[,2],7)) 
	g[,4]<-fTrue(A[,4])-mean(fTrue(A[,4]))
	g[,5]<-f1(A[,5])-mean(f1(A[,5]))
	g[,6]<-f2(A[,6])-mean(f2(A[,6]))
	# observations 
	ytrue <- rowSums(g) # the additive signal
	y <- ytrue + rnorm(n)*sigma 
 	# centering of observations
	y=y-mean(y)
 # the outputs
 l$d<-d
 l$p<-p
 l$active<-active
 l$not_active
 l$A<-A
 l$g<-g
 l$ytrue<-ytrue
 l$sigma<-sigma
 l$y<-y
 return(l)
 }