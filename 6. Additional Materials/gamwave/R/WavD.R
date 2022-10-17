#' WavD
#'
#' Computes the wavelet basis function design matrix
#'
#' @param x the x vector over which the basis functions are computed
#' @param range.x the min and max values of x over which the basis functions are computed
#' @param numLevels number of levels for the wavelet decomposition
#' @param filterNumber selects the desired filter, an integer that takes a value dependent upon the chosen wavelet family.
#' It selects the smoothness of wavelet that you want to use in the decomposition
#' The boundary handling is \code{bc="periodic"}, asumming therefore that the analyzed functions are  periodic on their interval of definition.  
#' @param resolution the highest resolution wavelets in the expansion
#' @param family the basic family that the wavelet comes from. The choices are \bold{DaubExPhase} for Daubechies' extremal phase wavelets, \bold{DaubLeAsymm} for Daubechies' ``least-asymmetric'' wavelets, and \bold{Coiflets} for Coifman's wavelets.
#' @import wavethresh
#' @return a design matrix containing the wavelet basis functions for the given input vector 
#'
#' @examples
#' # loading the lattice package for the plot
#' require("lattice")
#' n <- 300
#' x<-seq(0,1,length=300)
#' range.x<-c(0,1)
#' Z <- WavD(x,range.x=range.x,numLevels=4,family="DaubLeAsymm",filterNumber=5)
#' B<-cbind(rep(1,n),Z)
#' df <- data.frame(time = x,B)
#' form <- as.formula(paste(paste(names(df)[- 1],  collapse = ' + '),'x',  sep = '~'))
#' xyplot(form,data = df,type = 'l', grid=TRUE,outer =TRUE,ylab="DaubLeAsymm",ylim=c(-4,4),scales=list(tick.number=10))
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

########## R function: WavD ##########

# Creates a wavelet basis function
# design matrix for a given input vector "x".

# Last changed: 09 AUG 2016

WavD <- function(x,range.x=range(x),numLevels=6,filterNumber=5,family="DaubExPhase",
                  resolution=16384)
{
   # Load required package:
   require(wavethresh)
   # Check that x within support limits:
   
   if (any(x<range.x[1])|any(x>range.x[2]))
        stop("All abscissae should be within range.x values.")

   # Ensure that the number of levels is `allowable'.

   if (!any(numLevels==(1:10)))
        stop("Number of levels should be between 2 and 10.")

   # Ensure the resolution value is `allowable'.

   if (!any(resolution==(2^(10:20))))
        stop("Resolution value should be a power of 2, with the
              power between 10 and 20.")
   
    # Transform x to the unit interval and obtain variables
    # required for linear interpolation:

    xUnit <- (x - range.x[1])/(range.x[2] - range.x[1])
    xUres <- xUnit*resolution
    fXuRes <- floor(xUres)
   
    # Set filter and wavelet family  
    
	# family <- "DaubLeAsymm"
    # family <- "DaubExPhase"
	# family <- "Coiflets"
    K <- 2^numLevels - 1
    
    # Create a dummy wavelet transform object

    wdObj <- wd(rep(0,resolution),filter.number=filterNumber,
               family=family)
   
    Z <- matrix(0,length(x),K)
    for (k in 1:K)
    {
       # Create wobj so that it contains the Kth basis
       # function of the Z matrix with `resolution' regularly 
       # spaced points:
      
       putCobj <- putC(wdObj,level=0,v=0)
       putCobj$D <- putCobj$D*0
       putCobj$D[resolution-k] <- 1
       
       # Obtain kth column of Z via linear interpolation
       # of the wr(putCobj) grid values:

       wtVec <- xUres - fXuRes
       wvVec <- wr(putCobj)
       wvVec <- c(wvVec,rep(wvVec[length(wvVec)],2))
       Z[,k] <- sqrt(resolution)*((1 - wtVec)*wvVec[fXuRes+1]
                                  + wtVec*wvVec[fXuRes+2])
    }

    # Create column indices to impose "left-to-right" ordering
    # within the same level:

    newColInds <- 1
    for (ell in 1:(numLevels-1))
       newColInds <- c(newColInds,(2^(ell+1)-1):(2^(ell)))

    Z <- Z[,newColInds]
    
    return(Z)
}

############ End of WavD ###########

