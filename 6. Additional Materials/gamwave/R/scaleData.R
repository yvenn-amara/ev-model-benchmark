#' scaleData
#'
#' sscale a data matrix so that each column ranges from 0 to 1
#'
#' @param X the data matrix
#' @return the scaled design matrix

scaleData<-function(X){
	n<-dim(X)[1]
	p<-dim(X)[2]
    for (j in (1:p)){
        MIN <- min(X[,j]); MAX <-max(X[,j])
        X[,j] <- (X[,j] - MIN)/(MAX - MIN)
    }
return(X)
}
