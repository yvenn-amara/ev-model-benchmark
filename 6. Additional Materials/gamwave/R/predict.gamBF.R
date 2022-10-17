#' predict.gamwasp
#'
#' Prediction from fitted GAM (backfitting) model 
#'
#' @param g a fitted gamBF object 
#' @param newdata a regressor matrix (without constants)
#' @return a list containing
#' 
#' \item{S.forecast}{forecast term by term}
#' \item{forecast}{forecast of the target}
#'
#' @examples
#' y<-rnorm(10)
#' 
#' 
#' @author Anestis Antoniadis <anestisa@gmail.com>
#' @author Yannig  Goude <yannig.goude@edf.fr>
#' @export

predict.gamwasp<-function(g,newdata)
{
  p<-ncol(newdata)
  n<-nrow(newdata)
  Shat<-matrix(0,nrow=n,ncol=p)
  
  for(pp in 1:p){
    x<-(newdata[,pp]-min(newdata[,pp]))/(max(newdata[,pp])-min(newdata[,pp]))
    range.x <- c(0,1)
    if(g$type=="splines") 
      {
        X<-cbind(rep(1,n),x) 
        Z<-ZOSull(x,range.x=range.x,intKnots=g$intKnots[,pp]) 
      }
    
    if(g$type=="wavelet")
      {
        X<-rep(1,n)
        Z<-ZDaub(x,range.x=range.x,filterNumber=g$filternum,family=g$family,numLevels=g$numlev)
      }
    
    Cmat <- cbind(X,Z)
    Shat[,pp]<- Cmat%*%g$betaHat[,pp]
  }
  ychap<-apply(Shat,1,sum)
  
  l<-list()
  l$S.forecast<-Shat
  l$forecast<-ychap+g$alpha0
  
  return(l)
}










