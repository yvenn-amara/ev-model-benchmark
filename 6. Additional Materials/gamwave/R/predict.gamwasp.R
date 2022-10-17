#' predict.gamwasp
#'
#' Prediction from fitted GAM (mgcv's gam, or GAMsp or GAMwv) model 
#'
#' @param g a fitted gamBF object 
#' @param newdata a regressor matrix (without constants)
#' @return a list containing
#' 
#' \item{S.forecast}{forecast term by term}
#' \item{forecast}{forecast of the target}
#'
#' @examples
#' require(stacking)
#'f1 <- function(x) return(3*sin(2*pi*x^3))
#'f2 <- function(x) return(15* x *exp(-x**2))
#'
#'n <- 200
#'set.seed(1)
#'x1 <- runif(n)
#'x1<-(x1-min(x1))/(max(x1)-min(x1))
#'x1<-sort(x1)
#'x2 <- runif(n)
#'x2<-(x2-min(x2))/(max(x2)-min(x2))
#'x2<-sort(x2)
#'g1<-f1(x1)-mean(f1(x1)) 
#'g2<-f2(x2)-mean(f2(x2))
#'
#'y <- g1+g2+ rnorm(n)*.5 
#'
#'A<-cbind(x1,x2)
#'numlev=floor(log2(n))-2
#'gamW<-GAMwv(y,A,numlev=numlev,penalty="SCAD",Hybrid=FALSE)
#'gamMGCV<-gam(y~s(x1)+s(x2))
#'gamMGCV$fitted=predict(gamMGCV)
#'x1new <- runif(n)
#'x1new<-(x1new-min(x1new))/(max(x1new)-min(x1new))
#'x1new<-sort(x1new)
#'x2new <- runif(n)
#'x2new<-(x2new-min(x2new))/(max(x2new)-min(x2new))
#'x2new<-sort(x2new)
#'g1new<-f1(x1new)-mean(f1(x1new)) 
#'g2new<-f2(x2new)-mean(f2(x2new))
#'ynew <- g1new+g2new+ rnorm(n)*.5 
#'Anew=cbind(x1new,x2new)
#'Data<-data.frame(x1=x1new,x2=x2new)
#'gamMGCV$forecast=predict(gamMGCV,newdata=Data)
#'mape(y,gamMGCV$fitted)
#'mape(ynew,gamMGCV$forecast)
#'gamW$forecast<-predict(gamW,newdata=Anew)$forecast
#'mape(y,gamW$fitted)
#'mape(ynew,gamW$forecast)
#'
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

predict.gamwasp<-function(g,newdata)
{
  ## wavelet part forecasting
  formula <- g$formula
  newdata <- wavelet.var(formula, newdata)
  
  p<-ncol(newdata)
  n<-nrow(newdata)
  Shat<-matrix(0,nrow=n,ncol=p)
  
  for(pp in 1:p){
    x<-(newdata[,pp]-min(newdata[,pp]))/(max(newdata[,pp])-min(newdata[,pp]))
    range.x <- c(0,1)
    if(g$type=="splines") 
      {
        X<-cbind(rep(1,n),x) 
        Z<-SplineD(x,range.x=range.x,intKnots=g$intKnots[,pp]) 
      }
    
    if(g$type=="wavelet")
      {
        X<-rep(1,n)
  		Z <- WavD(x, range.x = range.x, numLevels = g$numlev, filterNumber=g$filternum,family=g$family)
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
