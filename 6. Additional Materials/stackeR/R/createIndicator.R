#' createIndicators
#'
#' convert a factor covariate in a data.frame to a set of indictors covariates
#'
#' @param Data the dataset, containing observations of covariates 
#' @param factor.name name of the factor covariate
#' @param prefix a character string to prefix the names of indicators covariates
#' 
#' @examples
#' to complete
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'


createIndicators<-function(Data,factor.name,prefix=NULL)
{
  l<-levels(Data[,factor.name])
  M<-matrix(0,nrow=nrow(Data),ncol=length(l))
  for(i in c(1:length(l)))
  {
    M[which(Data[,factor.name]==l[i]),i]<-1
  }
  if(is.null(prefix)) colnames(M)<-levels(Data[,factor.name])
  if(!is.null(prefix)) colnames(M)<-paste(prefix,levels(Data[,factor.name]),sep="")
  Data<-data.frame(Data,M)
  return(Data)
}
  
  

# 
# setwd("C:\\Amont\\PrixAlex\\")
# Data<-read.csv("histData2.csv",sep=';',header=T)
# Date<-as.POSIXct(strptime(Data$X.Date, "%Y-%m-%d %H:%M:%S"))
# Data$Date<-Date
# Data$Dow<-as.factor(weekdays(Data$Date))
# Data$Hour<-as.factor(Data$DemiHeure)
# 
# names(Data)
# Data<-createIndicators(Data,"Dow")
# Data<-createIndicators(Data,"Hour",prefix="H")


# plot(Data$H1[1:200])
# names(Data)
# summary(Data)
