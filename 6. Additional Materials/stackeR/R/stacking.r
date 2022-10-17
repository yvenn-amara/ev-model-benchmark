#' stacking
#'
#' compute a list of model and their stacked versions
#'
#' @param x0 the estimation dataset, a matrix X of covariate observations used for the estimation step
#' @param x1 the prediction dataset, a matrix X of covariate observations used for the prediction step
#' @param y0 a vecteur Y of the target observation used for the estimation step
#' @param train.rate a real number in [0,1] corresponding to the rate of observation used for model computation, randomly picked (how depends on sample.type)
#' n1=floor(train.rate*n) observations are used for model computation, n-n1 observations for their stacked versions
#' @param model.list a list of model to compute, already testes ones are:
#' projection based method: \code{projectionList<-c("pcr","ppr","pls","plsRglm","simpls","spls")}
#' trees: \code{treeList<-c("cubist","gbm","blackboost","ctree","ctree2","rpart1SE","rpart2","treebag","xgbTree")}
#' additive models: \code{additiveList<-c("bagEarth","bagEarthGCV", "bstTree","earth","gamLoess","gamSpline","gcvEarth")}
#' kernel methods: \code{kernelList<-c("kernelpls","kknn","svmLinear","svmPoly","svmRadial","svmRadialSigma","svmRadialCost","knn","kknn")}
#' @param trControl control parameter for the training of the models, see \code{\link[caret]{train}}
#' @param data.export a boolean indicating wether or not the datasets (including models' prediction) should be returned or not
#' @param sample.type the type of sampling method to split the observations in the dataset into two, one for model estimation, one for stacking. Could be: "random":
#' random sampling without replacement, "consecutive": just the first n1 rows or "blocks": data are picked by blocks of size \code{block.size} could be useful 
#' for time series
#' @return a list containing:
#' \item{model.list}{a list of models' output, as many elements as model in the argument model.list}
#' \item{forecast}{the prediction of the models on the prediction dataset}
#' \item{forecast.stack}{the prediction of the stacked models on the prediction dataset}
#' \item{Xnew}{a matrix X of covariate observations used for the stacked estimation step, return only when \code{data.export=TRUE}}
#' \item{Ynew}{a vector Y of covariate observations used for the stacked estimation step, return only when \code{data.export=TRUE}}

#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' rmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'
stacking<-function(x0,x1,y0,train.rate,model.list,trControl,data.export=FALSE,sample.type="random",block.size=NULL)
{
  if(sample.type=="random")
  {
    n<-nrow(x0)
    subset.index<-sample(c(1:n),floor(train.rate*n),replace=FALSE)
  }
  
  if(sample.type=="consecutive")
  {
    n<-nrow(x0)
    subset.index<-c(1:floor(train.rate*n))
  }
  
  if(sample.type=="blocks")
  {
    n<-nrow(x0)
    n1<-floor(train.rate*n)
    NbBlock<-floor((n-n1)/block.size)
    s<-sample(seq(1,n,by=block.size+1),NbBlock)
    subset.index<-pmin(rep(s,each=block.size)+rep(c(0:(block.size-1)),NbBlock),n)
  }
  

  x0a<-x0[subset.index,]
  x0b<-x0[-subset.index,]
  y0a<-y0[subset.index]
  y0b<-y0[-subset.index]
  
  mod1<-lapply(model.list,auto.train,x=x0a,y=y0a,trControl=trControl)
  names(mod1)<-model.list
  
  sel1<-!unlist(lapply(mod1,is.null))
  #print(model.list[sel1])
  
  forecast1<-lapply(mod1[sel1],predict,newdata=x0b)
  forecast2<-lapply(mod1[sel1],predict,newdata=x1)
  
  
  forecast1.mat<-matrix(unlist(forecast1),ncol=sum(sel1),byrow = F)
  forecast2.mat<-matrix(unlist(forecast2),ncol=sum(sel1),byrow = F)
  colnames(forecast1.mat)<-model.list[sel1]
  colnames(forecast2.mat)<-model.list[sel1]
  
  mod.stack<-lapply(model.list,auto.train,x=cbind(x0b,forecast1.mat),y=y0b,trControl=trControl)
  sel.stack<-!unlist(lapply(mod.stack,is.null))
  
  
  forecast.stack<-lapply(mod.stack[sel.stack],predict,newdata=cbind(x1,forecast2.mat))
  
  forecast.stack.mat<-matrix(unlist(forecast.stack),ncol=sum(sel.stack),byrow = F)
  colnames(forecast.stack.mat)<-paste(model.list[sel.stack],".stack",sep="")
  
  mod1$forecast<-forecast2.mat
  mod1$forecast.stack<-forecast.stack.mat
  
  if(data.export==TRUE)
  {
    mod1$Xnew<-cbind(x0b,forecast1.mat)
    mod1$Ynew<-y0b
    mod1$subset.index<-subset.index
  }
 
  return(mod1)
}





# 
# param<-list()
# param$trControl<-trControl
# param$x<-as.matrix(x0a)
# param$y<-as.numeric(y0a)
# param$method<-"lm"
# mod<-do.call(train,param)
# 
# 
# head(x0a)
















# 
# res <- tryCatch({
#   do.call(`+`, list(c(1,2), c(1, 2)))
# }, error = function(e){
#   NULL
# })
# 




# function(x,y,model,trControl){param$method<-"lm"; 
# param$x<-x; param$y<-y;param$method<-model; capture.output(mod<-do.call(train,param))}



# function(x,y,model,trControl){param$method<-"lm"; 
# param$x<-x; param$y<-y;param$method<-model; capture.output(mod<-do.call(train,param))}

# tryCatch(print(paste("log of", input, "=", log(input))),warning = function(w) {print(paste("negative argument", input)); 
#            log(-input)},
#          +              error = function(e) {print(paste("non-numeric argument", input));
#            NaN})





# 
# Data<-readRDS("C:\\Amont\\BENCHMARK\\Load_data_daily.RDS")
# names(Data)
# Data$Date
# Dataa=Data[1:(nrow(Data)-365),]
# Datab=Data[(nrow(Data)-365):nrow(Data),]
# 
# 
# Dataa_matrix<-Dataa[, c("Trend","wM","TL80wM","TL95wM","LOAD.24","LOAD.168","D0","D1","D2","D3","D4", "D5","D6")]
# Datab_matrix<-Datab[,c("Trend","wM","TL80wM","TL95wM","LOAD.24","LOAD.168","D0","D1","D2","D3","D4", "D5","D6")]
# 
# 
# model.list<-c("lm","lasso")
# trControl<-trainControl("repeatedcv", repeats=1,number=10)
# 
# stak<-stacking(x0=Dataa_matrix,x1=Datab_matrix,y0=Dataa$LOAD,train.rate=0.8,model.list,trControl)
#   
#   
# plot(Datab$LOAD,type='l',col='blue')
# matlines(stak$forecast,type='l')
# matlines(stak$forecast.stack,col='pink')  
# 
# 
# mod<-auto.train(Dataa_matrix,Dataa$LOAD,"lm",trControl)
# 
# test<-lapply(model.list,auto.train,x=Dataa_matrix,y=Dataa$LOAD,trControl=trControl)
# 
# 
# prev<-lapply(test,predict,newdata=Datab_matrix)
# 
# m<-matrix(unlist(prev),ncol=2,byrow = F)
# 
# test[[2]]
# 
# train.rate<-0.8
# n<-nrow(Dataa)
# subset.index<-sample(c(1:n),floor(train.rate*n),replace=FALSE)
# 
# 
# 
# 
# 
# model.list<-c("lm","lasso")
# trControl<-trainControl("repeatedcv", repeats=1,number=10)
# param<-list()
# param$trControl<-trControl
# param$x<-Dataa_matrix
# param$y<-Dataa$LOAD
# param$method<-model.list[[2]]
# mod<-do.call(train,param)
# mod$forecast1<-predict(mod, newdata=)
# 
# 
# mod.inst$fitted<-predict(mod.inst,newdata=x0[sel0,])
# mod.inst$forecast<-predict(mod.inst,newdata=x1[sel1,])
# 
# 
# 
# 
# 
# 
# param<-list()
# param$trControl<-trControl  
# param$x<-Data0$LOAD
# param$y<-Data1$LOAD
# mapply(train,model.list,trControl,MoreArgs=param)
# 
# 
# 
# 
# 
# 
# 
# #length(subset.index)









