#' stackingInst
#'
#' stacking by instant 
#'
#' @param i the instant
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
#' @param trControl control parameter for the training of the models, see \code{\link{caret:train}}
#' @param data.export a boolean indicating wether or not the datasets (including models' prediction) should be returned or not
#' 
#' @examples
#' y<-rnorm(10)
#' ychap<-rep(0,10)
#' rmse(y,ychap,digits=4)
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'
#'
stackingInst<-function(i,x0,x1,y0,train.rate,model.list,trControl,data.export=FALSE,Inst0,Inst1)
{
  sel0<-which(Inst0==i)
  sel1<-which(Inst1==i)
  
  x0.inst<-x0[sel0,]
  x1.inst<-x1[sel1,]
  y0.inst<-y0[sel0]
  
  mod<-stacking(x0.inst,x1.inst,y0.inst,train.rate,model.list,trControl,data.export=FALSE)
  return(mod)
}

