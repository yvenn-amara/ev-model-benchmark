#' GAMAdapt
#'
#' Fits additive models with spatially adaptive penalized splines
#'
#' @param formula a formula describing the additive splines model to be fitted
#' @param niter a maximum number of iterations for the mean estimation, default is 20.
#' @param niter.var a maximum number of iterations for the variance of random effects estimation, default is 50.
#' @param tol tolerance for the convergence criterion. Default is 1e-6.
#' @param tol.theta  tolerance for the convergence criterion (smoothing parameter function routine). Default is 1e-6.
#' @param returnFit a logical value indicating whether the fitted object should be returned when the maximum number of iterations is reached without convergence of the algorithm. Default is FALSE.
#' @param adap  TRUE for spatially adaptive smoothing parameter
#' @param ngrid  size of a grid of equidistant values to evaluate the fitted components
#' @param knots a vector (of dimension the number of components) containing the desired knots for the splines modelling the components
#' @param alpha confidence level 1-alpha when se = TRUE
#' @param se logical vector. If TRUE then upper and lower confidence bands at level 0.95 are returned
#' @param data dataset
#' @import AdaptFitOS
#'
#' @return A list object of class gamwabsp containing the fitted model. The components are:
#' \item{alpha0}{a real number estimate of the mean}
#' \item{LCB}{the estimated lower confidence bands}
#' \item{UCB}{the estimated upper confidence bands}
#' \item{Shat}{the estimated non-linear effects}
#' \item{fitted}{fitted values of the additive model}
#'
#' @examples
#'  An example of smooth functions (to be compared with mgcv)
#' require(gamwave)
#' require(AdaptFitOS)
#' f1 <- function(x) return(15*sin(2*pi*x^3))
#' f2 <- function(x) return(80* x *exp(-x**2))
#' center<-function(y) return(y-mean(y))
#' n <- 200
#' set.seed(1)
#' x1 <- runif(n)
#' x1 <- (x1-min(x1))/(max(x1)-min(x1))
#' x2 <- runif(n)
#' x2 <- (x2-min(x2))/(max(x2)-min(x2))
#' x3 <- runif(n)
#' x3 <- (x3-min(x3))/(max(x3)-min(x3))
#' g1 <- center(f1(x1))
#' g2 <- center(f2(x2))
#' g3 <- center(fTrue(x3))
#' y <- g1+g2+g3+rnorm(n)*.5
#' y <- center(y)
#' A <- cbind(x1,x2,x3)
#' form<-as.formula("y~s(x1)+s(x2)+s(x3)")
#' data<-cbind(y,A)
#' knots<-c(30,30,30)
#' se<-TRUE
#' gspa<-GAMAdapt(formula=form,data=data,knots=knots,ngrid=256,adap=TRUE,deg=3,tol=1.e-9,tol.theta=1.e-9, niter=20, niter.var=200, se=se)
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export

GAMAdapt<- function(formula,data,knots,ngrid=256,adap=FALSE,deg=3,tol=1.e-9,tol.theta=1.e-9, 
                  niter=20, niter.var=200, se=FALSE,alpha=0.05){
  ## Fits semiparametric regression models using the mixed model representation 
  ##  of penalized splines with spatially adaptive penalties based on the asp function 
  ## from package AdaptFit.
  ## formula
  formula <- as.formula(formula)
  ## response
  y <- data[, interpret.formula(formula)$response]
  Shat<-matrix(0,nrow=ngrid,ncol=ncol(data)-1)
  LCB<-matrix(0,nrow=ngrid,ncol=ncol(data)-1)
  UCB<-matrix(0,nrow=ngrid,ncol=ncol(data)-1)
  alpha0<-mean(y)
  split_formula <- unlist(strsplit(paste(deparse(formula, width.cutoff = 200), collapse=""), "\\+|~"))[-1]
  spline_formula <- interpret.formula(formula)[["smooth.spec"]]
  variables <- as.character(lapply(spline_formula, "[[", "term"))
  spline_variable <- grep(paste(variables, collapse="|"), split_formula, value=TRUE)
  spline_knots<-variables
  for (i in (1:length(variables))) spline_knots[i]<-paste("kn",as.character(i),sep="")
  for (i in (1:length(variables))) spline_variable[i] <- paste("f(",as.symbol(paste(variables[i], collapse="|")), ",", "degree=3,", "knots=",as.symbol(paste(spline_knots[i], collapse="|")),",adap=FALSE)")
  formula_gam <- as.formula(paste("y~", paste(spline_variable, collapse="+")))
  require(AdaptFitOS)
  aux<-NULL
  for (i in (1:length(knots))) { 
       aux[i]<-noquote(eval(paste(as.symbol(paste(spline_knots[i], collapse="|")),
                             "<-","default.knots(",as.symbol(paste(variables[i], collapse="|")),",",knots[i],")")))
       eval(parse(text=aux[i]),parent.frame())
  }
  gsp <- AdaptFitOS::asp2(formula_gam, niter = 20, niter.var = 200)
  scb <- AdaptFitOS::scbM(gsp,level=1-alpha,calc.stdev=FALSE)
  pscb<-plot(scb,plot=FALSE,grid=ngrid)
  newgrid<-NULL
  for (i in (1:length(variables))) {
     aux[i]<-paste("pscb$grid.x$", variables[i],sep="")
     es<-eval(parse(text=aux[i]))
     newgrid<-cbind(newgrid,es)
  }
  for (i in (1:length(variables))) {
    aux[i]<-paste("pscb$fitted$",variables[i],sep="")
    Shat[,i]<-eval(parse(text=aux[i]))
  }
  if (se) {
    aux<-NULL
   for (i in (1:length(variables))) {
      aux[i]<-paste("pscb$lcb$", variables[i],sep="")
      LCB[,i]<-eval(parse(text=aux[i]))
   } 
  }
  if (se) {
    aux<-NULL
   for (i in (1:length(variables))) {
      aux[i]<-paste("pscb$ucb$", variables[i],sep="")
      UCB[,i]<-eval(parse(text=aux[i]))
   } 
  }
# outputs 
  out<-NULL
  out$newgrid=newgrid
  out$Shat=Shat
  if (se) out$pscb=pscb
  if (se) out$UCB=UCB
  if (se) out$LCB=LCB
  out$alpha0=alpha0
  out$fitted<-alpha0+gsp$fitted
  out$type<-"Adaptive-splines"
  class(out)<-"gamwabsp"
  return(out)
}



