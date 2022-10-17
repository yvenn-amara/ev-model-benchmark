#' NewfitWavD.R
#'
#' Inference for an hybrid additive model with a spline part and a wavelet part by backfitting 
#'
#' @param formula a formula
#' @param data dataset
#' @param penalty the nonlinear penalisation method for model selection
#' @param conv.thresh convergence stopping value for relative difference in backfitting iterations
#' @param max.iter maximum number of backfitting iteration
#' @param trace  tracing the computations
#' @param nfolds nfolds (default = 10) number of cross validation folds
#' @param ngrid  size of a grid of equidistant values to evaluate the fitted components
#' @param knots a vector (of dimension the number of components) containing the desired knots for the splines modelling the smooth components
#' @param alpha confidence level 1-alpha when se = TRUE
#' @param se Logical vector. If TRUE then upper and lower confidence bands at level=1-alpha are returned
#' @import AdaptFitOS   wavethresh
#' @return an estimated hybrid model
#' \item{gspa}{the splines fitted part}
#' \item{gmw}{the wavelets fitted part}
#' \item{gridw}{xgrids for the wavelets fitted part}
#' \item{fHatw}{the estimated wavelets effects}
#' \item{hfwidth}{the estimated hfwidths for the wavelet effects confidence bands}
#' \item{fitted}{sum of the splines and wavelets parts}
#' \item{formula}{model formula}
#'
#' @examples
#'  An example of smooth functions (to be compared with splines)
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
#' form<-as.formula("y~s(x1)+s(x2)+w(x3)")
#' data<-cbind(y,A)
#' se<-TRUE
#' knots<-c(40,40)
#' ngrid<-256
#' range.x<-c(0,1)
#' trace=F; nfolds = 10; alpha=0.05;se=TRUE;sig=TRUE;sigma=NULL;
#' numlev<-floor(log2(nrow(data))-2)
#' aux<-NewfitWavD(formula=form, data=data, knots=knots, ngrid=ngrid,numlev =numlev,local=T, filternum = 5,resolution = 16384, family = "DaubExPhase", penalty = "MCP", conv.thresh=1.e-10, offset=0, max.iter = ncol(data)*5, trace=F, nfolds = 10, alpha=0.05,se=se,sig=TRUE,sigma=NULL) 
#' par(mfrow=c(1,3))
#' plot(aux$gspa$newgrid[,1],center(f1(aux$gspa$newgrid[,1])),lty=2,lwd=2,type="n",ylim=c(-20,14))
#' polygon(c(aux$gspa$newgrid[,1],rev(aux$gspa$newgrid[,1])),c(aux$gspa$LCB[,1],rev(aux$gspa$UCB[,1])), col="lightgrey", border=FALSE)
#' lines(aux$gspa$newgrid[,1],center(f1(aux$gspa$newgrid[,1])),lwd=2)
#' lines(aux$gspa$newgrid[,1],aux$gspa$Shat[,1],col="blue",lwd=2)
#' plot(aux$gspa$newgrid[,2],center(f2(aux$gspa$newgrid[,2])),lty=2,lwd=2,type="n",ylim=c(-20,14))
#' polygon(c(aux$gspa$newgrid[,2],rev(aux$gspa$newgrid[,2])),c(aux$gspa$LCB[,2],rev(aux$gspa$UCB[,2])), col="lightgrey", border=FALSE)
#' lines(aux$gspa$newgrid[,2],center(f2(aux$gspa$newgrid[,2])),lwd=2)
#' lines(aux$gspa$newgrid[,2],aux$gspa$Shat[,2],col="blue",lwd=2)
#' plot(aux$gridw[,1],center(fTrue(aux$gridw[,1])),lty=2,lwd=2,type="n",ylim=c(-12,14))
#' polygon(c(aux$gridw[,1],rev(aux$gridw[,1])),c(aux$fHatw[,1]-aux$hfwidth,rev(aux$fHatw[,1]+aux$hfwidth)), col="lightgrey", border=FALSE)
#' lines(aux$gridw[,1],center(fTrue(aux$gridw[,1])),lwd=2)
#' lines(aux$gridw[,1],aux$fHatw[,1],col="blue",lwd=2)
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#'

NewfitWavD<-function (formula, data, knots=knots, ngrid=256, numlev = floor(log2(nrow(data))-2), filternum = 5, local=T,
    resolution = 16384, family = "DaubLeAsymm", penalty = "MCP", conv.thresh=1.e-10, offset=0, max.iter = ncol(data)*5, 
    trace=F, nfolds = 10, alpha=0.05,se=TRUE,sig=TRUE,sigma=NULL) 
{
    formula <- as.formula(formula)
    if (missing(numlev)) numlev <- floor(log2(nrow(data))-2)
    #gam_backfit_wavelet <- GAMwv.fit(formula, numlev, Hybrid, penalty, filternum, family, resolution,
    #                               conv.thresh, offset, max.iter, trace, nfolds, data)
    # gmw<-gam_backfit_wavelet
    datawavelet <- data.frame(wavelet.var(formula, data))
    y <- data[,interpret.formula(formula)$response]
    dataspline <- spline.var(formula,data)
    dataS<-cbind(y,dataspline)
    dataS<-as.matrix(dataS)
    gspa<-NULL
    if (dim(datawavelet)[2]< (dim(data)[2]-1)) {
        split_formula <- unlist(strsplit(paste(deparse(formula, width.cutoff = 200), collapse=""), "\\+|~"))[-1]
        spline_formula <- interpret.formula(formula)[["smooth.spec"]]
        variables <- as.character(lapply(spline_formula, "[[", "term"))
        spline_variable <- grep(paste(variables, collapse="|"), split_formula, value=TRUE)
        for (i in (1:length(variables))) spline_variable[i] <- paste("s(",as.symbol(paste(variables[i], collapse="|")),")")
        formula_gam <- as.formula(paste("y~", paste(spline_variable, collapse="+")))
        # CA SE PLANTE ICI?????
        gspa<-GAMAdapt(formula=formula_gam,data=dataS,knots=knots,ngrid=ngrid,se=se,alpha=alpha)
    }
    if (is.null(gspa)) yres<-y
    if (!(is.null(gspa))) {
    yres <- y - gspa$fitted - gspa$alpha0
    newdata<-cbind(yres,as.matrix(datawavelet))
    colnames(newdata)<-c("y",names(datawavelet))
    gmw <- NewGAMwv.fit(formula, numlev, local=T, penalty, filternum, family, resolution, ngrid, conv.thresh, offset, max.iter, trace, nfolds, newdata)
     }
   fitted<-gmw$fitted
   if (!(is.null(gspa))) fitted <- fitted+gspa$fitted
   sigmahat<-sqrt(sum((y-fitted)^2/(length(y)-length(gmw$betaHat))))
   obs<-matrix(0,nrow=ngrid,ncol=dim(datawavelet)[2])
   hfwidth<-rep(0,dim(datawavelet)[2])
   sigmaHat<-rep(0,dim(datawavelet)[2])
   gridw<-matrix(0,nrow=ngrid,ncol=dim(datawavelet)[2])
   for (i in (1:dim(datawavelet)[2])) {
        x<-eval(parse(text=names(datawavelet)[i]))
        gridw[,i]<-seq(range(x)[1],range(x)[2],length=ngrid)
    }    
   if (se) {
     for (i in (1:dim(datawavelet)[2])) {
        obs[,i]<-gmw$Yhat[,i]
        if (sig==TRUE) out<-AdaptCB(obs[,i],sig=sig,sigma=sigmahat)
        if (!sig==TRUE) out<-AdaptCB(obs[,i])
        hfwidth[i]<-out$hfwidth #/qnorm(1-alpha/2)
        sigmaHat[i]<-out$sigmahat
     }
    }
    yres <- center(y) - gmw$fitted 
    newdataS<-cbind(yres,as.matrix(dataspline))
    colnames(newdataS)<-c("y",names(dataspline))
    gspa<-GAMAdapt(formula=formula,data=newdataS,knots=knots,ngrid=ngrid,se=se,alpha=alpha)
    fHatw<-matrix(0,nrow=ngrid,ncol=dim(datawavelet)[2])
    for (i in (1:dim(datawavelet)[2])) fHatw[,i] <- obs[,i]
    l <- list()
    l$gridw<-gridw
    l$fHatw<- fHatw
        if (se) {
           l$hfwidth <-hfwidth # * qnorm(1-alpha/2)
           l$sigmahat <-sigmaHat
        }
   
    l$gspa <- gspa
    l$gmw <- gmw
    l$fitted<-fitted
    return(l)
}
