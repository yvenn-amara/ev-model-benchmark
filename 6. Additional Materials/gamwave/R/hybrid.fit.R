#' hybrid.fit
#'
#' Estimate an hybrid additive model with a spline part and a wavelet part
#'
#' @param formula a formula
#' @param data dataset
#' @param penalty the nonlinear penalisation method for model selection
#' @param Hybrid True if boundaries are taken care of with hybrid wavelets
#' @param conv.thresh convergence stopping value for relative difference in backfitting iterations
#' @param max.iter maximum number of backfitting iteration
#' @param trace  tracing the computations
#' @param nfolds nfolds (default = 10) number of cross validation folds
#' @param method penalisation method for group selection
#' @import mgcv grpSLOPE wavethresh
#' @return an estimated hybrid model
#' \item{gsp}{the splines fitted part}
#' \item{gmw}{the wavelets fitted part}
#' \item{fitted}{sum of the splines and wavelets parts}
#' \item{formula}{model formula}
#' \item{selectvariable}{variables selected if selection is made}
#'
#' @examples
#'  An example of smooth functions (to be compared with splines)
#' f1 <- function(x) return(3*sin(2*pi*x^3))
#' f2 <- function(x) return(15* x *exp(-x**2))
#' n <- 200
#' set.seed(1)
#' x1 <- runif(n)
#' x1 <- (x1-min(x1))/(max(x1)-min(x1))
#' x2 <- runif(n)
#' x2 <- (x2-min(x2))/(max(x2)-min(x2))
#' g1 <- f1(x1)-mean(f1(x1))
#' g2 <- f2(x2)-mean(f2(x2))
#' y <- g1+g2+ rnorm(n)*.5
#' A <- cbind(x1,x2)
#' numlev = floor(log2(n))-1
#' offset = -1 # fit with standard wavelets or with hybrid wavelets (uncomment the appropriate lines)
#' gmw <- GAMwv(y, A,numlev=(numlev-1), penalty="SCAD", offset=offset, trace =T)
#' gmw <- GAMwv(y, A,numlev=numlev, penalty="SCAD", trace  = T)# for comparing with mgcv's gam
#' gmw <- GAMwv.fit(y ~ w(x1) + w(x2), numlev=numlev, penalty="SCAD",Hybrid=TRUE, data=cbind(y,A), trace  = T)
#' gmw <- hybrid.fit(y ~ w(x1) + s(x2), penalty="SCAD", offset=offset, data=cbind(y,A))
#' gmw <- hybrid.fit(y ~ w(x1) + s(x2), numlev=numlev, penalty="SCAD", Hybrid=TRUE, data=cbind(y,A)) # for comparing with mgcv's gam
#'
#' @export
#'
hybrid.fit <- function(formula, data, numlev=floor(log2(nrow(data))-2), Hybrid=F, penalty="SCAD", filternum=5, family="DaubLeAsymm", resolution=16384,
                      conv.thresh=1.e-10, offset=0, max.iter = ncol(data)*5, trace=F, nfolds = 10, method="fREML", selection=NA,
                      slope.fdr=0.1, grpreg.penalty="grLasso"){

  ## formula
  formula <- as.formula(formula)

  ## wavelet model
  gam_backfit_wavelet <- GAMwv.fit(formula, numlev, Hybrid, penalty, filternum, family, resolution,
                                   conv.thresh, offset, max.iter, trace, nfolds, data)

  print("----------------------------------------------")
  print("The estimation of the non regular part is done")
  print("----------------------------------------------")
  ## response
  y_wavelet <- interpret.formula(formula)$response
  yres <- data[, y_wavelet] - gam_backfit_wavelet$fitted

  ## split the formula - deparse allows to have a formula into a character
  #split_formula <- unlist(strsplit(paste(deparse(formula), collapse=""), "\\+|~"))[-1]
  split_formula <- unlist(strsplit(paste(deparse(formula, width.cutoff = 200), collapse=""), "\\+|~"))[-1]

  ## explanatory spline variables
  spline_formula <- interpret.formula(formula)[["smooth.spec"]]
  variables <- as.character(lapply(spline_formula, "[[", "term"))
  spline_variable <- grep(paste(variables, collapse="|"), split_formula, value=TRUE)

  formula_gam <- as.formula(paste0("yres ~", paste(spline_variable, collapse="+")))

  ## to estimate a gam model data must be a dataframe
  if(is.data.frame(data) == FALSE){
    data <- as.data.frame(data)
  }

  gam_backfit_spline <- mgcv::bam(formula_gam, data=data, method="fREML")

  selectvariable <- NULL
  ##groupe lasso selection
  if (!identical(selection, NA)){
    fitGAM.terms <- predict(gam_backfit_spline, type="lpmatrix")
    if(!identical(grep("\\(Intercept\\)", colnames(fitGAM.terms)), integer(0))){
      fitGAM.terms <- fitGAM.terms[, -1]
    }
    #nknots <- sum(unlist(lapply(gam_backfit_spline$smooth, "[[", "bs.dim")))
    #nbvar <- length(lapply(gam_backfit_spline$smooth, "[[", "term"))
    nknots <- 10
    d <- (ncol(fitGAM.terms))/(nknots-1)
    grp <- NULL
    for(i in c(1:d)){
      grp<-c(grp, rep(i, nknots-1))
    }
    if(identical(selection, "grpreg")){
      modgrpreg <- grpreg::cv.grpreg(X=fitGAM.terms, y=yres, group = grp, penalty = grpreg.penalty)
      coefgrpreg <- coef(modgrpreg) ## Beta at minimum CVE
      var <- coefgrpreg[!coefgrpreg==0 & names(coefgrpreg) != "(Intercept)"]
    }
    if(identical(selection, "grpslope")){
      if(length(unique(grp)) == 1){
        stop("you can't use grpslope with only one value of groupe")
      }
      modslope <- grpSLOPE::grpSLOPE(X=fitGAM.terms, y=yres, group = grp, fdr=slope.fdr)
      coefslope <- coef(modslope)
      names(coefslope) <- colnames(fitGAM.terms)
      var <- coefslope[!coefslope==0]
    }
    selectvariable <- unique(gsub(")|s\\(|\\.[0-9]", "", names(var)))
    grpvariable <- grep(paste(selectvariable, collapse="|"), formula_gam, value=TRUE)
    grpformula <- as.formula(paste0("yres ~ ", paste(grpvariable, sep="+")))
    gam_backfit_spline <- mgcv::bam(grpformula, data=data, method="fREML")
  }

  print("------------------------------------------")
  print("The estimation of the regular part is done")
  print("------------------------------------------")

  hybrid <- list()
  hybrid$gsp <- gam_backfit_spline
  hybrid$gmw <- gam_backfit_wavelet
  hybrid$fitted <- hybrid$gsp$fitted+hybrid$gmw$fitted
  hybrid$formula <- formula
  hybrid$selectvariable <- selectvariable

  class(hybrid) <- "hybrid"
  return(hybrid)
}
