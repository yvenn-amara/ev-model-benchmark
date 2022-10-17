#' plot.gamwasp
#'
#' plot of a gamwasp model
#'
#' @param model a fitted model
#'
#' @return a plot of the fitted components
#' @export
#'
#' @examples
#'
#' An example of smooth functions (to be compared with splines)
#' f1 <- function(x) return(3*sin(2*pi*x^3))
#' f2 <- function(x) return(15* x *exp(-x**2))
#' n <- 200
#' set.seed(1)
#' x1 <- runif(n)
#' x1 <- (x1-min(x1))/(max(x1)-min(x1))
#' x2 <- runif(n); x2 <- (x2-min(x2))/(max(x2)-min(x2)); g1 <- f1(x1)-mean(f1(x1))
#' g2 <- f2(x2)-mean(f2(x2)); y <- g1+g2+ rnorm(n)*.5;  A <- cbind(x1,x2); numlev = floor(log2(n))-1;
#' offset = -1# fit with standard wavelets or with hybrid wavelets (uncomment the appropriate lines)
#' gmw <- GAMwv(y, A,numlev=(numlev-1), penalty="SCAD", offset=offset)
#' gmw <- GAMwv(y, A,numlev=numlev, penalty="SCAD", Hybrid=TRUE)# for comparing with mgcv's gam
#' gfit <- gam(y~s(x1)+s(x2)); plotdata <- plot(gfit, pages=1)
#' par(mfrow=c(1,2))
#' o <- order(x1)
#' plot(x1[o], y[o]-g2[o], type="n", xlab="True (dashed); gam (green), wavelet (purple)", ylab="y-g2(.)")
#' lines(x1[o], g1[o], lty='dashed', lwd=2)
#' points(x1[o], y[o]-g2[o], pch=19, cex=.5)
#' lines(x1[o], gmw$Shat[o,1], col='purple',lwd=2)
#' lines(plotdata[1][[1]]$x, plotdata[1][[1]]$fit, lty=2, lwd=2, col="green")
#' o <- order(x2)
#' plot(x2[o],y[o]-g1[o], type="n", xlab="True (dashed); gam (green), wavelet (purple)", ylab="y-g1(.)")
#' lines(x2[o], g2[o], lty='dashed',lwd=2)
#' points(x2[o], y[o]-g1[o], pch=19, cex=.5)
#' lines(x2[o], gmw$Shat[o,2], col='purple', lwd=2)
#' lines(plotdata[2][[1]]$x, plotdata[2][[1]]$fit, lty=2, lwd=2, col="green")
#'
#' datai <- data.frame(cbind(y, x1, x2))
#' model <- GAMwv.fit(y ~ w(x1) + w(x2),numlev=numlev,penalty="SCAD", data = cbind(y, x1, x2))# for comparing with mgcv's gam
#' model2 <- GAMwv(y, A, numlev=numlev, penalty="SCAD")# for comparing with mgcv's gam
#'
#' o <- order(x1)
#' plot(x1[o], model$Shat[o, 1], lwd=2, type="l")
#' o2 <- order(x2)
#' plot(x2[o2], model$Shat[o2, 2], lwd=2, type="l")
#'
#'
#' o <- order(data_agg0$Temp)
#' formula <- Load.center ~ w(Temp) + w(Instant, by = DayType)
#' gmw <- GAMwv.fit(formula, data = data_agg0, penalty="SCAD", Hybrid=F, conv.thresh=0.05, max.iter = 10, trace=T, nfolds = 10)
#' plot(gmw)
#' dataplot <- wavelet.var(formula, data_agg0)
#' plot(data_agg0$Temp[o], gmw$Shat[o, 1], lwd=2, type="l")
#' plot(data_agg0$Temp[o], data_agg0$Load.center[o])
#' data_agg0$Instant[o])
#' o <- order(dataplot[, 3])
#' plot(dataplot[o, 3], gmw$Shat[o, 3], lwd=2, type="l")


plot.gamwasp <- function(model){

  ## model information
  formula <- as.formula(model$formula)
  dataplot <- wavelet.var(formula, model$data)

  if (dim(dataplot)[2] > 1 ) {
    ask <- devAskNewPage(TRUE)
  }

  for(i in 1:dim(dataplot)[2]){
    var <- paste0(colnames(dataplot)[i])
    o <- order(dataplot[, i])
    other_effects <- model$data[o, paste0(model$response)] - rowSums(model$Shat[o, -i])
    ymin <- min(min(model$Shat[o, i]), min(other_effects))
    ymax <- max(min(model$Shat[o, i]), max(other_effects))
    plot(dataplot[o, i], model$Shat[o, i], lwd=2, type="l", xlab=var, ylab=paste0("w(", var, ")"), ylim=c(ymin, ymax))

    if(substr(var,3, 3)=='_'){
      points(dataplot[o, i][dataplot[o, i]!=0], other_effects[dataplot[o, i]!=0], type="p")
    }else{
      points(dataplot[o, i], other_effects, type="p")
    }
  }
  par(ask = FALSE)

}
