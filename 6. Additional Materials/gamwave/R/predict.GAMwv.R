#' predGAMwv
#'
#' Prediction from fitted Wavelet fit model 
#'
#' @param g a fitted GAMwv object with only one (wavelet) predictor
#' @param newdata a regressor one-dimensional vector 
#'
#' @return a list containing
#' 
#' \item{predictor}{forecast on newdata}
#'
#' @examples
# An example of noisy signal
#'n <- 300 
#'set.seed(1) 
#'x <- sort(runif(n)) 
#'f<-fTrue(x); sd=sqrt(var(f)); snr=4; sigma=sd/snr;#
#'y <- f + rnorm(n)
#'fit<-NewGAMwv.fit(y~w(x), numlev =6, offset=-1,filternum = 5, family = "DaubLeAsymm",
#'   penalty = "MCP", data=cbind(y,x))
#'res <- predGAMwv(fit)
#'
#' @author Anestis Antoniadis <anestisa@gmail.com>
#' @author Yannig  Goude <yannig.goude@edf.fr>
#' @export


predGAMwv<-
function (object, newdata, na.action = na.pass, weights = 1, ...) 
{
    if (!inherits(object, "gamwasp")) 
        warning("calling GAMwv(<fake-gmw-object>) ...")
    if (missing(newdata) || is.null(newdata)) {
        X <- object$Z
        moy <- object$alpha0
    }
    else { 
        x<-as.data.frame(newdata)$x
        X<- WavD(x,range.x=range(x),numLevels=object$numlev,family=object$family,filterNumber=object$filternum)
        moy <- mean(object$fitted)
    }
    beta <- object$betaHat
    predictor <- X %*% beta[-1] + beta[1]
    if (!is.null(moy)) predictor <- predictor + moy
    list(fit = predictor)
 }
