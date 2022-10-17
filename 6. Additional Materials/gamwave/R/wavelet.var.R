#' wavelet.var
#'
#' Creation of the dataset for a wavelet model
#'
#' @param formula model formula
#' @param data dataset with explanatory variables
#'
#' @return the new dataset
#'
#' @examples
#'
#' f1 <- function(x) return(3*sin(2*pi*x^3)); f2 <- function(x) return(15* x *exp(-x**2)); n <- 200; set.seed(1); x1 <- runif(n)
#' x1<-(x1-min(x1))/(max(x1)-min(x1)); x2 <- runif(n); x2<-(x2-min(x2))/(max(x2)-min(x2)); g1<-f1(x1)-mean(f1(x1))
#' g2<-f2(x2)-mean(f2(x2)); y <- g1+g2+ rnorm(n)*.5;  A<-cbind(x1,x2); numlev=floor(log2(n))-1;
#' fac<-c(rep(1,n/2),rep(2,n/2))
#' data <- cbind(y, x1, fac)
#' formula <- "y ~ w(x1)" ; wavelet.var(formula, data)
#' formula <- "y ~ w(x1, by=fac)" ; wavelet.var(formula, data)
#'
#' @export
#'
wavelet.var <- function(formula, data){

  ## data must be a dataframe
  data <- as.data.frame(data)

  ## formula
  formula <- as.formula(formula)

  ## explanatory wavelet variables
  wavelet_formula <- interpret.formula(formula)[["wavelet.spec"]] ## wavelet part
  if(identical(wavelet_formula, list())){
    stop("No wavelet in the formula")
  }
  variables <- as.character(lapply(wavelet_formula, "[[", "term")) ## all wavelet variables
  wavelet_factor <-  grep("NA", as.character(lapply(wavelet_formula, "[[", "by")), value=TRUE, invert=TRUE) ## by variables

  ## treatment of by variables
  if (identical(wavelet_factor, character(0))){
    datawavelet <- data[variables]
  } else {
    ## function that multiply the two covariates
    wavebyvariable <- function(data, wavelet_factor, variables_by){
      if (is.factor(data[, wavelet_factor])==FALSE){
        data[, wavelet_factor] <- as.factor(data[, wavelet_factor])
        print("by element has been changed into a factor")
      }
      M_day0 <- stackeR::createIndicators(data[wavelet_factor], wavelet_factor, prefix = paste0(substr(wavelet_factor, 1, 2), "_"))
      M_wavelet <- mapply(`*`,M_day0[, -grep(wavelet_factor, names(M_day0))], data[variables_by])
      return(M_wavelet)
    }

    getbyvariable <- function(wavelet_factor, wavelet_formula, data, variables){
      variables_by <- variables[grep(wavelet_factor, as.character(lapply(wavelet_formula, "[[", "by")))] ## wavelet variable with a by
      val <- lapply(wavelet_factor, function(x) wavebyvariable(data, x, variables_by))
      df <- do.call("cbind", val)
      # if other wavelet terms without by
      nobyvariables <- setdiff(variables, variables_by)
      if (!identical(nobyvariables, character(0))){
        datawave <- cbind(data[nobyvariables], df)
      } else {datawave <- df}
      return(datawave)
    }
    datawavelet <- do.call("cbind", lapply(wavelet_factor, function(x) getbyvariable(x, wavelet_formula, data, variables)))
  }
  return(datawavelet)
}
