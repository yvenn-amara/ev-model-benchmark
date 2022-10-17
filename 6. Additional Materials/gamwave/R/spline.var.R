#' spline.var
#'
#' Creation of the dataset for a spline additive model
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
#' formula <- "y ~ s(x1)" ; spline.var(formula, data)
#' formula <- "y ~ s(x1, by=fac)" ; spline.var(formula, data)
#'
#' @export
#'
spline.var <- function(formula, data){

  ## data must be a dataframe
  data <- as.data.frame(data)

  ## formula
  formula <- as.formula(formula)

  ## explanatory spline variables
  spline_formula <- interpret.formula(formula)[["smooth.spec"]]
  if(identical(spline_formula, list())){
    stop("No splines in the formula")
  }
  variables <- as.character(lapply(spline_formula, "[[", "term")) ## all spline variables
  spline_factor <-  grep("NA", as.character(lapply(spline_formula, "[[", "by")), value=TRUE, invert=TRUE) ## by variables
  
  ## treatment of by variables
  if (identical(spline_factor, character(0))){
    dataspline <- data[variables]
  } else {
    ## function that multiply the two covariates
    splinebyvariable <- function(data, spline_factor, variables_by){
      if (is.factor(data[, spline_factor])==FALSE){
        data[, spline_factor] <- as.factor(data[, spline_factor])
        print("by element has been changed into a factor")
      }
      M_day0 <- stackeR::createIndicators(data[spline_factor], spline_factor, prefix = paste0(substr(spline_factor, 1, 2), "_"))
      M_spline <- mapply(`*`,M_day0[, -grep(spline_factor, names(M_day0))], data[variables_by])
      return(M_spline)
    }

    getbyvariable <- function(spline_factor, spline_formula, data, variables){
      variables_by <- variables[grep(spline_factor, as.character(lapply(spline_formula, "[[", "by")))] ## wavelet variable with a by
      val <- lapply(spline_factor, function(x) splinebyvariable(data, x, variables_by))
      df <- do.call("cbind", val)
      # if other wavelet terms without by
      nobyvariables <- setdiff(variables, variables_by)
      if (!identical(nobyvariables, character(0))){
        datawave <- cbind(data[nobyvariables], df)
      } else {datawave <- df}
      return(datawave)
    }
    dataspline <- do.call("cbind", lapply(spline_factor, function(x) getbyvariable(x, spline_formula, data, variables)))
  }
  return(dataspline)
}
