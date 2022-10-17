#' createPast
#'
#' create a new covariate called name.i, i is the lag, which is a lagged version of the covariate name
#'
#' @param Data the dataset, containing observations of covariates 
#' @param name name of the covariate
#' @param past a vecteur of lags
#' 
#' @examples
#' to complete
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export
#'

createPast = function(Data, name, past = 1:24){
  for (i in past) {
    name.i = paste(name, i, sep = '.')
    if ((name.i %in% colnames(Data))) {
      Data = Data[, !(names(Data) %in% name.i)] 
    }
    if (i > 0){
      x = data.frame(c(Data[1:i, name], Data[1:(nrow(Data) - i), name]))
    } else {
      x = data.frame(Data[, name])
    }
    names(x) = name.i
    Data = data.frame(Data, x)
  }
  return(Data)
}


