#' ZDaub
#'
#' Compute the Daubechy wavelet basis
#'
#' @param x the x value over which calculated the basis
#' @param range.x the min and max value over which calculated the basis
#' @param numLevels the level of the decomposition
#' @param filterNumber ...
#' @param resolution ...
#' @param family the family of the wavelet basis
#'
#' @return a matrix containing the wavelet basis
#'
#' @examples
#' y<-rnorm(10)
#' 
#' 
#' @author Anestis Antoniadis <anestisa@gmail.com>
#' @author Yannig  Goude <yannig.goude@edf.fr>
#' @export





ZDaub <- function(x,range.x=range(x),numLevels=6,filterNumber=5,
                  resolution=16384,family="DaubExPhase")
{
  
  
  # On charge la librairie wavethresh de Nason:
  
  #require(wavethresh)
  
  # Controle de l'etendue de x (de range.x).
  
  if (any(x<range.x[1])|any(x>range.x[2]))
    stop("Toutes les abcisses devraient dans range.x.")
  
  # On s'assure que le nombre de niveaux est raisonnabe.
  
  if (!any(numLevels==(1:10)))
    stop("Le nombre de niveaux devrait etre entre 2 et 10.")
  
  # On s'assure que la resolution fine est raisonnabe.
  
  if (!any(resolution==(2^(10:20))))
    stop("La resolution devrait etre une puissance de 2, avec
         une puissance entre 10 et 20.")
  # On transforme x afin qu'il varie dans[0,1] et on obtient
  # les variables utiles pour l'interpolation lineaire:
  
  xUnit <- (x - range.x[1])/(range.x[2] - range.x[1])
  xUres <- xUnit*resolution
  fXuRes <- floor(xUres)
  
  # On choisit le filtre et la famille d'ondelettes  
  
  #    family <- "DaubExPhase"
  family <- family
  K <- 2^numLevels - 1
  
  # Creation d'un objet transformee en ondelette
  
  wdObj <- wavethresh::wd(rep(0,resolution),filter.number=filterNumber,
              family=family)
  
  Z <- matrix(0,length(x),K)
  for (k in 1:K)
  {
    # creation des fonction de bases sur un plan
    # equidistant de resolution fine 
    
    putCobj <- wavethresh::putC(wdObj,level=0,v=0)
    putCobj$D <- putCobj$D*0
    putCobj$D[resolution-k] <- 1
    
    # kieme colonne de Z par interpolation lineaire
    
    wtVec <- xUres - fXuRes
    wvVec <- wavethresh::wr(putCobj)
    wvVec <- c(wvVec,rep(wvVec[length(wvVec)],2))
    Z[,k] <- sqrt(resolution)*((1 - wtVec)*wvVec[fXuRes+1]
                               + wtVec*wvVec[fXuRes+2])
  }
  
  # Creation des indices de colonne pour un ordre de 
  #  gauche a droite pour chaque niveau:
  
  newColInds <- 1
  for (ell in 1:(numLevels-1))
    newColInds <- c(newColInds,(2^(ell+1)-1):(2^(ell)))
  
  Z <- Z[,newColInds]
  
  return(Z)
}
