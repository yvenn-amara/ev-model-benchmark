#' ZOSull
#'
#' Computes the O'Sullivan spline basis
#'
#' @param x the x observed values
#' @param range.x the min and max value over which calculated the basis
#' @param intKnots number of internal knots
#' @param drv derivative order (default 0)
#'
#' @return a matrix containing the spline basis
#'
#' @examples
#' y<-rnorm(10)
#' 
#' 
#' @author Anestis Antoniadis <anestisa@gmail.com>
#' @author Yannig  Goude <yannig.goude@edf.fr>
#' @export
#' 
ZOSull <- function(x,range.x,intKnots,drv=0){
  # Controle de l'etendue de x (de range.x).
  if (!missing(range.x))
  {
    if (length(range.x)!=2) stop("range.x  doit etre de longueur 2.")
    if (range.x[1]>range.x[2]) stop("range.x[1] depasse range.x[1].")
    if (range.x[1]>min(x)) stop("range.x[1] doit etre <= que min(x).")
    if (range.x[2]<max(x)) stop("range.x[2] doit etre >= que max(x).")
  }
  
  if (drv>2) stop("splines non assez lisses pour plus de  2 derivees")
  # On charge la librairie splines
  require(splines)
  
  # Valuers par defaut de `range.x' et `intKnots'
  
  if (missing(range.x))
    range.x <- c(1.05*min(x)-0.05*max(x),1.05*max(x)-0.05*min(x))
  
  if (missing(intKnots))
  {
    numIntKnots <- min(length(unique(x)),35)
    intKnots <- quantile(unique(x),seq(0,1,length=
                                         (numIntKnots+2))[-c(1,(numIntKnots+2))])
  }
  numIntKnots <- length(intKnots) 
  
  # Calcul de la matrice de penalisation.
  
  allKnots <- c(rep(range.x[1],4),intKnots,rep(range.x[2],4)) 
  K <- length(intKnots) ; L <- 3*(K+8)
  xtilde <- (rep(allKnots,each=3)[-c(1,(L-1),L)]+ 
               rep(allKnots,each=3)[-c(1,2,L)])/2
  wts <- rep(diff(allKnots),each=3)*rep(c(1,4,1)/6,K+7)
  Bdd <- spline.des(allKnots,xtilde,derivs=rep(2,length(xtilde)),
                    outer.ok=TRUE)$design  
  Omega     <- t(Bdd*wts)%*%Bdd     
  
  # On utilise la decomposition spectrale de Omega 
  # pour obtenir la forme canonique Z.
  
  svdOmega <- svd(Omega) 
  indsZ <- 1:(numIntKnots+2)
  UZ <- svdOmega$u[,indsZ] 
  LZ <- t(t(UZ)/sqrt(svdOmega$d[indsZ]))
  
  # Controle de stabilite.   
  
  indsX <- (numIntKnots+3):(numIntKnots+4)
  UX <- svdOmega$u[,indsX]   
  L <- cbind(UX,LZ)
  stabCheck <- t(crossprod(L,t(crossprod(L,Omega))))          
  if (sum(stabCheck^2) > 1.0001*(numIntKnots+2))
    print("ATTENTION:  INSTABILITE NUMERIQUE DUE\\
              A LA DECOMPOSITION SPECTRALE")
  # Obtenir B et post-multiplier par LZ  pour obtenir Z.
  
  B <- spline.des(allKnots,x,derivs=rep(drv,length(x)),
                  outer.ok=TRUE)$design  
  Z <- B%*%LZ
  # On ajoute `range.x' et 'intKnots' comme attributs 
  # de l'objet retourne.
  
  attr(Z,"range.x") <- range.x
  attr(Z,"intKnots") <- intKnots
  # Retour de Z avec les 2 attributs
  return(Z)
}
