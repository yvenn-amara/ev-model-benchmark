#' SplineD
#'
#' Evaluates the design matrix for the O'Sullivan-type splines
#'
#' @param x the x vector over which the basis functions are evaluated
#' @param range.x the min and max values of x over which the basis functions are evaluated
#' (x is allowed to take values outside the interior knots).
#' @param intKnots the interior knots for the B-splines basis functions (all splines are of order 4 (cubic))
#' @param drv an integer with values between 0 and 3. The derivative of the given order \code{drv} is evaluated at the x positions. Defaults to zero.
#'
#' @return a design matrix with each column containing the evaluation of a B-splines basis function on x in canonical form
#'
#' @references
#' Wand, M.P. and Ormerod, J.T. (2008). On semiparametric regression with
#' O-Sullivan penalized splines. \emph{Australian and New Zealand Journal of Statistics}, \strong{50}, pages 179-198.
#'
#' @examples
#'
#' n <- 300
#' x<-seq(0,1,length=n)
#' range.x<-c(0,1)
#' numIntKnots <- 25
#' intKnots <- quantile(unique(x),seq(0,1,length=numIntKnots+2))[-c(1,numIntKnots+2)]
#' X <- SplineD(x,range.x=range.x,intKnots=intKnots)
#' plot(x,X[,ncol(X)],type="n",xlab="x",ylab="Basis functions", main="O-Spline basis functions for 25 equispaced knots")
#' for (i in (1:ncol(X))) lines(x,X[,i],col=i)
#' 
#' @author Anestis Antoniadis <anestisa@@gmail.com>
#' @author Yannig  Goude <yannig.goude@@edf.fr>
#' @export
#' 
SplineD <- function(x,range.x,intKnots,drv=0){
  # Controle de l'etendue de x (de range.x).
  if (!missing(range.x))
  {
    if (length(range.x)!=2) stop("range.x must be of length 2.")
    if (range.x[1]>range.x[2]) stop("range.x[1] exceeds range.x[1].")
    if (range.x[1]>min(x)) stop("range.x[1] must be <= than min(x).")
    if (range.x[2]<max(x)) stop("range.x[2] must be >= than max(x).")
  }
  
  if (drv>2) stop("splines not smooth enough for more than 2 derivatives")
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
      print("WARNING: NUMERICAL INSTABILITY ARISING\\
             FROM SPECTRAL DECOMPOSITION")
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
