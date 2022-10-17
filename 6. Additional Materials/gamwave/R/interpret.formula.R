#' interpret.formula
#'
#' interpret a partial linear additive model formula (with wavelets and/or splines) of the generic form:
#' y~x0+x1+x3*x4+s(x5)+s(x6,x7)+w(x8)+w(x9, by=fac) + ...
#'
#' @param formula a model formula
#' @return Information about the model:
#' \item{pf} {a model formula for the parametric part}
#' \item{smooth.spec} {a list of descriptors for each spline element: smooth.spec}
#' \item{wavelet.spec} {a list of descriptors for each wavelet element: wavelet.spec}
#' \item{response} {the response covariate of the model}
#'
#' @examples
#' f1 <- function(x) return(3*sin(2*pi*x^3)) 
#' f2 <- function(x) return(15* x *exp(-x**2))
#' n <- 200
#' set.seed(1)
#' x1 <- runif(n)
#' x1<-(x1-min(x1))/(max(x1)-min(x1))
#' x2 <- runif(n)
#' x2<-(x2-min(x2))/(max(x2)-min(x2))
#' g1<-f1(x1)-mean(f1(x1))
#' g2<-f2(x2)-mean(f2(x2))
#' y <- g1+g2+rnorm(n)*.5
#' A<-cbind(x1,x2)
#' numlev=floor(log2(n))-1
#' fac<-c(rep(1,n/2),rep(2,n/2))
#' data <- cbind(y, x1, x2, fac)
#' formula <- "y ~ w(x1) + s(x2, k=10)" 
#' interpret.formula(formula)
#' formula <- "y ~ w(x1, by=fac)" 
#' interpret.formula(formula)
#'
#' @export
interpret.formula <- function(formula){

  ## formula must be of type formula
  formula <- as.formula(formula)

  p.env <- environment(formula) # environment of formula
  tf <- terms.formula(formula, specials=c("s", "te", "ti", "t2","w"))
  terms <- attr(tf, "term.labels") # labels of the model terms
  nt <- length(terms) # how many terms?

  if (attr(tf,"response") > 0) {  # start the replacement formulae
    response <- as.character(attr(tf,"variables")[2])
  } else {
    response <- NULL
  }

  sp <- attr(tf,"specials")$s     # array of indices of smooth terms
  tp <- attr(tf,"specials")$te    # indices of tensor product terms
  tip <- attr(tf,"specials")$ti   # indices of tensor product pure interaction terms
  t2p <- attr(tf,"specials")$t2   # indices of type 2 tensor product terms
  wp <- attr(tf,"specials")$w   # indices of wavelet terms

  ## have to translate sp, tp, tip, t2p so that they relate to terms,
  ## rather than elements of the formula...
  vtab <- attr(tf,"factors") # cross tabulation of vars to terms
  if (length(sp)>0) for (i in 1:length(sp)) {
    ind <- (1:nt)[as.logical(vtab[sp[i],])]
    sp[i] <- ind # the term that smooth relates to
  }
  if (length(tp)>0) for (i in 1:length(tp)) {
    ind <- (1:nt)[as.logical(vtab[tp[i],])]
    tp[i] <- ind # the term that smooth relates to
  }
  if (length(tip)>0) for (i in 1:length(tip)) {
    ind <- (1:nt)[as.logical(vtab[tip[i],])]
    tip[i] <- ind # the term that smooth relates to
  }
  if (length(t2p)>0) for (i in 1:length(t2p)) {
    ind <- (1:nt)[as.logical(vtab[t2p[i],])]
    t2p[i] <- ind # the term that smooth relates to
  } ## re-referencing is complete

  if (length(wp)>0) for (i in 1:length(wp)) {
    ind <- (1:nt)[as.logical(vtab[wp[i],])]
    wp[i] <- ind # the term that wavelet relates to
  }

  k <- kt <- kti <- kt2 <- ks <- kp <- kw <- 1 # counters for terms in the 2 formulae
  len.sp <- length(sp)
  len.tp <- length(tp)
  len.tip <- length(tip)
  len.t2p <- length(t2p)
  len.wp <- length(wp)
  ns <- len.sp + len.tp + len.tip + len.t2p + len.wp # number of smooths + wavelet
  pav <- av <- rep("",0)
  smooth.spec <- list()
  wavelet.spec <- list()
  mgcvat <- "package:mgcv" %in% search() ## is mgcv in search path?
  if (nt) for (i in 1:nt) { # work through all terms
    if (k <= ns && ((ks<=len.sp && sp[ks]==i)||(kt<=len.tp && tp[kt]==i)||
                    (kti<=len.tip && tip[kti]==i)||(kt2<=len.t2p && t2p[kt2]==i)
                    ||(kw<=len.wp && wp[kw]==i))) { # it's a smooth
      ## have to evaluate in the environment of the formula or you can't find variables
      ## supplied as smooth arguments, e.g. k <- 5;gam(y~s(x,k=k)), fails,
      ## but if you don't specify namespace of mgcv then stuff like
      ## loadNamespace('mgcv'); k <- 10; mgcv::interpret.gam(y~s(x,k=k)) fails (can't find s)
      ## eval(parse(text=terms[i]),envir=p.env,enclos=loadNamespace('mgcv')) fails??
      ## following may supply namespace of mgcv explicitly if not on search path...
      if (mgcvat) st <- eval(parse(text=terms[i]), envir=p.env) else {
        st <- try(eval(parse(text=terms[i]), envir=p.env), silent=TRUE)
        if (inherits(st, "try-error"))
          st <- eval(parse(text=terms[i]), enclos=p.env, envir=loadNamespace('gamwave'))
        ## la commande parse permet de transforme une chaine de caractère en une expression, la fonction eval
        ## permet ensuite de l'évaluer et sort toutes les caractéristiques de l'élement
      }
      if(attr(st, "class")== "wavelet.spec"){
        wavelet.spec[[kw]] <- st
      } else { iks <- max(ks, kt, kti, kt2)
      smooth.spec[[iks]] <- st}
      if (ks<=len.sp&&sp[ks]==i) ks <- ks + 1 else # counts s() terms
        if (kt<=len.tp&&tp[kt]==i) kt <- kt + 1 else # counts te() terms
          if (kti<=len.tip&&tip[kti]==i) kti <- kti + 1 else # counts ti() terms
            if (kt2<=len.t2p&&t2p[kt2]==i) kt2 <- kt2 + 1  else # counts t2() terms
              if (kw<=len.wp&&wp[kw]==i) kw <- kw + 1 # counts s() terms
              k <- k + 1      # counts smooth terms
    } else {          # parametric
      av[kp] <- terms[i] ## element kp on rhs of parametric
      kp <- kp+1    # counts parametric terms
    }
  }

  pf <- paste(response,"~",paste(av,collapse=" + "))
  if (attr(tf,"intercept")==0) {
    pf <- paste(pf,"-1",sep="")
    if (kp>1) pfok <- 1 else pfok <- 0
  } else {
    pfok <- 1;if (kp==1) {
      pf <- paste(pf,"1");
    }
  }

  ret <- list(pf=as.formula(pf,p.env), smooth.spec=smooth.spec,
              wavelet.spec=wavelet.spec, response=response)
  class(ret) <- "split.gam.formula"
  return(ret)
}
