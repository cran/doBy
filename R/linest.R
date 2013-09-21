## ####################################################################################################
##
## Banff, august 2013
## This is an experimental function for creating 'linear estimates'
##
## ####################################################################################################

linest<- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, df.adjust=FALSE, ...){
  UseMethod("linest")
}

linest.lmerMod <- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, df.adjust=FALSE, ...){

  bhat <- fixef( object )
  VV   <- vcov( object )

  if (df.adjust && require("pbkrtest")) {
    VVadj<- vcovAdj( object, 0 )
    ddfm <- function(kk, se) .get_ddf(VVadj, VV, kk, se * se)
    VVuse <- VVadj
  } else {
    ddfm <- function(kk, se) 1
    VVuse <- VV
  }

  KK  <- popMatrix(object, effect=effect, at=at, only.at=only.at)

  used      <- which(!is.na(bhat))
  not.used  <- which(is.na(bhat))
  bhat.used <- bhat[used]

  if (length(not.used) > 0) {
    ## cat("some are 'not.used'\n")
    null.basis <- .get_null_basis ( object ) ## Do all objects have qr slot ??
    estimable  <- .is.estimable(KK, null.basis)
  } else {
    null.basis <- NULL
    estimable  <- rep(TRUE, length(bhat.used))
  }

  res <- .do.estimation_lmerMod( KK, bhat.used, VVuse, ddfm, used, estimable, null.basis )
  cbind(res, attributes(KK)$grid)
}


.do.estimation_lmerMod <- function( KK, bhat, VV, ddfm, used, estimable, null.basis ){
  res <- matrix(NA, nrow=nrow(KK), ncol=3)
  for (ii in 1:nrow(res)){
    kk <- KK[ii,]
    if (estimable[ii]){
      kk   <- kk[used]
      est  <- sum(kk * bhat)
      se   <- sqrt(sum(kk * (VV %*% kk)))
##      print(kk); print(se)
      df2  <- ddfm(kk, se)
      res[ii,] <- c(est, se, df2)
    }
  }
  colnames(res) <- c("estimate","SE","df")
  res
}

linest.lm <- function(object, effect=NULL, at=NULL, only.at=TRUE, grid=TRUE, ...){

  bhat <- coef(object)
  VV   <- vcov(object)
  df   <- object$df.residual ## Need function for this...

  cl  <- match.call()
  KK  <- popMatrix(object, effect=effect, at=at, only.at=only.at)
  used     <- which(!is.na(bhat))
  not.used <- which(is.na(bhat))
  bhat.used     <- bhat[used]

  if (length(not.used) > 0) {
    ##cat("some are 'not.used'\n")
    null.basis <- .get_null_basis ( object ) ## Do all objects have qr slot ??
    estimable  <- .is.estimable(KK, null.basis)
  } else {
    null.basis <- NULL
    estimable  <- rep(TRUE, length(bhat.used))
  }

  res <- .do.estimation_default( KK, bhat.used, VV, df, used, estimable, null.basis )
  cbind(res, attributes(KK)$grid)

}



.do.estimation_default <- function( KK, bhat, VV, df, used, estimable, null.basis ){
  res <- matrix(NA, nrow=nrow(KK), ncol=3)
  for (ii in 1:nrow(res)){
    kk <- KK[ii,]
    if (estimable[ii]){
      kk  <- kk[used]
      est <- sum(kk * bhat)
      se  <- sqrt(sum(kk * (VV %*% kk)))
      df2 <- df
      res[ii,] <- c(est, se, df2)
    }
  }
  colnames(res) <- c("estimate","SE","df")
  res
}


## .get_ddf: Adapted from Russ Lenths 'lsmeans' package.
## Returns denom d.f. for testing lcoefs'beta = 0 where lcoefs is a vector
# VVA is result of call to VVA = vcovAdj(object, 0) in pbkrtest package
# VV is vcov(object) ## May not now be needed
# lcoefs is contrast of interest
# varlb is my already-computed value of lcoef' VV lcoef = est variance of lcoef'betahat

.get_ddf <- function(VVadj, VV, lcoefs, varlb) {

  .spur = function(U){
    sum(diag(U))
  }
  .divZero = function(x,y,tol=1e-14){
    ## ratio x/y is set to 1 if both |x| and |y| are below tol
    x.y  =  if( abs(x)<tol & abs(y)<tol) {1} else x/y
    x.y
  }

  vlb = sum(lcoefs * (VV %*% lcoefs))
  Theta = Matrix(as.numeric(outer(lcoefs,lcoefs) / vlb), nrow=length(lcoefs))
  P = attr(VVadj, "P")
  W = attr(VVadj, "W")

  A1 = A2 = 0
  ThetaVV = Theta%*%VV
  n.ggamma = length(P)
  for (ii in 1:n.ggamma) {
    for (jj in c(ii:n.ggamma)) {
      e = ifelse(ii==jj, 1, 2)
      ui = ThetaVV %*% P[[ii]] %*% VV
      uj = ThetaVV %*% P[[jj]] %*% VV
      A1 =  A1 +  e* W[ii,jj] * (.spur(ui) * .spur(uj))
      A2 =  A2 +  e* W[ii,jj] *  sum(ui * t(uj))
    }}

  ## substituted q = 1 in pbkrtest code and simplified
  B  =  (A1 + 6 * A2) / 2
  g  =  (2 * A1 - 5 * A2)  / (3 * A2)
  c1 =  g/(3 + 2 * (1 - g))
  c2 =  (1 - g) / (3 + 2 * (1 - g))
  c3 =  (3 - g) / (3 + 2 * (1 - g))
  EE =  1 + A2
  VV =  2 * (1 + B)
  EEstar  =  1/(1 - A2)
  VVstar  =  2 * ((1 + c1 * B)/((1 - c2 * B)^2  *  (1 - c3 * B)))
  V0 = 1 + c1 * B
  V1 = 1 - c2 * B
  V2 = 1 - c3 * B
  V0 = ifelse(abs(V0) < 1e-10, 0, V0)
  rho  = (.divZero(1 - A2, V1))^2 * V0/V2
  df2  =  4 + 3 / (rho - 1)
  df2
}

