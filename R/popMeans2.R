.pop <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", grid=TRUE, ...){

  bhat <- coef(object)
  VV   <- vcov(object)
  df   <- object$df.residual ## Need function for this...
  
  cl  <- match.call()
  KK  <- popMatrix(object, effect=effect, at=at, only.at=only.at) ##;print(KK)
  used     <- which(!is.na(bhat))
  not.used <- which(is.na(bhat))
  bhat     <- bhat[used]

  if (length(not.used) > 0) {
    null.basis <- .get_null_basis ( object ) ## Do all objects have qr slot ??
    estimable <- .is.estimable(KK, null.basis)    
  } else {
    null.basis <- NULL
    estimable <- rep(TRUE, length(bhat))    
  }

  res <- .do.estimation( KK, bhat, VV, df, used, estimable, null.basis )
  cbind(res, attributes(KK)$grid)
  
}

.do.estimation <- function( KK, bhat, VV, df, used, estimable, null.basis ){
  res <- matrix(NA, nrow=nrow(KK), ncol=3)
  for (ii in 1:nrow(res)){
    kk <- KK[ii,]
    if (estimable[ii]){
      kk = kk[used]
      est = sum(kk * bhat)
      se = sqrt(sum(kk * (VV %*% kk)))
      df2 = df
      res[ii,] <- c(est, se, df2)
    }
  }
  colnames(res) <- c("estimate","SE","df")
  res
}


.is.estimable <- function(KK, null.basis){
  res <- rep.int(NA, nrow(KK))
  for (ii in 1:nrow(KK)){
    kk <- KK[ii, ]
    res[ii] <- all(abs(apply(null.basis, 2, function(x) sum(kk * x))) < 1e-04)
  }
  res
}

## do.est = function(k, bhat, VV, df, used, null.basis) {
##   est = se =  NA
##   #df = NA
##   estimable = TRUE
##   if (!is.null(null.basis)) {
##     estimable = all(abs(apply(null.basis, 2, function(x) sum(k * 
##       x))) < 1e-04)

##   }
  
##   if (estimable) {
##     k = k[used]
##     est = sum(k * bhat)
##     se = sqrt(sum(k * (VV %*% k)))
##     #if (!is.null(ddfm)) 
##     #  df = ddfm(k, se)
##   }
  
##   res <- c(estimate = est, SE = se, df = df)
##   res
## }

