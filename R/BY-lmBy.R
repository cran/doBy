
lmBy <- function(formula, data, id=NULL, ...){
  cl   <- match.call()
  mff  <- parseGroupFormula(formula)
  wdl  <- splitBy(mff$groupFormula, data=data)
  mm   <- lapply(wdl, function(wd) {lm(mff$model, data=wd, ...)})

  if (!is.null(id)){
    id.vars <- unique(c(all.vars(id), all.vars(mff$groupFormula)))
  } else {
    id.vars <- all.vars(mff$groupFormula)
  }
  id.data <- do.call(rbind, lapply(wdl, function(wd) {wd[1,id.vars,drop=FALSE]}))

  attr(mm,  "call")     <- cl
  attr(mm,  "dataList") <- wdl
  attr(mm,  "idData")   <- id.data	
  
  class(mm) <- "lmBy"
  mm
}


print.lmBy <- function(x, ...){
  ##lapply(c(x), print)
  print(c(x))
  return(invisible(x))
}


summary.lmBy <- function(object, ...){
  res <- lapply(object, summary)
  class(res) <- "summary.lmBy"
  res
}

print.summary.lmBy <- function(x, ...){
  lapply(x, print)
  return(invisible(x))
}

coef.summary.lmBy <- function(object, simplify=FALSE, ...){
  ans <- lapply(object, coef)
  if (simplify){
    cc <- do.call(rbind, ans)
    cn <- colnames(cc)
    rn <- rownames(cc)

    
    nn <- names(ans)
    rn <- rownames(ans[[1]])
    ff <- factor(rep(nn, each=length(rn)))
    
    rownames(cc) <- NULL
    ans <- data.frame(ff, rn, as.data.frame(cc))
    colnames(ans) <- c("stratum", "parameter", cn)
  }
  ans
}




getBy <- function(object, name=c()){
  if (missing(name)) 
    stop("'name' must not be missing")
  switch(class(object),
         "lmBy"={
           ii <- match(name, c("dataList","idData"))
           if (is.na(ii))
             stop(sprintf("%s not available", name))
           attr(object,name)	
         })
  
}

coef.lmBy <- function(object, augment=FALSE, ...){
  ans <- do.call(rbind, lapply(object, coef))
  if (augment){
    ans <- cbind(ans, getBy(object,"idData"))
  }
  ans
}


fitted.lmBy <- function(object, augment=FALSE,...){
  ans <- lapply(object, fitted)
  if (augment) {
    ans <- mapply(function(a,b){data.frame(.fit=a,b)}, ans, getBy(object, "dataList"),
                  SIMPLIFY=FALSE)
  }
  ans
}


residuals.lmBy <- function(object, augment=FALSE,...){
  ans <- lapply(object, residuals)
  if (augment) {
    ans <- mapply(function(a,b){data.frame(.fit=a,b)}, ans, getBy(object, "dataList"),
                  SIMPLIFY=FALSE)
  }
  ans
}



