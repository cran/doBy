.modelFormula <- function(form)
{
    if (class(form) != "formula" || length(form) != 3)
        stop("formula must be a two-sided formula object")
    rhs <- form[[3]]
    if (class(rhs) != "call" || rhs[[1]] != as.symbol('|'))
        stop("rhs of formula must be a conditioning expression")
    form[[3]] <- rhs[[2]]
	groups <- rhs[[3]]
	grpFormula <- as.formula(paste("~", deparse(groups)))
    list(model = form, groups = groups, groupFormula=grpFormula)
}

lmBy <- function(formula, data, id=NULL, ...){
  cl   <- match.call()
  mff  <- .modelFormula(formula)
  wdl  <- splitBy(mff$groupFormula, data=data)
  mm   <- lapply(wdl, function(wd) {lm(mff$model, data=wd, ...)})
  mm$call <- cl
  
  if (!is.null(id)){
    id.vars <- unique(c(all.vars(id), all.vars(mff$groupFormula)))
  } else {
    id.vars <- all.vars(mff$groupFormula)
  }
  id.data <- do.call(rbind, lapply(wdl, function(wd) {wd[1,id.vars,drop=FALSE]}))
  attr(mm,  "dataList") <- wdl
  attr(mm,  "idData") <- id.data	
  
  class(mm) <- "lmBy"
  mm
}


print.lmBy <- function(x, ...){
  print(x$call)
  return(invisible(x))
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



