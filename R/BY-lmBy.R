##########################################################################
#' @title List of lm objects with a common model
#' @description The data is split into strata according to the levels
#'     of the grouping factors and individual lm fits are obtained for
#'     each stratum.
#' @name by-lmby
##########################################################################
#' @aliases lmBy coef.lmBy coef.summary_lmBy summary.lmBy fitted.lmBy
#'     residuals.lmBy getBy
#' @param formula A linear model formula object of the form y ~ x1 +
#'     ... + xn | g1 + ... + gm.  In the formula object, y represents
#'     the response, x1,...,xn the covariates, and the grouping
#'     factors specifying the partitioning of the data according to
#'     which different lm fits should be performed.
#' @param data A dataframe
#' @param id A formula describing variables from data which are to be
#'     available also in the output.
#' @param \dots Additional arguments passed on to \code{lm()}.
#' @return A list of lm fits.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @keywords models
#' @examples
#' 
#' bb <- lmBy(1 / uptake ~ log(conc) | Treatment, data=CO2)
#' 
#' coef(bb)
#' 
#' fitted(bb)
#' residuals(bb)
#' 
#' summary(bb)
#' coef(summary(bb))
#' coef(summary(bb), simplify=TRUE)
#'
#' ## A more modern alternative based on tidyverse end broom
#'
#' ## if (require(tidyverse) && require(broom)){
#' ##  gg <- CO2 %>% group_by(Treatment)
#' ##  gg %>% do(broom::tidy(lm(1 / uptake ~ log(conc), data=.)))
#' ##}
#' 

#' @export
#' @rdname by-lmby
lmBy <- function(formula, data, id=NULL, ...){
  cl   <- match.call()
  mff  <- parseGroupFormula(formula)
  groupData  <- splitBy(mff$groupFormula, data=data)

  mmm <- mff$model
  mm  <- lapply(groupData, function(wd) {
    zzz<-lm(mmm, data=wd, ...)
    zzz$call[[2]]<- mmm
    zzz
  })

  if (!is.null(id)){
    id.vars <- unique(c(all.vars(id), all.vars(mff$groupFormula)))
  } else {
    id.vars <- all.vars(mff$groupFormula)
  }
  id.data <- do.call(rbind, lapply(groupData, function(wd) {wd[1,id.vars,drop=FALSE]}))

  attr(mm,  "call")     <- cl
  attr(mm,  "dataList") <- groupData
  attr(mm,  "idData")   <- id.data	
  
  class(mm) <- "lmBy"
  mm
}

#' @export
print.lmBy <- function(x, ...){
  ##lapply(c(x), print)
  print(c(x))
  return(invisible(x))
}

#' @export
summary.lmBy <- function(object, ...){
  res <- lapply(object, summary)
  class(res) <- "summary_lmBy"
  res
}

#' @export
print.summary_lmBy <- function(x, ...){
  lapply(x, print)
  return(invisible(x))
}

#' @export
coef.summary_lmBy <- function(object, simplify=FALSE, ...){
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


#' @export
getBy <- function(object, name=c()){
  if (missing(name)) 
    stop("'name' must not be missing")
  if (inherits(object, "lmBy"))
  {
      ii <- match(name, c("dataList", "idData"))
      if (is.na(ii))
          stop(sprintf("%s not available", name))
      attr(object,name)	
  }
  
}

#' @export
coef.lmBy <- function(object, augment=FALSE, ...){
  ans <- do.call(rbind, lapply(object, coef))
  if (augment){
    ans <- cbind(ans, getBy(object, "idData"))
  }
  ans
}

#' @export
fitted.lmBy <- function(object, augment=FALSE,...){
  ans <- lapply(object, fitted)
  if (augment) {
    ans <- mapply(function(a,b){data.frame(.fit=a,b)}, ans, getBy(object, "dataList"),
                  SIMPLIFY=FALSE)
  }
  ans
}

#' @export
residuals.lmBy <- function(object, augment=FALSE,...){
  ans <- lapply(object, residuals)
  if (augment) {
    ans <- mapply(function(a,b){data.frame(.fit=a,b)}, ans, getBy(object, "dataList"),
                  SIMPLIFY=FALSE)
  }
  ans
}



