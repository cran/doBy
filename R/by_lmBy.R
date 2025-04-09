##########################################################################
#'
#' @title List of lm objects with a common model
#' @description The data is split into strata according to the levels
#'     of the grouping factors and individual lm fits are obtained for
#'     each stratum.
#' @name by-lmby
#' 
##########################################################################
#'
#' @aliases lmBy coef.lmBy coef.summary_lmBy summary.lmBy fitted.lmBy
#'     residuals.lmBy getBy
#' @param formula. A linear model formula object of the form
#'     `y ~ x1 + ... + xn | g1 + ... + gm`.  In the formula object, `y` represents
#'     the response, `x1, ..., xn` the covariates, and the grouping
#'     factors specifying the partitioning of the data according to
#'     which different lm fits should be performed.
#' @param data. A dataframe
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
#' coef(bb)
#' 
#' fitted(bb)
#' residuals(bb)
#' 
#' summary(bb)
#' coef(summary(bb))
#' coef(summary(bb), simplify=TRUE)

#' @export
#' @rdname by-lmby
lm_by <- function (data., formula., id=NULL, ...) {
    cl <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula.
    cl[[3]] <- data.
    names(cl)[2:3] <- c("formula.", "data.")
    cl[[1]] <- as.name("lmBy")
    eval(cl)
}

#' @export
#' @rdname by-lmby
lmBy <- function(formula., data., id=NULL, ...){
  cl   <- match.call()
  mff  <- parseGroupFormula(formula.)
  group_data  <- splitBy(mff$groupFormula, data=data., omit=FALSE)
    
  form <- mff$model
  
  model_list  <- lapply(group_data,
                function(wd) {
                    zzz <- lm(form, data=wd, ...)
                    zzz$call[[2]] <- form
                    zzz
                })

  if (!is.null(id)){
      id.vars <- unique(c(all.vars(id),
                          all.vars(mff$groupFormula)))
  } else {
    id.vars <- all.vars(mff$groupFormula)
  }


  id.data <- do.call(rbind, lapply(group_data,
                                   function(wd) {
                                       wd[1, id.vars, drop=FALSE]
                                   }))
  

  attr(model_list,  "call")     <- cl
  attr(model_list,  "dataList") <- group_data
  attr(model_list,  "idData")   <- id.data	
  
  class(model_list) <- "lmBy"
  model_list
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
  invisible(x)
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
sigma.lmBy <- function(object, ...){
    out <- sapply(object, sigma, ...)
    return(out)
}



#' @export
fitted.lmBy <- function(object, augment=FALSE, ...){
  ans <- lapply(object, fitted)
  if (augment) {
      ans <- mapply(function(a,b){
          data.frame(.fit=a,b)
      }, ans, getBy(object, "dataList"), SIMPLIFY=FALSE)
  }
  ans
}

#' @export
residuals.lmBy <- function(object, augment=FALSE, ...){
  ans <- lapply(object, residuals)
  if (augment) {
      ans <- mapply(function(a,b){
          data.frame(.fit=a,b)
      }, ans, getBy(object, "dataList"), SIMPLIFY=FALSE)
  }
  ans
}



