

LSmeans <- function(object, effect=NULL, at=NULL, level=0.95,...){
    UseMethod("LSmeans")
}

LSmeans.default <- function(object, effect=NULL, at=NULL, level=0.95,...){
    K      <- .getK(object, effect=effect, at=at)
    out <- linest(object, K, level=level, ...)
    out
}

LSmeans.lmerMod <- function(object, effect=NULL, at=NULL, level=0.95, adjust.df=TRUE, ...){
    K      <- .getK(object, effect=effect, at=at)
    out <- linest(object, K, level=level, adjust.df=adjust.df, ...)
    out
}

popMeans <- LSmeans
popMeans.default <- LSmeans.default
popMeans.lmerMod <- LSmeans.lmerMod
