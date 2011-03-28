popMeans <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", ...){
  UseMethod("popMeans")
}

popMeans.lm <- function(object, effect=NULL, at=NULL, only.at=TRUE, engine="esticon", ...){
  cl  <- match.call()
  mm  <- popMatrix(object, effect=effect, at=at, only.at=only.at)
  ans <- do.call(engine, list(object, mm,...))

  if (engine=="esticon"){
    attributes(ans)[c("grid","at")] <- attributes(mm)[c("grid","at")]
    attr(ans,"X") <- mm
    attr(ans,"call") <- cl
    class(ans)    <- c("popMeans", "conMeans", "data.frame")
  }
  ans
}

lsMeans    <- popMeans
lsMeans.lm <- popMeans.lm

linMeans <- function(object, at=NULL, engine="esticon", ...){
  UseMethod("linMeans")
}

linMeans.lm <- function(object, at=NULL, engine="esticon", ...){
  cl  <- match.call()
  mm  <- linMatrix(object, at=at)
  ans <- do.call(engine, list(object, mm,...))

  if (engine=="esticon"){
    attributes(ans)[c("grid","at")] <- attributes(mm)[c("grid","at")]
    attr(ans,"X") <- mm
    attr(ans,"call") <- cl
    class(ans)    <- c("linMeans", "conMeans", "data.frame")
  }
  ans
}


summary.conMeans <- function(object,...){
  print(object)

  cat("Call:\n")
  print(attr(object,"call"))
  cat("Contrast matrix:\n")
  summary(attr(object,"X"))
}
