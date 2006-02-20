orderBy <- function (formula, data, na.last = TRUE, decreasing = FALSE){
  dietox <- data
  form <- unlist(strsplit(paste(formula)[2],"\\+"))
  form <- gsub(" ","",form)
  
  dodo<-dietox[,form,drop=FALSE]
  
  z <- NULL
  for (j in 1:ncol(dodo)){
    z <- c(z, list(dodo[,j]))
  }
  if (any(diff(sapply(z, length)) != 0))
    stop("argument lengths differ")
  ans <- sapply(z, is.na)
  ok <- if (is.matrix(ans)){
    !apply(ans, 1, any)
  } else {
    !any(ans)
  }
  
  if (all(!ok))
    return(integer(0))
  z[[1]][!ok] <- NA
  ans <- do.call("order", c(z, decreasing = decreasing))
  keep <- seq(along = ok)[ok]
  ord<-ans[ans %in% keep]
  return(data[ord,])

}
