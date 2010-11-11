recodevar <- function(x, src, tgt){
  stop("The 'recodevar' function is deprecated; please use 'recodeVar'")
}

recodeVar <- function(x, src, tgt, default=NULL, keep.na=TRUE){

  if (length(src)!=length(tgt)){
    stop("length of src not equal to length of tgt")
  }
  mtc <- lapply(src, function(zzz){which(x %in% zzz)})
  idx <- seq_along(x)
  unmatch <- setdiff(idx, unlist(mtc))
  
  val <- x#rep(NA,length(x))
  
  for (ii in 1:length(tgt))
    val[mtc[[ii]]] <- tgt[[ii]]

  if (!is.null(default)){
    if (keep.na){
      iii <- intersect(which(!is.na(x)), unmatch)
      val[iii] <- default
    } else {
      val[unmatch] <- default
    }
  }
  
  if (is.factor(x))
    val <- as.factor(val)
  val

  return(val)
}


## recodeVar <- function(x, src, tgt, default=NA, keep.na=TRUE){
##   if (length(src)!=length(tgt)){
##     stop("length of src not equal to length of tgt")
##   }
##   mtc <- lapply(src, function(zzz){which(x %in% zzz)})
##   idx <- seq_along(x)
##   unmatch <- setdiff(idx, unlist(mtc))

##   val <- rep(NA,length(x))

##   for (ii in 1:length(tgt))
##     val[mtc[[ii]]] <- tgt[[ii]]

##   if (keep.na){  
##     iii <- intersect(which(!is.na(x)), unmatch)
##     val[iii] <- default
##   } else {
##     val[unmatch] <- default
##   }
  
##   if (is.factor(x))
##     val <- as.factor(val)
##   val 

##   return(val)
## }




## recodevar <- function(var,src,tgt){
##   mtc <- lapply(src, function(x){which(var %in% x)})
##   val <- rep(NA,length(var))

##   for (i in 1:length(mtc))
##     val[mtc[[i]]] <- tgt[[i]]

##   rst <- which(is.na(val))
##   val[rst] <- tgt[[length(tgt)]]
##   if (is.factor(var))
##     val <- as.factor(val)
##   return(val)
## }
