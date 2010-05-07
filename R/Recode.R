
recodevar <- function(var,src,tgt){
  mtc <- lapply(src, function(x){which(var %in% x)})
  val <- rep(NA,length(var))

  for (i in 1:length(mtc))
    val[mtc[[i]]] <- tgt[[i]]

  rst <- which(is.na(val))
  val[rst] <- tgt[[length(tgt)]]
  if (is.factor(var))
    val <- as.factor(val)
  return(val)
}
