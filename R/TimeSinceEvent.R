
timeSinceEvent <- function(yvar, tvar=seq_along(yvar)){
  
  if (!(is.numeric(yvar) | is.logical(yvar))){
    stop("yvar must be either numeric or logical")
  }
  
  event.idx <- which(yvar==1)
  if (length(event.idx)==0){
    return(NULL)
  }
  
  res <- vector("list", length(event.idx))
  for (kk in seq_along(event.idx)){
    jj <- event.idx[kk]
    res[[kk]] <- tvar-tvar[jj]
  }
  
  res<-do.call(rbind, res)
  abs.tse<-apply(abs(res),2,min)
  
  dtse <- c(-1,diff(abs.tse))
  dtse[dtse==0 & yvar ==0] <- -1
  dtse[dtse<0] <- -1
  dtse[dtse>0] <- 1
  
  sign.tse <- abs.tse * dtse
  
  
  
  run <- rep(NA, length(abs.tse))
  
  curr.state <- 1
  run[1] <- curr.state
  for (jj in 2:(length(run)-1)){
    if (sign.tse[jj] <= sign.tse[jj-1]){
      curr.state <- curr.state + 1
    } 
    run[jj] <- curr.state
  }
  run[length(run)] <- curr.state
  
  
  ans <- cbind(data.frame(yvar=yvar, tvar=tvar), abs.tse, sign.tse, run)
  return(ans)
}


#yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0)
#tse<- timeSinceEvent(yvar)
#plot(sign.tse~tvar, data=tse, type="b")
#grid()
#rug(tse$tvar[tse$yvar==1], col=4,lwd=4)
#points(scale(tse$run), col=tse$run,lwd=2)
#lines(abs.tse+.2~tvar, data=tse, type="b",col=3)
#
#


  




