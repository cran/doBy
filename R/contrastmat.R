### ####################################
###
### Create contrast matrices
###
### ####################################

.get_xlevels <- function(obj){
	UseMethod(".get_xlevels")
}

.get_xlevels.default <- function(obj){
	obj$xlevels
}

.get_xlevels.mer <- function(obj){
	ans <- lapply(obj@frame,levels)
	ans <- ans[unlist(lapply(ans, length))>0]

        nn <- names(attr(terms(obj), "dataClasses"))
        ii <- match(names(ans), nn)
        ii <- ii[!is.na(ii)]
        ans <- ans[nn[ii]]       
        ans
}

.get_contrasts <- function(obj){
  UseMethod(".get_contrasts")
}

.get_contrasts.default <- function(obj){
	obj$contrasts
}

.get_contrasts.mer <- function(obj){
  attr(obj@X,"contrasts")
}

.set_xlevels <- function(xlev, at){
  nam    <- names(xlev)
  nn <- match(nam, names(at))
  nn <- nn[!is.na(nn)]
  at.fact <- at[nn]
  xlev[names(at[nn])]  <- at.fact
  attr(xlev, "at.fact") <- at.fact
  xlev
}

.set_convals <- function(xlev, covariateVal){
  nam    <- names(xlev)
  nn <- match(nam, names(covariateVal))
  nn <- nn[!is.na(nn)]
  con.con <- covariateVal[nn]
  xlev[names(covariateVal[nn])] <- con.con
  xlev
}

.get_vartypes <- function(object){
  tt <- terms(object)
  tt  <- delete.response(tt)
  att <- attributes(tt)
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  nums      <- rhs.terms[rhs.class=="numeric"]
  fact      <- rhs.terms[rhs.class=="factor"]
  list(numeric=nums, factor=fact)
}


linMatrix <- function(object, at){
  tt 	   <- terms(object)
  Terms    <- delete.response(tt)
  xlev  <- .get_xlevels(object)
  ccc   <- .covariateAve(object, at)
  
  ## popMatrix and linMatrix only differ here
  at.grid  <- expand.grid(at)
  at.grid  <- as.data.frame(lapply(at.grid, as.character),stringsAsFactors=FALSE)
  ## !!!
  #dcat("at.grid:\n"); print(at.grid)
  
  res <- list()
  for (ii in 1:nrow(at.grid)){
    conf  <- at.grid[ii,,drop=FALSE]
                                        #cat("conf:\n"); print(conf)
    ## "set" factor values specified in "at.grid"
    xlev2 <- .set_xlevels(xlev,  at=conf)
                                        #cat("xlev2 - 1:\n"); print(xlev2)
    
    newdata <- expand.grid(xlev2)
    ## "set" covariate values specified in "at"
    newdata[,names(ccc)] <- ccc 
    mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
    X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
    ans <- apply(X,2,mean)
    res[[ii]] <- ans
  }

  grid <- at.grid
  grid[names(at)]  <- at
  grid[names(ccc)] <- ccc
  res <- do.call(rbind, res)  
  attr(res,"grid")   <- grid
  attr(res,"at")     <- at
  class(res) <- c("linMatrix", "conMatrix","matrix")
  res
}


popMatrix <- function(object, effect=NULL, at=NULL, only.at=TRUE){
  tt <- terms(object)
  Terms   <- delete.response(tt)
  xlev    <- .get_xlevels(object)
  ccc     <- .covariateAve(object,at)
  vartype <- .get_vartypes(object)


##   cat("INPUT: effect:\n"); str(effect)
##   cat("INPUT: at:\n"); str(at)
##   cat("---------------------------\n")
  xlev   <- .get_xlevels(object)

  if (is.null(effect)){
    at.factor <- at[intersect(vartype$factor, names(at))]
    xxx       <- if(length(at.factor)>0)
      at.factor
  } else {
    xlev   <- .set_xlevels(xlev, at=at)
    at.fact <- names(attr(xlev, "at.fact"))
    effect <- setdiff(effect, at.fact)
    xxx    <- xlev[c(effect,at.fact)]
  }

#  print(ccc)
#  print(xxx)
  

  #print(xxx)
  if (is.null(xxx)){
    ## No 'effect' and no 'at'; just to a global average.
    newdata <- expand.grid(xlev)
    newdata[,names(ccc)] <- ccc   
    mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
    X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
    res <- apply(X,2,mean)
    res <- do.call(rbind, list(res))
    attr(res,"at") <- at[intersect(vartype$numeric, names(at))]
  } else {
    eff.grid  <- expand.grid(xxx)
    eff.grid  <- as.data.frame(lapply(eff.grid, as.character),stringsAsFactors=FALSE)
    #cat("eff.grid:\n"); print(eff.grid)
    res <- list()
    for (ii in 1:nrow(eff.grid)){
      conf  <- eff.grid[ii,,drop=FALSE]
      xlev2 <- .set_xlevels(xlev,  at=conf)
      #cat("xlev2 (which defines the grid):\n"); str(xlev2)
      newdata <- expand.grid(xlev2)
      newdata[,names(ccc)] <- ccc   

      #print(newdata)
      mm   <- .getX(object, newdata)
      X    <- apply(mm,2,mean)
      res[[ii]] <- X
    }

    res <- do.call(rbind, res)
#    print(eff.grid)
    uuu <- at[intersect(vartype$numeric, names(at))]
#    print(uuu)
#    print(vartype)
#    print(at)
#    print(ccc)
    #eff.grid[,names(ccc)] <- at[intersect(vartype$numeric, names(at))]
    eff.grid[,names(ccc)] <- ccc
    attr(res,"grid") <- eff.grid
    attr(res,"at") <- at
  }
  class(res) <- c("popMatrix", "conMatrix","matrix")
  res 
}

print.conMatrix <- function(x,...){
  attr(x,"grid") <- attr(x,"at") <- attr(x,"class")<-NULL
  print.default(x)
  invisible(x)
}

summary.conMatrix <- function(object, ...){
	print(object)
	cat("grid:\n")
	str(attr(object,"grid"))
	cat("at:\n")
	str(attr(object,"at"))
}


.getX <- function(object, newdata){
  tt <- terms(object)
  Terms  <- delete.response(tt)
  mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
  X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
  attr(X,"assign")<-NULL
  attr(X, "contrasts") <- NULL
  X
}

.covariateAve <- function(object, at=NULL, tt=terms(object)){
  tt  <- delete.response(tt)
  att <- attributes(tt)
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  nums      <- rhs.terms[rhs.class=="numeric"]

  ans  <- lapply(model.frame(object)[,nums, drop=FALSE], mean) 
  
  nn <- match(names(ans), names(at))
  nn <- nn[!is.na(nn)]
  at.num <- at[nn]
  ans[names(at[nn])] <- at.num
  attr(ans, "at.num") <- at.num
  ans
}









## popMatrix <- function(object, effect=NULL, at=NULL, only.at=TRUE){
##   tt <- terms(object)
##   Terms  <- delete.response(tt)
##   xlev   <- .get_xlevels(object)
##   ccc    <- .covariateAve(object,at)
##   cat("INPUT: effect:\n"); str(effect)
##   cat("INPUT: at:\n"); str(at)


## ##   cat("======= Ignore 'effect' ========\n")
## ##   xlev   <- .get_xlevels(object)
## ##   xxx    <- at
  
## ##   eff.grid  <- expand.grid(xxx)
## ##   eff.grid  <- as.data.frame(lapply(eff.grid, as.character),stringsAsFactors=FALSE)
## ##   cat("eff.grid:\n"); print(eff.grid)
## ##   res <- list()
## ##   for (ii in 1:nrow(eff.grid)){
## ##     conf  <- eff.grid[ii,,drop=FALSE]
## ##     xlev2 <- .set_xlevels(xlev,  at=conf)
## ##     cat("xlev2 (which defines the grid):\n"); str(xlev2)
## ##     newdata <- expand.grid(xlev2)
## ##     newdata[,names(ccc)] <- ccc   
## ##     mm  <- .getX(object, newdata)
## ##     X   <- apply(mm,2,mean)
## ##     res[[ii]] <- X
## ##     ##print(mm)
## ##   }
## ##   print(do.call(rbind, res))
## ##   cat("======= DONE Ignore 'effect' ========\n")

  
##   cat("======= USE 'effect' ========\n")
##   xlev   <- .get_xlevels(object)

##   if (is.null(effect)){
##     xxx    <- at
##   } else {
##     xlev   <- .set_xlevels(xlev, at=at)
##     xxx    <- xlev[effect]
##   }

##   print(xxx)
##   eff.grid  <- expand.grid(xxx)
##   eff.grid  <- as.data.frame(lapply(eff.grid, as.character),stringsAsFactors=FALSE)
##   cat("eff.grid:\n"); print(eff.grid)
##   res <- list()
##   for (ii in 1:nrow(eff.grid)){
##     conf  <- eff.grid[ii,,drop=FALSE]
##     xlev2 <- .set_xlevels(xlev,  at=conf)
##     cat("xlev2 (which defines the grid):\n"); str(xlev2)
##     newdata <- expand.grid(xlev2)
##     newdata[,names(ccc)] <- ccc   
##     mm  <- .getX(object, newdata)
##     X   <- apply(mm,2,mean)
##     res[[ii]] <- X
##     ##print(mm)
##   }
##   print(do.call(rbind, res))
##   cat("======= DONE USE 'effect' ========\n")  

## ##   xlev      <- .set_xlevels(xlev, at=at)   

  
## ##   if (!is.null(effect)){
## ##     cat("effect != NULL\n")
## ##     xxx       <- xlev[effect]
## ##     eff.grid  <- expand.grid(xxx)
## ##     eff.grid  <- as.data.frame(lapply(eff.grid, as.character),stringsAsFactors=FALSE)
## ##     cat("eff.grid:\n"); print(eff.grid)
## ##     ## !!!
    
## ##     res <- list()
## ##     for (ii in 1:nrow(eff.grid)){
## ##       conf <- eff.grid[ii,,drop=FALSE]
## ##       xlev2 <- .set_xlevels(xlev,  at=conf)
## ##       cat("xlev2 (which defines the grid - before at is set):\n"); dput(xlev2)
## ##       ## "set" factor values specified in "at"
## ##       #xlev2 <- .set_xlevels(xlev2, at=at)   
## ##       #cat("xlev2 (which defines the grid - after at is set):\n"); dput(xlev2)
## ##       newdata <- expand.grid(xlev2)
## ##       ##print(newdata)
## ##       ## "set" covariate values specified in "at"
## ##       newdata[,names(ccc)] <- ccc   
## ##       mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
## ##       X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
## ##       ans <- apply(X,2,mean)
## ##       res[[ii]] <- ans
## ##     }
    
## ##     grid <- eff.grid
## ##     grid[names(at)]  <- at
## ##     grid[names(ccc)] <- ccc
## ##     res <- do.call(rbind, res)  
## ##     attr(res,"grid")   <- grid
## ##     attr(res,"at")     <- at
## ##   } else {
## ##     cat("CASE: effect == NULL\n")
## ##     cat("xlev - 3:\n"); dput(xlev)
## ##     newdata <- expand.grid(xlev)
## ##     newdata[,names(ccc)] <- ccc   
## ##     mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
## ##     X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
## ##     res <- apply(X,2,mean)
## ##     res <- do.call(rbind, list(res))
## ##     attr(res,"at")     <- at
## ##   }

## ##   res



## }



##   if (length(ddd)>0)
##     cat(sprintf("Notice: levels not set for these factors: %s\n", toString(ddd)))



## .make.conmat <- function(mm, conf, at=NULL, covariateVal=covariateAve(mm,at)){
##   tt 	 <- terms(mm)
##   Terms  <- delete.response(tt)
##   ccc    <- covariateAve(mm, at=at)
  
##   xlev   <- .get_xlevels(mm)
  
##   xlev[names(conf)] <- conf           # "set" factor values from variables over which an average is taken.
##   xlev <- .set_convals(xlev, cov=ccc) # "set" covariate values specified in "at"

##   xlev <- .set_xlevels(xlev, at=at)   # "set" factor values specified in "at"
##   at.fact <- attr(xlev, "at.fact")

  
##   newdata <- expand.grid(xlev)
##   newdata[,names(covariateVal)] <- covariateVal

  
##   mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(mm))
##   X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(mm))
##   ans <- apply(X,2,mean)
  
##   attr(ans, "at.fact") <- at.fact
##   attr(ans, "at.num")  <- attr(covariateVal, "at.num")
##   ans
## }

## lsmMatrixOld <- function(mm, effect, at=NULL){
##   tt <- terms(mm)
##   Terms  <- delete.response(tt)
##   xlev   <- .get_xlevels(mm)
##   ccc <- covariateAve(mm,at)
  
##   xxx    <- xlev[effect]
##   at.grid   <- expand.grid(xxx)
##   at.grid   <- as.data.frame(lapply(at.grid, as.character),stringsAsFactors=FALSE)

##   ans <- list()
##   for (ii in 1:nrow(at.grid)){
##     conf <- at.grid[ii,,drop=FALSE]
##     ans[[ii]] <- .make.conmat(mm, conf, at=at, covariateVal=ccc)
##   }
  
##   ans2 <- do.call(rbind, ans)  
##   zzz <- at.grid
##   zzz[,names(attr(ans[[1]],"at.num"))]  <- attr(ans[[1]],"at.num")
##   zzz[,names(attr(ans[[1]],"at.fact"))] <- attr(ans[[1]],"at.fact")
##   attr(ans2,"vars") 	<- zzz
##   ans2
## }
