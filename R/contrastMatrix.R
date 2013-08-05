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
  nam     <- names(xlev)
  nn      <- match(nam, names(at))
  nn      <- nn[!is.na(nn)]
  at.fact <- at[nn]
  xlev[names(at[nn])]   <- at.fact
  attr(xlev, "at.fact") <- at.fact
  xlev
}

.set_convals <- function(xlev, covariateVal){
  nam     <- names(xlev)
  nn      <- match(nam, names(covariateVal))
  nn      <- nn[!is.na(nn)]
  con.con <- covariateVal[nn]
  xlev[names(covariateVal[nn])] <- con.con
  xlev
}

.get_vartypes <- function(object){
  tt  <- terms(object)
  tt  <- delete.response(tt)
  att <- attributes(tt)
  rhs.terms <- rownames(att$factors)[rowSums(att$factors)>0]
  rhs.class <- att$dataClass[match(rhs.terms, names(att$dataClass))]
  nums      <- rhs.terms[rhs.class=="numeric"]
  fact      <- rhs.terms[rhs.class=="factor"]
  list(numeric=nums, factor=fact)
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

.get_covariate_ave <- function(object, at=NULL, tt=terms(object)){
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

.get_null_basis <- function( object ){
  tR = t(qr.R(object$qr))
  rank = object$qr$rank
  for (i in (rank + 1):nrow(tR)) tR[i, i] = 1
  null.basis = qr.resid(qr(tR[, 1:rank]), tR[, -(1:rank)])
  if (!is.matrix(null.basis)) 
    null.basis = matrix(null.basis, ncol = 1)
  null.basis[object$qr$pivot, ] = null.basis
  null.basis
}


popMatrix <- function(object, effect=NULL, at=NULL, only.at=TRUE){
  Terms   <- delete.response(terms(object))
  xlev    <- .get_xlevels(object)
  cov.ave <- .get_covariate_ave(object,at)
  vartype <- .get_vartypes(object)
  ##str(list(xlev=xlev, tt=tt, cov.ave=cov.ave, vartype=vartype))
  
  if (is.null(effect)){
    at.factor <- at[intersect(vartype$factor, names(at))]
    xxx       <- if(length(at.factor)>0)
      at.factor
  } else {
    xlev    <- .set_xlevels( xlev, at=at )
    at.fact <- names(attr(xlev, "at.fact"))
    effect  <- setdiff( effect, at.fact )
    xxx     <- xlev[ c( effect, at.fact ) ]
  }
  
  if (is.null(xxx)){ ## No 'effect' and no 'at'; hence just a global average...
    newdata <- expand.grid(xlev)
    newdata[,names(cov.ave)] <- cov.ave   
    mf  <- model.frame(Terms, newdata, xlev = .get_xlevels(object))
    X   <- model.matrix(Terms, mf, contrasts.arg = .get_contrasts(object))
    res <- apply(X, 2, mean)
    res <- do.call(rbind, list(res))
    attr(res,"at") <- at[intersect(vartype$numeric, names(at))]
  } else {
    grid  <- expand.grid(xxx)
    grid  <- as.data.frame(lapply(grid, as.character),stringsAsFactors=FALSE)
    #cat("grid:\n"); print(grid)
    res <- list()
    for (ii in 1:nrow(grid)){
      conf    <- grid[ii,,drop=FALSE]
      xlev2   <- .set_xlevels(xlev,  at=conf) #cat("xlev2 (which defines the grid):\n"); str(xlev2)
      newdata <- expand.grid(xlev2)
      newdata[,names(cov.ave)] <- cov.ave   
      mm        <- .getX(object, newdata)
      X         <- apply(mm,2,mean)
      res[[ii]] <- X
    }

    grid[, names(cov.ave) ] <- cov.ave
    res                <- do.call(rbind, res)
    attr(res,"grid")   <- grid
    attr(res,"at")     <- at
  }
  
  class(res) <- c("popMatrix", "conMatrix", "matrix")
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







linMatrix <- function(object, at){
  tt 	   <- terms(object)
  Terms    <- delete.response(tt)
  xlev     <- .get_xlevels(object)
  ccc      <- .get_covariate_ave(object, at)
  
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
