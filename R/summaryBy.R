summaryBy <- 
function (formula, data = parent.frame(), id=NULL, FUN = mean, keep.names=FALSE,
          prefix=NULL,  ...) {
  
  parseIt <- function(x){
    if (class(x)=='name'){
      value <- x
    } else {
      s <- paste(x[[1]])
      value <- switch(s,
                      '+'={  c(parseIt(x[[2]]),parseIt(x[[3]]))},
                      'I'={  x[[2]]},
                      {  x})
    }
    return(value)
  }
  
  lhsString <- function(formula) {
    if (!is.null(formula))
      if (length(formula)>=3)
        unlist(strsplit(deparse(formula[[2]]),".\\+."))
  }
  rhsString <- function(formula) {
    if (!is.null(formula))
      unlist(strsplit(deparse(formula[[length(formula)]]),".\\+."))
  }

  #########################################

  lhs      <- formula[[2]]
  rhsvar   <- rhsString(formula)
  ##print(rhsvar)
  idvar    <- rhsString(id)
  datavar  <- names(data)

  lhsAtoms <- parseIt(lhs)
  lhsvar   <- lhsAtoms
  if (length(lhsvar)==1)
    lhsvar <- list(lhsvar)

  if (paste(rhsvar)[1]=='.')
    rhsvar <- NULL
  
  if ("." %in% lhsvar){
    lhsvar <- setdiff(lhsvar, ".")
    v <- setdiff(datavar, c(lhsvar, rhsvar, idvar))
    isSpecial <- rep(NA,length(v))
    for (j in 1:length(v)){
      isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
    }      
    extralhsvar<-v[!isSpecial]
    lhsvar <- union(lhsvar, extralhsvar)    
  }

  ## print(lhsvar)

  if (is.null(rhsvar)){
    rhsvar <- setdiff(datavar, c(lhsvar, idvar))
    formula <- as.formula(paste(formula[[2]], "~", paste (rhsvar, collapse='+')))
  }
    
#   cat("status:\n")
#   cat("lhsvar     :", paste(lhsvar),"\n")
#   cat("rhsvar     :", paste(rhsvar),"\n")
#   cat("idvar      :", paste(idvar),"\n")
#   print(formula)


  transformData <- sapply(paste(lhsvar), function(x)eval(parse(text=x), data))

  ## Function names
  if (!is.list(FUN)) 
    fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
  else
    fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
  ##cat("fun.names  :", paste(fun.names), "\n")
  if (!is.list(FUN)) 
    FUN <- list(FUN)

  ## Prefix for new variables
  if (!is.null(prefix)){
    if (length(prefix) != length(fun.names))
      stop("Length of prefix not equal to the number of functions")
    varPrefix <- prefix
  } else {
    varPrefix <- fun.names
  }

  ## Names for new variables
  if (keep.names){
    if (length(fun.names)==1)
      newNames <- lhsvar
    else {
      cat("Can not keep names of original variables because more than one function is applied \n")
      newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
    }
  } else {
    newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
  }

  newdata <- cbind(transformData, data[,c(rhsvar,idvar),drop=FALSE])

  ## Split data
  groupFormula <- formula
  groupFormula[[2]] <- NULL
  splitData <- splitBy(groupFormula, data=newdata, drop=TRUE, return.matrix=TRUE)

  ## Calculate groupwise statistics
  lhsvarvec <- paste(unlist(lhsvar)) 
  byList <- lapply(splitData, function(x){
    xr  <- x[, lhsvarvec, drop = FALSE]
    v <- NULL
    for (j in 1:length(FUN)) {        
      currFUN <- FUN[[j]]
      vf <- apply(xr, 2, currFUN, ...)
      v <- c(v,vf)
    }
  })
  val        <- as.data.frame(do.call("rbind", byList))
  names(val) <- c(newNames)

  if (!is.null(rhsvar)){
    group <- attr(terms(formula), "term.labels")  
    groupid <- attr(splitData, "groupid")

    for (j in 1:length(group)){
      grpj <- group[j]
      newclass <- class(data[, grpj])
      class(groupid[, grpj]) <- newclass
      if (newclass[1]=='factor' | newclass[1]=='ordered'){        
        levels(groupid[, grpj]) <- levels(data[, grpj])
      }
    }
    val <- cbind(groupid, val)
  }
  
  if (!is.null(idvar)){
    idList <- lapply(splitData, function(x){
      x[1, idvar, drop=FALSE]
    })
    idid <- do.call("rbind", idList)
    val <- cbind(val, idid)
  }
  return(val)
}

  ##cat("varPrefix  :", paste(varPrefix), "\n")
  ##cat("newNames   :", paste(newNames),"\n")


  ##print("transformData");  print(transformData[1:10,])

  ##print(data[1:5,c(rhsvar,idvar),drop=FALSE])
  ##print(newdata[1:5,])
