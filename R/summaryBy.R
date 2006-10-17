
summaryBy <- 
function (formula, data= parent.frame() , id=NULL, FUN = mean, keep.names=FALSE,
          postfix=NULL,  p2d=FALSE, order=TRUE, ...) {

    
  parseIt <- function(x){
    ###cat("parseIt:"); print(x); print(class(x))
    if (class(x)=='name'){
      value <- x
    } else {
      s <- paste(x[[1]])
      value <- switch(s,
                      '+'={  c(parseIt(x[[2]]),parseIt(x[[3]]))},
                      'I'={  x[[2]]},
                      {  deparse(x)})
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

  trace <- 0
  lhs      <- formula[[2]]
  rhsvar   <- rhsString(formula)
  idvar    <- rhsString(id)
  datavar  <- names(data)

  cls <- lapply(data, class)

  numvar <- datavar[  cls %in% c("numeric","integer")]
  facvar <- datavar[!(cls %in% c("numeric","integer"))]
  if (trace>=1){
    cat("datavar:"); print(datavar)
    cat("numvar :"); print(numvar)
    cat("facvar :"); print(facvar)
    cat("idvar  :"); print(idvar)
  }
  
  lhsAtoms <- parseIt(lhs)
  lhsvar   <- lhsAtoms
  
  
  if (length(lhsvar)==1)
    lhsvar <- list(lhsvar)

  if (paste(rhsvar)[1]=='.')
    rhsvar <- NULL
  
  if ("." %in% lhsvar){
    lhsvar <- setdiff(lhsvar, ".")
    v <- setdiff(numvar, c(lhsvar, intersect(rhsvar,numvar), intersect(idvar, numvar)))
    isSpecial <- rep(NA,length(v))
    for (j in 1:length(v)){
      isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
    }      
    extralhsvar<-v[!isSpecial]
    lhsvar <- union(lhsvar, extralhsvar)
    if (trace>=1)
      cat("lhsvar (new) :", paste(lhsvar),"\n")
  }

  lhsstr <- paste(lhsvar, collapse='+')
  if (trace>=1)
    cat("lhsstr:", lhsstr, "\n")
  
  ## print(lhsvar)
  if (is.null(rhsvar) | "." %in% rhsvar){
    rhsvar <- setdiff(facvar, c(lhsvar, idvar))
    if (trace>=1)
      {cat(".rhsvar: "); print(rhsvar)}
  }
  #if (rhsvar=="1")
    

  rhsstr <- paste (rhsvar, collapse='+')
  str <- paste(paste(lhsstr, "~", rhsstr, collape=''))
  formula <- as.formula(str)

  ###cat("Updated formula: ");  print(formula)
  
  if (trace>=1){
    cat("status:\n")
    cat("rhsvar     :", paste(rhsvar),"\n")
    cat("idvar      :", paste(idvar),"\n")
    print(formula)
  }
  transformData <- sapply(paste(lhsvar), function(x)eval(parse(text=x), data))

### Function names
  if (!is.list(FUN)) 
    fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
  else
    fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
  ##cat("fun.names  :", paste(fun.names), "\n")
  
  if (!is.list(FUN)) 
    FUN <- list(FUN)

  
### Names for new variables
  if (keep.names){
    if (length(fun.names)>1){
      cat("Can not keep names of original variables, more than one function is applied.\n")
      keep.names <- FALSE
    }
  }

  rhsidcol <- if (rhsvar[1]=="1") idvar else c(rhsvar,idvar)
  if (!is.null(rhsidcol))
    newdata <- cbind(transformData, data[,rhsidcol,drop=FALSE])
  else{
    newdata <- as.data.frame(transformData)    
  }
  
### Split data
  groupFormula <- formula
  groupFormula[[2]] <- NULL
  splitData <- splitBy(groupFormula, data=newdata, drop=TRUE, return.matrix=FALSE)

### Calculate groupwise statistics

  lhsvarvec <- paste(unlist(lhsvar)) 
  byList <- lapply(splitData, function(x){
    xr  <- x[, lhsvarvec, drop = FALSE]
    v <- NULL
    for (j in 1:length(FUN)) {        
      currFUN <- FUN[[j]]
      vf2 <- lapply(xr, currFUN, ...)
      vf2 <- changeNames(vf2,fun.names[j],j,postfix, keep.names)
      v <- c(v,vf2)
    }
  })


  val        <- as.data.frame(do.call("rbind", byList))
  
  if (!is.null(rhsvar) && rhsvar!="1"){
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
  if (length(unique(names(val))) != length(names(val)))
    warning("dataframe contains replicate names \n", call.=FALSE)


  if (order==TRUE){
    if (rhsstr!="1")
      val <- orderBy(as.formula(paste("~", rhsstr)), data=val)
  }

  names(val) <- gsub(" ","",names(val))
  if (p2d)
    names(val) <- gsub("\\)","\.",gsub("\\(","\.",names(val)))

  
  rownames(val) <- 1:nrow(val)
  return(val)
}


changeNames <- function(vv, fname, funnum, postfix, keep.names=FALSE){

  trace <- 0
  varnames     <- names(vv)
  vectordim    <- length(vv[[1]])
  statnames    <- names(vv[[1]])

  defaultStatnames <-
    if (vectordim>1)
      defaultStatnames <- paste('stat',  1:vectordim, sep="")
    else
      'stat'

  newStatnames     <-
    if (is.null(statnames))
      rep("", vectordim)
    else
      statnames
  
  newStatnames[newStatnames==''] <- defaultStatnames[newStatnames=='']

  if (vectordim>1)
    keep.names <- FALSE

  
  if (!is.null(postfix))
    if (length(postfix) >= vectordim){
      postfix <- postfix[1:vectordim]
    } else {
      postfix <- NULL
    }

  functionORlist <- (!is.na(pmatch("function",fname)) ||!is.na(pmatch("list(",fname))) 
  if (trace>=1){
    cat("status:\n")
    cat(".fname             : ", fname,
        "\n.functionORlist    : ", functionORlist,
        "\n.keep.names        : ", keep.names,
        "\n.postfix            : ", postfix, 
        "\n.varnames          : ", varnames,
        "\n.vectordim         : ", vectordim,
        "\n.statnames         : ", statnames,
        "\n.defaultStatnames  : ", defaultStatnames,
        "\n.newStatnames      : ", newStatnames,
        "\n.functionORlist    : ", functionORlist,
        "\n")
  }


    
  if (!is.null(postfix)){
    ##vv <<- vv; postfix <<- postfix
    vv2 <- lapply(vv, "names<-", postfix);
    vv2 <- unlist(vv2)
  } else {
    if (keep.names==TRUE){
      ## vv <<- vv; varnames<<-varnames ;print(vv); print(varnames)
      vv2 <- lapply(vv, "names<-", NULL);
      vv2 <- unlist(vv2);
    } else {
      if (is.null(statnames)){
        if (functionORlist){ ## E.g. FUN=function(x)... or FUN=c(function()..., function(x)..)
          vv<- lapply(vv, "names<-", newStatnames)
        } else { ## E.g. FUN=c(mean,var)
          if (vectordim>1){
            newStatnames  <- paste(fname, 1:vectordim, sep="")
            vv<- lapply(vv, "names<-", newStatnames);
          } else { 
            newStatnames <- paste(fname,  sep="")
            vv<- lapply(vv, "names<-", newStatnames);
          }    
        } 
      } else {    
        vv <- lapply(vv, "names<-", newStatnames);    
      }
      vv2          <- unlist(vv)
    }
  }
  return(vv2)
}
  


