
summaryBy <- 
  function (formula, data=parent.frame(), id=NULL, FUN=mean, keep.names=FALSE,
            p2d=FALSE, order=TRUE, full.dimension=FALSE, ...)
  {    
    debug.info <- 0
    data.var  <- names(data)
    
    lhs      <- formula[[2]]
    lh.var   <- .lhsParse(lhs)
    
    rh.var   <- .rhsVar(formula)
    id.var   <- .rhsVar(id)

    cls <- lapply(data, class)
    
    num.var <- data.var[  cls %in% c("numeric","integer")]
    fac.var <- data.var[!(cls %in% c("numeric","integer"))]

    if (debug.info>=1){
      cat("status:\n")
      cat("  data.var : "); print(data.var)
      cat("  num.var  : "); print(num.var)
      cat("  fac.var  : "); print(fac.var)
      cat("  id.var   : "); print(id.var)
    }
    
   
    if (length(lh.var)==1)
      lh.var <- list(lh.var)
    
    if (paste(rh.var)[1]=='.')
      rh.var <- NULL
    
    if ("." %in% lh.var){
      lh.var <- setdiff(lh.var, ".")
      v  <- setdiff(num.var, c(lh.var, intersect(rh.var,num.var), intersect(id.var, num.var)))
      isSpecial <- rep(NA,length(v))
      for (j in 1:length(v)){
        isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
      }      
      extralh.var<-v[!isSpecial]
      lh.var <- union(lh.var, extralh.var)
      if (debug.info>=1)
        cat("lh.var (new) :", paste(lh.var),"\n")
    }
    
    lh.string <- paste(lh.var, collapse='+')
    if (debug.info>=1)
      cat("lh.string:", lh.string, "\n")

### If no rh.var, set rh.var to those factors in the dataframe which
### do not appear as lh.var or id.var
###
    if (is.null(rh.var) | "." %in% rh.var){
      rh.var <- setdiff(fac.var, c(lh.var, id.var))
      
      if (length(rh.var)==0){
        stop("No factors are identified for grouping...")
      }
          
      if (debug.info>=1)
        {cat(".rh.var: "); print(rh.var)}
    }
    
    rhs.string  <- paste (rh.var, collapse='+')
    str     <- paste(paste(lh.string, "~", rhs.string, collape=''))
    formula <- as.formula(str)
    
    if (debug.info>=1){
      cat("status:\n")
      cat("  rh.var     : ", paste(rh.var),"\n")
      cat("  id.var     : ", paste(id.var),"\n")
      cat("  formula    : ");print(formula)
    }
    
### Function names
###
    
    if (!is.list(FUN)) 
      fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
    else
      fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
    
    if (!is.list(FUN)) 
      FUN <- list(FUN)


### Get rh data
###

    rh.trivial <- identical(rh.var, "1") ## Is the rhs just 1 ??
    
    if (rh.trivial){
      rh.string <- rep.int(1, nrow(data))
    } else {
    
      rh.data <- data[, rh.var, drop=FALSE]
      rh.string <- apply(rh.data, 1, paste, collapse="@")
      names(rh.string) <- NULL

      #rh.string <<- rh.string
      
      rh.unique <- unique(rh.string)
      rh.idx    <- match(rh.unique, rh.string)
      if (debug.info>=1){
        cat("  rh.var (1) :", paste(rh.var),"\n")
        cat("  rh.string  : "); print (rh.string)
        cat("  rh.idx     : "); print (rh.idx)
      }
    }
    
### Get string for id.vars
###
    if (!is.null(id.var)){
      id.data <-  data[,id.var,drop=FALSE]
      id.string <-  apply(id.data, 1, paste, collapse="@")
      if (debug.info>=1){
        cat("  id.string : "); print(id.string)
      }
      id.data <- id.data[rh.idx,,drop=FALSE]
    }
    
### Get lh data
###
    
    lh.var <- paste(unlist(lh.var))    
    lh.data <- do.call(cbind,lapply(paste(lh.var), function(x)eval(parse(text=x), data)))
    colnames(lh.data) <- lh.var

    
### Calculate groupwise statistics
###
    ans <- NULL
    rh.string.factor <- factor(rh.string, levels=unique(rh.string)) ## This is important
    #print(rh.string.factor)
    for (ff in 1:length(FUN)) {  ## loop over functions
      for (vv in 1:length(lh.var)) {  ## loop over variables
        currFUN <- FUN[[ff]]
        ##         zzz <- tapply(lh.data[,lh.var[vv]], rh.string,
        ##                       function(x){  currFUN(x,...)},
        ##                       simplify=FALSE)
        zzz <- tapply(lh.data[,lh.var[vv]], rh.string.factor,
                      function(x){  currFUN(x,...)},
                      simplify=FALSE)

        zzz  <- do.call(rbind, zzz)
        ans  <- cbind(ans, zzz)
      }
    }

    
### Names for new variables
###  
    if (keep.names){
      if (ncol(ans)==length(lh.var)){
        newnames <- lh.var
      } else {
        keep.names <- FALSE
      }
    }

    ## Dim of response (per variable on LHS)
    dimr <- (ncol(ans))/length(lh.var) 
    oldnames <- colnames(ans)

    if (is.null(oldnames))
      hasNames <- 0
    else {
      hasNames <- 1*(prod(nchar(oldnames))>0)
    }
    
    if (!keep.names){
      if (hasNames>0){
        funNames <- colnames(ans)[1:dimr]         
        newnames <- unlist(lapply(lh.var, function(v){paste(v, funNames, sep='.')}))

      } else {
        if (length(fun.names) != dimr){
          fun.names <- paste("FUN", 1:dimr,sep='')
          newnames <- unlist(lapply(lh.var, function(v){paste(v, fun.names, sep='.')}))
        } else {
          newnames <- unlist(lapply(fun.names, function(x) paste(lh.var, x, sep='.')))
        }
        if (length(newnames)!=ncol(ans)){
          fun.names <- paste(fun.names, 1:dimr, sep=".")
          newnames <- unlist(lapply(fun.names, function(x) paste(lh.var, x, sep='.')))
        }
      }
    }

    colnames(ans) <- newnames
    ans <- as.data.frame(ans)
        
### Pad the rhs data to the result
###
    if (!rh.trivial){ 
      ##       print("ans:");print(ans)
      ##       print("rh.idx:");print(rh.idx)
      ##       print("rh.data:");print(rh.data)
      ##       print(rh.data[rh.idx,,drop=FALSE])
      ans <- cbind(rh.data[rh.idx,,drop=FALSE], ans)
                                        #print("ans (with rh.data):");print(ans)
    }

### Pad id.data to result
###
    if (!is.null(id.var)){
      ans <- cbind(ans, id.data)
                                        #print("ans (pad):");print(ans)
    }

    if (full.dimension){
      rrr <-as.numeric(rh.string.factor)
      ans <- ans[rrr,]
    }
    
### Order the result by the rhs
###
    if (order==TRUE){
      if (!rh.trivial)
        ans <- orderBy(as.formula(paste("~", rhs.string)), data=ans)
                                        #print("ans (ordered):");print(ans)
    }

### Replace '('s and ')'s with '.'s
###
    if (p2d)
      names(ans) <-  gsub("\\)","\\.", gsub("\\(","\\.",names(ans)))

    rownames(ans) <- 1:nrow(ans)

    if (length(unique(names(ans))) != length(names(ans)))
      warning("dataframe contains replicate names \n", call.=FALSE)


    
    return(ans)
  }


### -------------------------------------------------------------------




.lhsParse <- function(x){
  ##cat(".lhsParse:"); print(x); print(class(x))
  if (class(x)=='name'){
    value <- x
  } else {
    s <- paste(x[[1]])
    value <- switch(s,
                    '+'={  c(.lhsParse(x[[2]]),.lhsParse(x[[3]]))},
                    'I'={  x[[2]]},
                    {  deparse(x)})
  }
  return(value)
}



.lhsVar <- function(formula) {
  if (!is.null(formula))
    if (length(formula)>=3){
      .xxx. <- formula[[2]]
      unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))
    }
}
.rhsVar <- function(formula) {
  if (!is.null(formula)){
    .xxx. <- formula[[length(formula)]]
    unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))
  }
}










## .summaryByBAK <- 
##   function (formula, data= parent.frame() , id=NULL, FUN = mean, keep.names=FALSE,
##             ##postfix=NULL,
##             p2d=FALSE, order=TRUE, ...)
##   {    
##     details <- 1
##     lhs      <- formula[[2]]
##     rhsvar   <- .rhsVar(formula)
##     idvar    <- .rhsVar(id)
##     datavar  <- names(data)
    
##     cls <- lapply(data, class)
    

##     numvar <- datavar[  cls %in% c("numeric","integer")]
##     facvar <- datavar[!(cls %in% c("numeric","integer"))]
##     if (details>=1){
##       cat("status:\n")
##       cat("  datavar: "); print(datavar)
##       cat("  numvar : "); print(numvar)
##       cat("  facvar : "); print(facvar)
##       cat("  idvar  : "); print(idvar)
##     }
    
##     lhsvar <- .lhsParse(lhs)
    
##     if (length(lhsvar)==1)
##       lhsvar <- list(lhsvar)
    
##     if (paste(rhsvar)[1]=='.')
##       rhsvar <- NULL
    
##     if ("." %in% lhsvar){
##       lhsvar <- setdiff(lhsvar, ".")
##       v  <- setdiff(numvar, c(lhsvar, intersect(rhsvar,numvar), intersect(idvar, numvar)))
##       isSpecial <- rep(NA,length(v))
##       for (j in 1:length(v)){
##         isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
##       }      
##       extralhsvar<-v[!isSpecial]
##       lhsvar <- union(lhsvar, extralhsvar)
##       if (details>=1)
##         cat("lhsvar (new) :", paste(lhsvar),"\n")
##     }
    
##     lhsstr <- paste(lhsvar, collapse='+')
##     if (details>=1)
##       cat("lhsstr:", lhsstr, "\n")
    
##     if (is.null(rhsvar) | "." %in% rhsvar){
##       rhsvar <- setdiff(facvar, c(lhsvar, idvar))
      
##       if (length(rhsvar)==0){
##         stop("No factors are identified for grouping...")
##       }
          
##       if (details>=1)
##         {cat(".rhsvar: "); print(rhsvar)}
##     }
    
##     rhsstr  <- paste (rhsvar, collapse='+')
##     str     <- paste(paste(lhsstr, "~", rhsstr, collape=''))
##     formula <- as.formula(str)
    
##     if (details>=1){
##       cat("status:\n")
##       cat("  rhsvar     :", paste(rhsvar),"\n")
##       cat("  idvar      :", paste(idvar),"\n")
##       cat("  formula    : ");print(formula)
##     }
    
## ### Function names
## ###
    
##     if (!is.list(FUN)) 
##       fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
##     else
##       fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
    
##     if (!is.list(FUN)) 
##       FUN <- list(FUN)

##     zzz <<- rhsvar

##     cat("  rhsvar (1)    :", paste(rhsvar),"\n")
    
##     rhsvar <- if (rhsvar[1]=="1") idvar else c(rhsvar,idvar)

##     cat("  rhsvar (2)    :", paste(rhsvar),"\n")

##     lhsvar <- paste(unlist(lhsvar))
    
##     rhsdata <- data[,rhsvar,drop=FALSE]
##     rhsfact <- apply(rhsdata, 1, paste, collapse="@")
##     names(rhsfact)<-NULL
##     rhsunique <- unique(rhsfact)

##     lhsdata <- do.call(cbind,lapply(paste(lhsvar), function(x)eval(parse(text=x), data)))
##     colnames(lhsdata) <- lhsvar
    
## ### Calculate groupwise statistics
## ###

##     df <- NULL
##     for (ff in 1:length(FUN)) {  ## loop over functions
##       for (vv in 1:length(lhsvar)) {  ## loop over variables
##         currFUN <- FUN[[ff]]
##         zzz <- tapply(lhsdata[,lhsvar[vv]], rhsfact, function(x){
##           currFUN(x,...)
##         }, simplify=FALSE)
##         zzz <- do.call(rbind, zzz)
##         df  <- cbind(df, zzz)
##       }
##     }

## ### Names for new variables
## ###  

##     if (keep.names){
##       if (ncol(df)==length(lhsvar)){
##         newnames <- lhsvar
##       } else {
##         keep.names <- FALSE
##       }
##     }

##     dimr <- (ncol(df))/length(lhsvar) ## Dim of response (per variable on LHS)
##     oldnames <- colnames(df)

##     if (is.null(oldnames))
##       hasNames <- 0
##     else {
##       hasNames <- 1*(prod(nchar(oldnames))>0)
##     }
    
##     if (!keep.names){
##       if (hasNames>0){
##         funNames <- colnames(df)[1:dimr]         
##         newnames <- unlist(lapply(lhsvar, function(v){paste(v, funNames, sep='.')}))

##       } else {
##         if (length(fun.names) != dimr){
##           fun.names <- paste("FUN", 1:dimr,sep='')
##           newnames <- unlist(lapply(lhsvar, function(v){paste(v, fun.names, sep='.')}))
##         } else {
##           newnames <- unlist(lapply(fun.names, function(x) paste(lhsvar, x, sep='.')))
##         }
##         if (length(newnames)!=ncol(df)){
##           fun.names <- paste(fun.names, 1:dimr, sep=".")
##           newnames <- unlist(lapply(fun.names, function(x) paste(lhsvar, x, sep='.')))
##         }
##       }
##     }

##     colnames(df) <- newnames
##     df <- as.data.frame(df)
    
##     rowid <- tapply(1:length(rhsfact), rhsfact, function(x){x[1]})
##     df <- cbind(rhsdata[rowid,,drop=FALSE], df)
    
##     if (length(unique(names(df))) != length(names(df)))
##       warning("dataframe contains replicate names \n", call.=FALSE)

##     if (order==TRUE){
##       if (rhsstr!="1")
##         df <- orderBy(as.formula(paste("~", rhsstr)), data=df)
##     }
##     if (p2d)
##       names(df) <-  gsub("\\)","\\.", gsub("\\(","\\.",names(df)))

##     rownames(df) <- 1:nrow(df)
##     return(df)

##   }


## ### -----------------------------------------------------------------------






























##     lhsString <- function(formula) {
##       if (!is.null(formula))
##         if (length(formula)>=3){
##           .xxx. <- formula[[2]]
##           ##unlist(strsplit(deparse(.xxx.),".\\+."))
##           ##unlist(paste(strsplit(deparse(.xxx.), collapse=""),".\\+."))
##           ##unlist(strsplit(paste(deparse(.xxx.), collapse=""),".\\+."))
##           unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))

##         }
##     }
##     rhsString <- function(formula) {
##       if (!is.null(formula)){
##         .xxx. <- formula[[length(formula)]]
##         ##unlist(strsplit(deparse(.xxx.),".\\+."))
##         ##unlist(strsplit(paste(deparse(.xxx.),collapse=""),".\\+."))
##         ##unlist(strsplit(paste(deparse(.xxx.), collapse=""),".\\+."))
##         unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))

##       }
##     }



## .summaryByOLD <- 
##   function (formula, data= parent.frame() , id=NULL, FUN = mean, keep.names=FALSE,
##             postfix=NULL,  p2d=FALSE, order=TRUE, ...) {
    
    
##     parseIt <- function(x){
##       ##cat("parseIt:"); print(x); print(class(x))
##       if (class(x)=='name'){
##         value <- x
##       } else {
##         s <- paste(x[[1]])
##         value <- switch(s,
##                         '+'={  c(parseIt(x[[2]]),parseIt(x[[3]]))},
##                         'I'={  x[[2]]},
##                         {  deparse(x)})
##       }
##       return(value)
##     }
    
##     lhsString <- function(formula) {
##       if (!is.null(formula))
##         if (length(formula)>=3)
##           unlist(strsplit(deparse(formula[[2]]),".\\+."))
##     }
##     rhsString <- function(formula) {
##       if (!is.null(formula))
##         unlist(strsplit(deparse(formula[[length(formula)]]),".\\+."))
##     }
    
##     ## #######################################
    
##     trace <- 0
##     lhs      <- formula[[2]]
##     rhsvar   <- rhsString(formula)
##     idvar    <- rhsString(id)
##     datavar  <- names(data)
    
##     cls <- lapply(data, class)
    
##     numvar <- datavar[  cls %in% c("numeric","integer")]
##     facvar <- datavar[!(cls %in% c("numeric","integer"))]
##     if (trace>=1){
##       cat("datavar:"); print(datavar)
##       cat("numvar :"); print(numvar)
##       cat("facvar :"); print(facvar)
##       cat("idvar  :"); print(idvar)
##     }
    
##     lhsAtoms <- parseIt(lhs)
##     lhsvar   <- lhsAtoms
    
##     if (length(lhsvar)==1)
##       lhsvar <- list(lhsvar)
    
##     if (paste(rhsvar)[1]=='.')
##       rhsvar <- NULL
    
##     if ("." %in% lhsvar){
##       lhsvar <- setdiff(lhsvar, ".")
##       v  <- setdiff(numvar, c(lhsvar, intersect(rhsvar,numvar), intersect(idvar, numvar)))
##       isSpecial <- rep(NA,length(v))
##       for (j in 1:length(v)){
##         isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
##       }      
##       extralhsvar<-v[!isSpecial]
##       lhsvar <- union(lhsvar, extralhsvar)
##       if (trace>=1)
##         cat("lhsvar (new) :", paste(lhsvar),"\n")
##     }
    
##     lhsstr <- paste(lhsvar, collapse='+')
##     if (trace>=1)
##       cat("lhsstr:", lhsstr, "\n")
    
##     if (is.null(rhsvar) | "." %in% rhsvar){
##       rhsvar <- setdiff(facvar, c(lhsvar, idvar))
      
##       if (length(rhsvar)==0){
##         stop("No factors are identified for grouping...")
##       }
      
##       ##
##       ##print(facvar); print(rhsvar)
##       ##rhsvar <- setdiff(datavar, c(lhsvar, idvar))
      
##       if (trace>=1)
##         {cat(".rhsvar: "); print(rhsvar)}
##     }
    
##     rhsstr <- paste (rhsvar, collapse='+')
##     str <- paste(paste(lhsstr, "~", rhsstr, collape=''))
##     formula <- as.formula(str)
    
##     ##cat("Updated formula: ");  print(formula)
    
##     if (trace>=1){
##       cat("status:\n")
##       cat("rhsvar     :", paste(rhsvar),"\n")
##       cat("idvar      :", paste(idvar),"\n")
##       print(formula)
##     }
##     transformData <- sapply(paste(lhsvar), function(x)eval(parse(text=x), data))
    
## ### Function names
## ###
##     if (!is.list(FUN)) 
##       fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
##     else
##       fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
##     ##cat("fun.names  :", paste(fun.names), "\n")
    
##     if (!is.list(FUN)) 
##       FUN <- list(FUN)
    
    
## ### Names for new variables
## ###  
##     if (keep.names){
##       if (length(fun.names)>1){
##         cat("Can not keep names of original variables, more than one function is applied.\n")
##         keep.names <- FALSE
##       }
##     }
    
##     rhsidcol <- if (rhsvar[1]=="1") idvar else c(rhsvar,idvar)
##     if (!is.null(rhsidcol))
##       newdata <- cbind(transformData, data[,rhsidcol,drop=FALSE])
##     else{
##       newdata <- as.data.frame(transformData)    
##     }
    
## ### Split data
## ###
##     groupFormula <- formula
##     groupFormula[[2]] <- NULL
##     splitData <- splitBy(groupFormula, data=newdata, drop=TRUE, return.matrix=FALSE)
    
    
    
## ### Calculate groupwise statistics
## ###
    
##   lhsvarvec <- paste(unlist(lhsvar)) 
##     byList <- lapply(splitData, function(x){
##       xr  <- x[, lhsvarvec, drop = FALSE]
##       v <- NULL
##       for (j in 1:length(FUN)) {        
##         currFUN <- FUN[[j]]
##         ##print(currFUN)
##         ##cf <<- currFUN; xr <<- xr
##         vf2 <- lapply(xr, currFUN, ...)
##         ##print("aaaaa")
##         vf2 <- changeNames(vf2,fun.names[j],j,postfix[j], keep.names)
##         ##print("bbbbb")
##         v <- c(v,vf2)
##       }
##     })
    
    
    
##     val  <- as.data.frame(do.call("rbind", byList))
    
##     if (!is.null(rhsvar) && rhsvar!="1"){
##       group <- attr(terms(formula), "term.labels")  
##       groupid <- attr(splitData, "groupid")
      
##       for (j in 1:length(group)){
##         grpj <- group[j]
##         newclass <- class(data[, grpj])
##         class(groupid[, grpj]) <- newclass
##         if (newclass[1]=='factor' | newclass[1]=='ordered'){        
##           levels(groupid[, grpj]) <- levels(data[, grpj])
##         }
##       }
##       val <- cbind(groupid, val)
##     }
    
##     if (!is.null(idvar)){
##       idList <- lapply(splitData, function(x){
##         x[1, idvar, drop=FALSE]
##       })
##       idid <- do.call("rbind", idList)
##       val <- cbind(val, idid)
##     }
##     if (length(unique(names(val))) != length(names(val)))
##       warning("dataframe contains replicate names \n", call.=FALSE)
    
##     ##print("SSSSBBBB")
##     ##val <<- val
##     ##print(rhsstr)
##     if (order==TRUE){
##       if (rhsstr!="1")
##         val <- orderBy(as.formula(paste("~", rhsstr)), data=val)
##     }
    
##     names(val) <- gsub(" ","",names(val))
##     if (p2d)
##       names(val) <-  gsub("\\)","\\.", gsub("\\(","\\.",names(val)))
    
##     rownames(val) <- 1:nrow(val)
##     return(val)
##   }


## changeNames <- function(vv, fname, funnum, postfix, keep.names=FALSE){

##   trace <- 0
##   varnames     <- names(vv)
##   vectordim    <- length(vv[[1]])
##   statnames    <- names(vv[[1]])

##   defaultStatnames <-
##     if (vectordim>1)
##       defaultStatnames <- paste('stat',  1:vectordim, sep="")
##     else
##       'stat'

##   newStatnames     <-
##     if (is.null(statnames))
##       rep("", vectordim)
##     else
##       statnames
  
##   newStatnames[newStatnames==''] <- defaultStatnames[newStatnames=='']

##   if (vectordim>1)
##     keep.names <- FALSE

##   postfix <- unlist(postfix)
##   if (!is.null(postfix))
##     if (length(postfix) >= vectordim){
##       postfix <- postfix[1:vectordim]
##     } else {
##       postfix <- NULL
##     }

##   functionORlist <- (!is.na(pmatch("function",fname)) ||!is.na(pmatch("list(",fname))) 

##   if (trace>=1){
##     cat("status:\n")
##     cat(".fname             : ", fname,
##         "\n.functionORlist    : ", functionORlist,
##         "\n.keep.names        : ", keep.names,
##         "\n.postfix           : ", postfix, 
##         "\n.varnames          : ", varnames,
##         "\n.vectordim         : ", vectordim,
##         "\n.statnames         : ", statnames,
##         "\n.defaultStatnames  : ", defaultStatnames,
##         "\n.newStatnames      : ", newStatnames,
##         "\n.functionORlist    : ", functionORlist,
##         "\n")
##   }

##   if (!is.null(postfix)){
##     vv2 <- lapply(vv, "names<-", postfix);
##     vv2 <- unlist(vv2)
##   } else {
##     if (keep.names==TRUE){
##       ## vv <<- vv; varnames<<-varnames ;print(vv); print(varnames)
##       vv2 <- lapply(vv, "names<-", NULL);
##       vv2 <- unlist(vv2);
##     } else {
##       if (is.null(statnames)){
##         if (functionORlist){
##           ## E.g. FUN=function(x)... or FUN=c(function()..., function(x)..)
##           vv<- lapply(vv, "names<-", newStatnames)
##         } else {
##           ## E.g. FUN=c(mean,var)
##           if (vectordim>1){
##             newStatnames  <- paste(fname, 1:vectordim, sep="")
##             vv<- lapply(vv, "names<-", newStatnames);
##           } else { 
##             newStatnames <- paste(fname,  sep="")
##             vv<- lapply(vv, "names<-", newStatnames);
##           }    
##         } 
##       } else {    
##         vv <- lapply(vv, "names<-", newStatnames);    
##       }
##       vv2          <- unlist(vv)
##     }
##   }
##   return(vv2)
## }
  


    
##     currFUN <- FUN[[1]]
##     df <- tapply(data[,lhsvar[1]], rhsfact, function(x){
##       currFUN(x,...)
##     }, simplify=FALSE)

##     if (length(FUN)>1){
##       for (ff in 2:length(FUN)){
##         currFUN <- FUN[[ff]]
##         df <- cbind(df,
##                     tapply(data[,lhsvar[1]], rhsfact, function(x){
##                       currFUN(x,...)
##                     }, simplify=FALSE))        
##       }
##     }

##     df <- do.call(rbind, df)
##     print(df)

##     if (length(lhsvar)>1){
##       df2 <- matrix(NA, nrow=length(rhsunique), ncol=ncol(df)*(length(lhsvar)-1))
##       print(df2); print(lhsvar); print(FUN)
      
##       kk <- 1
##       for (ff in 1:length(FUN)){
##         currFUN <- FUN[[ff]]
##         for (vv in 2:length(lhsvar)){
##           df2[,kk] <- tapply(data[,lhsvar[vv]], rhsfact, function(x){
##             currFUN(x,...)
##           }, simplify=FALSE)
##           kk <- kk + 1
##         }
##       }
##       df <- cbind(df, df2)
##     }


##    transformData <- sapply(paste(lhsvar), function(x)eval(parse(text=x), data))

##     if (!is.null(rhsvar))
##       newdata <- cbind(transformData, data[,rhsvar,drop=FALSE])
##     else{
##       newdata <- as.data.frame(transformData)    
##     }

    
##     currFUN <- FUN[[1]]
##     df <- as.numeric(
##                        tapply(data[,lhsvar[1]], rhsfact, function(x){
##                          currFUN(x,...)
##                        }))
    
##     df = data.frame( df )
    
##     if(length(lhsvar) > 1){
##       for(ii in 2:length(lhsvar)){
##         df = cbind(df, as.numeric(
##           tapply(data[,lhsvar[ii]], rhsfact, function(x){
##             currFUN(x,...)
##           })))
##       }
##     }

##     if (length(FUN)>1){
##       for (kk in 2:length(FUN)){
##         currFUN <- FUN[[kk]]        
##         for(ii in 1:length(lhsvar)){
##           df = cbind(df, as.numeric(
##             tapply(data[,lhsvar[ii]], rhsfact, function(x){
##               currFUN(x,...)
##             })))
##         }
##       }
##     }


##     unrhs   <- unique(rhsfact)
##     unrhs   <<- unrhs

## ### Split data
## ###
##     groupFormula <- formula
##     groupFormula[[2]] <- NULL
##     splitData <- splitBy(groupFormula, data=newdata, drop=TRUE, return.matrix=FALSE)


    
##     byList <- as.list(rep(NA, length(unrhs)))

##     for (ii in 1:length(unrhs)){
##       x <- newdata[unrhs[ii]==rhsfact,]
##       xr  <- x[, lhsvarvec, drop = FALSE]
##       v <- NULL
##       for (j in 1:length(FUN)) {        
##         currFUN <- FUN[[j]]
##         vf2 <- lapply(xr, currFUN, ...)
##         vf2 <- changeNames(vf2,fun.names[j],j,postfix[j], keep.names)
##         v <- c(v,vf2)
##       }
##       byList[[ii]] <- v
##     }
        
##     val  <- as.data.frame(do.call("rbind", byList))
    
##     if (!is.null(rhsvar) && rhsvar!="1"){
##       group <- attr(terms(formula), "term.labels")  
##       groupid <- attr(splitData, "groupid")

##       ##print(groupid)
##       for (j in 1:length(group)){
##         grpj <- group[j]
##         newclass <- class(data[, grpj])
##         class(groupid[, grpj]) <- newclass
##         if (newclass[1]=='factor' | newclass[1]=='ordered'){        
##           levels(groupid[, grpj]) <- levels(data[, grpj])
##         }
##       }
##       val <- cbind(groupid, val)
##     }
    
##     if (!is.null(idvar)){
##       idList <- lapply(splitData, function(x){
##         x[1, idvar, drop=FALSE]
##       })
##       idid <- do.call("rbind", idList)
##       val <- cbind(val, idid)
##     }

##     if (length(unique(names(val))) != length(names(val)))
##       warning("dataframe contains replicate names \n", call.=FALSE)
    
##     if (order==TRUE){
##       if (rhsstr!="1")
##         val <- orderBy(as.formula(paste("~", rhsstr)), data=val)
##     }
    
##     names(val) <- gsub(" ","",names(val))
    
##     rownames(val) <- 1:nrow(val)
##     return(val)




## summaryDF = function(data, splits, operate.on.columns, functions = "mean",...){ 
##   ## function to create a summary data frame
##   ##  data is the original data to use
##   ##  splits = column names which describe the rows to combine
##   ##  operate.on.columns = column names which give the columns to summarize
##   ##  functions = the function to apply to the elements of one column
##   ##     having the same splits.
## #########################################################################
## ###########################################################################
## ## # example front end code
## ## #df1=read.table(file.choose(),header=T,sep=",")
## ## #df1$fips=df1$nst*1000+df1$ncty
## ## #names(df1)
## ## #[1] "yr"         "nst"        "crd"        "ncty"       "pacre"     
## ## #[6] "hacre"      "hyld"       "production" "fips"
## ## #######################################
## ## #source('c:\\rcode\\summaryDF.r') 
## ## #######################################
## ## #df.tmp=summaryDF(df1,c("nst","yr"), c("hacre","hyld"), c("mean"))
## ## #df.tmp=summaryDF(df1,c("nst","yr"), c("hacre","hyld"), c("mean"),na.rm=TRUE)
## ## #or write your own function
## ## #my.sum <- function(x)  {sum(x, na.rm=TRUE)}
## ## #df.tmp1=summaryDF(df7,c("mcty","myr"), c("pcpc","pcpc"),c("sum","my.sum"))
## ## # rename columns
## ## #dimnames(df.tmp1)[[2]] <- c("county","yr","pcpc1","pcpc2")
## ###########################################################################
## ########################################################################

##   if(!is.numeric(splits)){
##     if( !all(splits %in% names(data)))
##       stop("Second argument must be columns of the first argument.")
##   }
##   else {
##     if(any(splits < 0) | any(splits > length(data))){
##       stop("Second argument is < 0 or > dim(data)")
##     }
##   }
##   split.names <-  names(data[,splits, drop=F ])
##   #print(split.names) #SHD
  
##   if(!is.numeric(operate.on.columns)){    
##     if( !all(operate.on.columns %in% names(data)))
##       stop("Third argument must be columns of the first argument.")
##   }
##   else
##     if(any(operate.on.columns < 0) | any(operate.on.columns > length(data))){      
##       stop("Third argument is < 0 or > dim(data)")
##     }
  
##   if((op.len <- length(operate.on.columns)) > length(functions))
##     functions = rep(functions, op.len)[1:op.len]
##   ##  Use the same functions repeatedly
  
  
##   ##  start building up combinations of levels of split columns
  
##   combos <- as.character(unlist(data[,splits[1]]))

##   #print(combos)
  
##   if((s.len <- length(splits)) == 1){
##     ## only one split column
##     id.columns = list(levels(as.factor(data[,splits])))
##   }   
##   else {                         ## more than one split
##     for(i in splits[-1] )
##       combos = paste(combos,as.character(unlist(data[,i])),sep="@")

##   }

##   print(combos)

##   ## names(id.columns) = splits
##   ##  build first column to be output:
##   col1 = as.numeric(
##     tapply(data[,operate.on.columns[1]], combos, eval(functions[1]),...))
  
##   #print(col1)
  
##   df = data.frame( col1)
##   ## grab names to match farms with proper rows
##   if(s.len == 1){
##     id.columns =  data.frame(c1=names(tapply(combos,combos,length)))
##   }
##   else{
##     ## split apart the combo strings used in the split
##     rowlabels = strsplit(names(tapply(combos,combos,length)),"@")
##     ## a list of split strings
    
##     id.columns = data.frame( c1 =
##       unlist(lapply(rowlabels, function(x) x[1])))
##     ## first split labels in column 1
    
    
##     ## check to see if it's factor coded or numeric:
##     if(is.numeric(data[,splits[1]]))
##       id.columns[,1] = as.numeric(as.character(id.columns[,1]))
    
    
    
##     for(i in 2:s.len ){
##       id.columns = cbind(id.columns,
##         unlist( lapply(rowlabels,function(x) x[i])))
##       ## additional split labels in more columns
      
      

##       ## check to see if it's factor coded or numeric:
##       if(is.numeric(data[,splits[i]]))
##         id.columns[,i] = as.numeric(as.character(id.columns[,i]))
##     }
    
##   }
##   ##  give a name to each column
##   names(id.columns) = split.names

##   print(id.columns)

  
##   if(op.len > 1){
##     for(i in 2:op.len){
##       df = cbind(df, as.numeric(
##         tapply(data[,operate.on.columns[i]], combos,eval(functions[i]),...)))
##     }
##   }
  
## #  print(df)
## #  print(id.columns)


  
##   names(df) = paste(names(data[,operate.on.columns,drop=F]),functions,sep=".")
  
##   df2=data.frame(id.columns, df)
  
##                                         # Jim  I added the following.  Joe
  
##   for(i in 1:length(splits))  {
##     if(is.numeric(data[,splits[i]]))  {
##       df2[,splits[i]] = as.numeric(as.character(df2[,splits[i]]))
##     }
##   }
  
## #  print(df2)
##   df2
  
  
## }
 




