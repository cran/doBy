

summaryBy <- 
function (formula, data = parent.frame(), id=NULL, FUN = mean, keep.names=FALSE,
          prefix=NULL,  ...) {

    
parseIt <- function(x){
  ##cat("parseIt:"); print(x); print(class(x))
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
  rhsvar   <<- rhsString(formula)
  idvar    <- rhsString(id)
  datavar  <- names(data)

  cls <<- lapply(data, class)

  numvar <<- datavar[cls %in% c("numeric","integer")]
  facvar <<- datavar[!(cls %in% c("numeric","integer"))]
  if (trace>=1){
    cat("datavar:"); print(datavar)
    cat("numvar :"); print(numvar)
    cat("facvar :"); print(facvar)
    cat("idvar  :"); print(idvar)
  }
  
  lhsAtoms <- parseIt(lhs)
  lhsvar   <<- lhsAtoms
  
  
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

### Prefix for new variables
  if (!is.null(prefix)){
    if (length(prefix) != length(fun.names))
      stop("Length of prefix not equal to the number of functions")
    varPrefix <- prefix
  } else {
    varPrefix <- fun.names
  }

  
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
      vf2 <- changeNames(vf2,fun.names[j],j,keep.names)
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

  rownames(val) <- 1:nrow(val)
  return(val)
}


changeNames <- function(vv, fname, funnum, keep.names=FALSE){
  ##fname <<-fname
  #print(keep.names)
  varnames     <- names(vv)
  vectordim    <- length(vv[[1]])
  statnames    <- names(vv[[1]])
  vv2          <- unlist(vv)
  #cat("fname:", fname, "varnames:", varnames, "vectordim:", vectordim,
  #    "statnames:", statnames,"\n")
  ##print(vv)
  
  if (is.null(statnames)){
    #cat("stats are not named...\n")
    if (!is.na(pmatch("function",fname)) ||!is.na(pmatch("list(",fname)) ){
      ## fname is not function(x)....
      #cat("fname is function(x).....\n")
      #print(varnames); print(fname); print(vv)
      if (keep.names==TRUE){
        statnames2 <- varnames
      } else {
        statnames2 <- paste("stat", 1:vectordim, sep="")
        vv<- lapply(vv, "names<-", statnames2)
      }

      ##print(vv)
    } else {
      #print("fname is NOT function(x).....")
      if (vectordim>1)
        statnames2 <- paste(fname, 1:vectordim, sep="")
      else {

        if (keep.names==FALSE){
          statnames2 <- paste(fname,  sep="")
          vv<- lapply(vv, "names<-", statnames2); #print(vv)                
        }        
      }
    }
  } else {
    #cat ("At least one stats has a name\n");  #print(statnames)
    if (keep.names==TRUE){
      vv <- lapply(vv, "names<-", NULL)
    } else {
      if (vectordim>1)
        statnames2 <- paste('stat',  1:vectordim, sep="")
      else
        statnames2 <- 'stat'

      statnames[statnames==''] <- statnames2[statnames=='']
      vv<- lapply(vv, "names<-", statnames);    #print(vv)      
    }
  }
  vv2          <- unlist(vv)
  ##print(vv2)  
  val <- vv2 ##unlist(vv)
  return(vv2)
}




# changeNames <- function(vv, fname, keep.names=FALSE){
#   if (is.matrix(vv)){
#     cn <- colnames(vv)
#     rn <- 
#     if (!is.null(rownames(vv))){
#         rownames(vv)
#       } else {
#         paste(1:nrow(vv))
#       }      
#     vv2<-as.numeric(vv)
#     names(vv2) <- unlist(lapply(cn, paste, rn, sep='.'))
#   } else {
#     vv2 <- vv
#     if (!keep.names)
#       names(vv2) <- lapply(names(vv), paste, fname, sep='.')
#   }
#   return(vv2)
# }



#   if (keep.names){
#     if (length(fun.names)==1)
#       newNames <- lhsvar
#     else {
#       keep.names <- FALSE
#       cat("Can not keep names of original variables, more than one function is applied \n")
#       newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
#     }
#   } else {
#     newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
#   }

#    xr  <- newdata[, lhsvarvec, drop = FALSE]
#    v <- NULL
#    for (j in 1:length(FUN)) {        
#      currFUN <- FUN[[j]]
#      print(fun.names[j])
     
#      vf <- apply(xr, 2, currFUN, ...)
#      vf <- print(changeNames(vf,fun.names[j]))
     
#      print(vf)
#      v <- c(v,vf)

#    }
#   vv<<-v

# summaryByOld <- 
# function (formula, data = parent.frame(), id=NULL, FUN = mean, keep.names=FALSE,
#           prefix=NULL,  ...) {
  
#   parseIt <- function(x){
#     if (class(x)=='name'){
#       value <- x
#     } else {
#       s <- paste(x[[1]])
#       value <- switch(s,
#                       '+'={  c(parseIt(x[[2]]),parseIt(x[[3]]))},
#                       'I'={  x[[2]]},
#                       {  x})
#     }
#     return(value)
#   }
  
#   lhsString <- function(formula) {
#     if (!is.null(formula))
#       if (length(formula)>=3)
#         unlist(strsplit(deparse(formula[[2]]),".\\+."))
#   }
#   rhsString <- function(formula) {
#     if (!is.null(formula))
#       unlist(strsplit(deparse(formula[[length(formula)]]),".\\+."))
#   }

#   #########################################

#   lhs      <- formula[[2]]
#   rhsvar   <- rhsString(formula)
#   ##print(rhsvar)
#   idvar    <- rhsString(id)
#   datavar  <- names(data)

#   lhsAtoms <- parseIt(lhs)
#   lhsvar   <- lhsAtoms
#   if (length(lhsvar)==1)
#     lhsvar <- list(lhsvar)

#   if (paste(rhsvar)[1]=='.')
#     rhsvar <- NULL
  
#   if ("." %in% lhsvar){
#     lhsvar <- setdiff(lhsvar, ".")
#     v <- setdiff(datavar, c(lhsvar, rhsvar, idvar))
#     isSpecial <- rep(NA,length(v))
#     for (j in 1:length(v)){
#       isSpecial[j]<- (class(data[,v[j]])[1] %in% c("POSIXt", "factor", "character"))
#     }      
#     extralhsvar<-v[!isSpecial]
#     lhsvar <- union(lhsvar, extralhsvar)    
#   }

#   ## print(lhsvar)

#   if (is.null(rhsvar)){
#     rhsvar <- setdiff(datavar, c(lhsvar, idvar))
#     formula <- as.formula(paste(formula[[2]], "~", paste (rhsvar, collapse='+')))
#   }
    
# #   cat("status:\n")
# #   cat("lhsvar     :", paste(lhsvar),"\n")
# #   cat("rhsvar     :", paste(rhsvar),"\n")
# #   cat("idvar      :", paste(idvar),"\n")
# #   print(formula)


#   transformData <- sapply(paste(lhsvar), function(x)eval(parse(text=x), data))

#   ## Function names
#   if (!is.list(FUN)) 
#     fun.names <- paste(deparse(substitute(FUN)), collapse = " ")
#   else
#     fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
#   ##cat("fun.names  :", paste(fun.names), "\n")
#   if (!is.list(FUN)) 
#     FUN <- list(FUN)

#   ## Prefix for new variables
#   if (!is.null(prefix)){
#     if (length(prefix) != length(fun.names))
#       stop("Length of prefix not equal to the number of functions")
#     varPrefix <- prefix
#   } else {
#     varPrefix <- fun.names
#   }

#   ## Names for new variables
#   if (keep.names){
#     if (length(fun.names)==1)
#       newNames <- lhsvar
#     else {
#       cat("Can not keep names of original variables because more than one function is applied \n")
#       newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
#     }
#   } else {
#     newNames <- unlist(lapply(varPrefix, paste, lhsvar, sep = "."))
#   }

#   newdata <- cbind(transformData, data[,c(rhsvar,idvar),drop=FALSE])

#   ## Split data
#   groupFormula <- formula
#   groupFormula[[2]] <- NULL
#   splitData <- splitBy(groupFormula, data=newdata, drop=TRUE, return.matrix=TRUE)


  
#   ## Calculate groupwise statistics
#   lhsvarvec <- paste(unlist(lhsvar)) 

#   byList <- lapply(splitData, function(x){
#     xr  <- x[, lhsvarvec, drop = FALSE]
#     v <- NULL
#     for (j in 1:length(FUN)) {        
#       currFUN <- FUN[[j]]
#       vf <- apply(xr, 2, currFUN, ...)
#       v <- c(v,vf)
#     }
#   })

#   val        <- as.data.frame(do.call("rbind", byList))
#   names(val) <- c(newNames)

  
#   if (!is.null(rhsvar)){
#     group <- attr(terms(formula), "term.labels")  
#     groupid <- attr(splitData, "groupid")

#     for (j in 1:length(group)){
#       grpj <- group[j]
#       newclass <- class(data[, grpj])
#       class(groupid[, grpj]) <- newclass
#       if (newclass[1]=='factor' | newclass[1]=='ordered'){        
#         levels(groupid[, grpj]) <- levels(data[, grpj])
#       }
#     }
#     val <- cbind(groupid, val)
#   }
  
#   if (!is.null(idvar)){
#     idList <- lapply(splitData, function(x){
#       x[1, idvar, drop=FALSE]
#     })
#     idid <- do.call("rbind", idList)
#     val <- cbind(val, idid)
#   }
#   return(val)
# }

#   ##cat("varPrefix  :", paste(varPrefix), "\n")
#   ##cat("newNames   :", paste(newNames),"\n")


#   ##print("transformData");  print(transformData[1:10,])

#   ##print(data[1:5,c(rhsvar,idvar),drop=FALSE])
#   ##print(newdata[1:5,])

