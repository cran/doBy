
summaryBy <- 
function (formula, data = parent.frame(), id=NULL, FUN = mean, ...) 
{
    mf <- match.call(expand.dots = FALSE)

    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0)
    ff <- as.formula(mf[[2]])
    d <- data

    
    ## idvars are just copied to the output
    if (!is.null(id))
      if (class(id)!="formula"){
        stop("id variables must be given as a formula.")
      } else {
        idvar <-unlist(strsplit(deparse(id[[2]]),".\\+."))
      }
    else
      idvar <- NULL
    
    if (ff[[2]] == ".") {
      ffc <- as.character(ff) 
      v <- paste(setdiff(names(data), all.vars(ff)))
      v<-setdiff(v,idvar)
      isFactor <- rep(NA,length(v))
      for (j in 1:length(v)){
        isFactor[j]<-is.factor(d[,v[j]])
      }      
      v<-v[!isFactor]
      v <- paste(v, collapse='+')
      ffc[[2]] <- paste(v) #paste("cbind(", v, ")")
      ff <- as.formula(paste(ffc[[2]], ffc[[3]], sep = ffc[[1]]))
    }

    ## ff[[2]] : lhs of formula
    respVar <- unlist(strsplit(deparse(ff[[2]]),".\\+."))
    yyy <- sapply(respVar, function(x)eval(parse(text=x), d))
    
    ff <- terms(ff, data = data)

    m <- match.call(expand.dots = TRUE)

    respny<-unlist(strsplit(deparse(ff[[2]]),".\\+."))
    respny <- setdiff(respny, idvar)

    yy <- yyy
    if (is.null(dim(yy))) 
        dim(yy) <- c(length(yy), 1)
    if (!is.list(FUN)) 
        fun.names <- paste(deparse(substitute(FUN)))
    else fun.names <- unlist(lapply(substitute(FUN)[-1], function(a) paste(a)))
    if (!is.list(FUN)) 
        FUN <- list(FUN)

    group <- attr(terms(ff), "term.labels")

    resp <- respny
    colnames(yy) <- resp
    groupList <- NULL

    s <- unlist(lapply(fun.names, paste, resp, sep = "."))

    extra <- which(is.na(match(colnames(yy), names(d))))
    d <- cbind(d, yy[, extra, drop = FALSE])
    
    vv <- by(d, d[, group],
             function(x) {
               xf <- x[1, group, drop = FALSE]
               if (!is.null(id))
                 xid <- x[1, idvar, drop = FALSE]
               else
                 xid <- NULL
               xr <- x[, resp, drop = FALSE]
               #v <- rep(NA,length(FUN))
               v <- NULL
               for (j in 1:length(FUN)) {        
                 FF <- FUN[[j]]
                 #vf <- apply(na.omit(xr), 2, FF)
                 vf <- apply(xr, 2, FF, ...)
                 #v[j] <- vf
                 v <- c(v,vf)
               }
               v <- c(xf, xid, v)
             })

    idx <- unlist(lapply(vv, length)) > 0
    vv <- vv[idx]
    val <- unlist(vv)
    val <- as.data.frame(matrix(val, ncol = length(vv[[1]]), 
        byrow = TRUE))
    names(val) <- c(group, idvar, s)

    grpi <- match(group, names(d))
    for (j in 1:length(group)) {
        if (is.factor(d[, group[j]])) {
            l <- levels(d[, grpi[j]])[val[, group[j]]]
            val[, group[j]] <- as.factor(l)
        }
    }

    for (j in 1:length(s)){
      val[,s[j]] <- as.numeric(paste(val[,s[j]]))
    }

    return(val)
}


