splitBy<-function (formula, data = parent.frame(),drop=TRUE, return.matrix=FALSE)
{
    mf <- match.call(expand.dots = FALSE)
    
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0)
    ff <- as.formula(eval.parent(mf[[2]]))
    if (ff[[2]]==1){
      groupData <- list(data)
      attr(groupData,"groupid") <- 1
    } else {
      
      ff    <- terms(ff, data = data)
      m     <- match.call(expand.dots = TRUE)
      group <- attr(terms(ff), "term.labels")
      
      ## workinggroup: Those variables in 'group' which are not constant.
      nonconstgroup <- rep(TRUE, length(group))
      
      names(nonconstgroup) <- group
      for (i in 1:length(group)){
        nunique <- length(unique(data[,group[i]]))
        ##cat("Variable:", group[i], "unique values:", nunique, "\n")
        if (nunique==1)
          nonconstgroup[i] <- FALSE
      }
      workinggroup <- group[nonconstgroup]            

      ## grps: Recode groups into one vector
      grps <- data[, workinggroup,drop=FALSE]
      grpsvec<-paste(grps[,1])
      if (ncol(grps)>1){
        for (j in 2:ncol(grps)){
          grpsvec <- paste(grpsvec,paste(grps[,j]),sep='|')
        }}
      grps <- grpsvec ## aa|b|xx etc...

      dataMatrix <- .asNumericMatrix2(data)
      at <- .subsAttr2(data)
      
      a <- mApply(dataMatrix, grps, function(x){x}, simplify=FALSE)
      if (drop==TRUE)
        a<- a[lapply(a,nrow)>0]

      if (return.matrix==TRUE)
        groupData <- a
      else{
        groupData <- lapply(a, .matrix2dataFrame2, at=at,restoreAll=FALSE)
      }
      groupid        <- lapply(groupData, function(x) x[1,group,drop=FALSE])
      
      names(groupid) <- NULL
      groupid        <- as.data.frame(do.call('rbind',groupid))
      names(groupid) <- group
      rownames(groupid) <- 1:nrow(groupid)
      
      attr(groupData,"groupid") <- groupid

      uniqval <- sapply(1:nrow(groupid), function(k){
        x <- groupid[k,,drop=TRUE]
        paste(x,collapse='|')
      })
      
      idxvec <- as.list(rep(NA, length(uniqval)))
      names(idxvec) <- uniqval
      for (i in 1:length(uniqval)){
        idxvec[[i]] <- which(grps==uniqval[i])
      }
      
      attr(groupData,"idxvec") <- idxvec
      attr(groupData,"grps") <- grps
    }
    class(groupData) <- c("splitByData", "list")
    return(groupData)
}

print.splitByData <- function(x,...){
#  print(attr(x,"groupid"))
  print(cbind(listentry=names(x), attr(x,"groupid")))
  return(invisible(x))
}





