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
      grps <- grpsvec

      ##print("IIIIIIIIIIIII")
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
      ##at <<- at
      ##a  <<- a
      #print(group)
      #print(groupData)
      ##print("IIIIIIIIJJJJJJJJJJ")
      #print(groupData)
      groupid        <- lapply(groupData, function(x) x[1,group,drop=FALSE])
      #print(groupid)

      names(groupid) <- NULL
      groupid        <- as.data.frame(do.call('rbind',groupid))
      names(groupid) <- group

      attr(groupData,"groupid") <- groupid
    }

    return(groupData)
}

