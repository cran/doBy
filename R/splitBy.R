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

      ff <- terms(ff, data = data)
      m <- match.call(expand.dots = TRUE)
      group <<- attr(terms(ff), "term.labels")
      
      ## workinggroup: Those variables in 'group' which are not constant.
      nonconstgroup <- rep(TRUE, length(group))
      
      ##print(system.time({
      names(nonconstgroup) <- group
      for (i in 1:length(group)){
        nunique <- length(unique(data[,group[i]]))
        ##cat("Variable:", group[i], "unique values:", nunique, "\n")
        if (nunique==1)
          nonconstgroup[i] <- FALSE
      }
      workinggroup <<- group[nonconstgroup]            
      ##}))


      ## grps: Recode groups into one vector
      grps <- data[, workinggroup,drop=FALSE]
      grpsvec<-paste(grps[,1])
      if (ncol(grps)>1){
        for (j in 2:ncol(grps)){
          grpsvec <- paste(grpsvec,paste(grps[,j]),sep='|')
        }}
      grps <- grpsvec
      
      ##print(data)
      ##print(system.time({
        dataMatrix <<- asNumericMatrix(data)
      ##}))
            
      ##print(system.time({
        at <- subsAttr(data)
      ##}))
      
      ##print(system.time({
      a <<- mApply(dataMatrix, grps, function(x){x}, simplify=FALSE)
      ##}))
                
      if (drop==TRUE)
        a<<- a[lapply(a,nrow)>0]

      if (return.matrix==TRUE)
        groupData <- a
      else{
        ##print(system.time({
        groupData <- lapply(a, matrix2dataFrame, at=at)
        ##}))
      }
      
      ##print(system.time({
        groupid<-lapply(groupData, function(x) x[1,group])
        names(groupid)<-NULL
      ##}))


      ##print(system.time({
        groupid<-as.data.frame(do.call('rbind',groupid))
      ##}))
      
      ##print.default(groupid)
      names(groupid) <- group
      #groupid <<- groupid
      attr(groupData,"groupid") <- groupid
    }
    return(groupData)
}
