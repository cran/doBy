splitBy<-function (formula, data = parent.frame())
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "offset"), names(mf), 0)
    ff <- as.formula(eval(mf[[2]]))

    if (ff[[2]]==1){
      print("JKLJLJLW")
      groupData <- list(data)
      attr(groupData,"groupid") <- 1
    } else {
      ff <- terms(ff, data = data)
      m <- match.call(expand.dots = TRUE)
      group <- attr(terms(ff), "term.labels")
      groupData <- split(data, data[, group])
      groupid<-lapply(groupData,function(x)x[1,group])
      names(groupid)<-NULL
      groupid<-as.data.frame(do.call('rbind',groupid))
      print.default(groupid)
      names(groupid) <- group
      groupid <<- groupid
      ##attr(groupData,"groupid") <- groupid
    }
    return(groupData)
}
