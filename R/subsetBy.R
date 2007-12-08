# subsetBy <- function(formula, subset, data=parent.frame()){
#   mysubset <- function(d,subset){
#     d[eval(parse(text=subset),d),]
#   }
#   ddd<-splitBy(formula, data=data)
#   ddd<-lapply(ddd, mysubset, subset)
#   return(do.call("rbind",ddd))
# }

subsetBy <- function(formula, subset, data=parent.frame(), select, drop=FALSE, join=TRUE,...){
  ddd<-splitBy(formula, data=data)
  subsetMissing <- missing(subset)
  selectMissing <- missing(select)  
  e <- substitute(subset)
  ddd<-lapply(ddd, 
    function(x){
      if (subsetMissing) 
          r <- TRUE
      else {
          r <- eval(e, x, parent.frame())
          if (!is.logical(r)) 
              stop("'subset' must evaluate to logical")
          r <- r & !is.na(r)
      }
      if (selectMissing) 
          vars <- TRUE
      else {
          nl <- as.list(1:ncol(x))
          names(nl) <- names(x)
          vars <- eval(substitute(select), nl, parent.frame())
      }
      x[r, vars, drop = drop]
    }
  )
  if (join)
    return(do.call("rbind",ddd))
  else
    return(ddd)
}
