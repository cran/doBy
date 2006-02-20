subsetBy <- function(formula, subset, data=parent.frame()){
  mysubset <- function(d,subset){
    d[eval(parse(text=subset),d),]
  }
  ddd<-splitBy(formula, data=data)
  ddd<-lapply(ddd, mysubset, subset)
  return(do.call("rbind",ddd))
}
