
subSeq <- function(x, item=NULL){
  first  <-  c(1,which(c(FALSE,
                         mapply(function(a,b){!identical(a,b)},
                                x[-1],x[-length(x)])
                         )
                       )
               )
  last   <-  c( first[-1] - 1, length(x))
  rlen   <-  (last-first)+1
  value  <-  x[first]
  midp   <-  floor(first + rlen/2)
  ans    <-  data.frame(value=value, first=first, last=last, slength=rlen,
                        midpoint=midp, stringsAsFactors=FALSE)

  if (!is.null(item)){
    if (item %in% value){
      iii <- unlist(lapply(as.character(ans$value), identical, as.character(item)))
      ans <- ans[iii,]
    } else {
      ans <- NULL
    }
  } 
  return(ans)  
}



## subseq <- function(x, item=NULL){
##   first  <-  c(1,which(c(FALSE, mapply(function(a,b)!identical(a,b), x[-1],x[-length(x)]))))
##   last   <-  c( first[-1] - 1, length(x))
##   rlen   <-  (last-first)+1
##   value  <-  x[first]
##   ans    <-  data.frame(value=value, first=first, last=last, slength=rlen)
##   ans$midpoint <- floor(first + rlen/2)

##   if (!is.null(item)){
##     if (item %in% value){
##       ans <- ans[ans$value==item,]
##     } else {
##       ans <- NULL
##     }
##   } 
##   return(ans)  
## }



# Example:
# x <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 3)
# subseq(x)
# subseq(x,item=1)

# x <- c(T,T,F,F,F,T,F,T,T,T,T)
# subseq(x)
# subseq(x,item=TRUE)





