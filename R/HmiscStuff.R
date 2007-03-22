.matrix2dataFrame2 <- function (x, at, restoreAll = TRUE)
{
    d <- dimnames(x)
    k <- length(d[[2]])
    w <- vector("list", k)
    nam <- names(w) <- d[[2]]
    sm <- storage.mode(x)
    for (i in 1:k) {
      a <- at[[nam[i]]]; # print("a"); print(a)
      if (!length(a))
        next
      xi <- x[, i]
      names(xi) <- NULL

      ## SHD Add:
      ## Handle characters
      if (!is.null(a$storage.mode) && a$storage.mode == "character"){
        xi <- a$flevels[xi]
        a$flevels <- NULL
      }
      ## Handle times
      if (!is.null(a$class)){
        if (identical(a$class, c("POSIXt", "POSIXct"))){
          if (a$storage.mode != sm)
            storage.mode(xi) <- a$storage.mode
          #a$storage.mode <- NULL
          attributes(xi) <- a       
        }          
      }
      ## End !

      if (restoreAll) {
        if (a$storage.mode != sm)
          storage.mode(xi) <- a$storage.mode
        a$storage.mode <- NULL
        attributes(xi) <- a
      }
      else {
        if (length(l <- a$label))
          label(xi) <- l
        if (length(u <- a$units))
          units(xi) <- u
        if (length(lev <- a$levels))
          xi <- factor(xi, 1:length(lev), lev)
      }
      w[[i]] <- xi
    }
    structure(w, class = "data.frame", row.names = d[[1]])
  }


.subsAttr2<-function (x)
{
    g <- function(y) {
        a <- attributes(y)
        a$dim <- a$names <- a$dimnames <- NULL
        a$storage.mode <- storage.mode(y)
        ## Add:
        if (length(class(y))==1 && class(y)=="character")
          a$flevels <- levels(as.factor(y))
        
        ## End!
        a
      }
    if (is.list(x)){
      ##cat("calling 1\n")
      sapply(x, g)
    } else {
      ##cat("calling 2\n")
      g(x)
    }
  }

.asNumericMatrix2 <- function (x)
{
    a <- attributes(x)
    k <- length(a$names)

    idx <- which(lapply(x,class)=="character")
    if (length(idx)>0){
      for (j in idx){
        x[,j] <- as.factor(x[,j])
      }
    }
    

#     val <- lapply(x, function(xx){
#       if (class(xx)=="character")
#         as.factor(xx)
#       else
#         xx
#     })
#     x<- val


    y <- matrix(unlist(x), ncol = k, dimnames = list(a$row.names,  a$names))
    #y <- matrix(unlist(val), ncol = k, dimnames = list(a$row.names,  a$names))
    ## End

    # y <- matrix(as.numeric(unlist(x)), ncol = k, dimnames = list(a$row.names,  a$names))
    #if (storage.mode(y) == "character")
    #    warning("x had at least one character vector")
    y
}
