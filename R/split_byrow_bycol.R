#' @title Split matrix or dataframe into list
#'
#' @description Split matrix or dataframe into list by columns or by rows
#'
#' @name split_byrow_bycol
#' @param x Matrix or dataframe.
#' @param idx Index to split by. If NULL, split by columns or rows.
#' @param as.list If TRUE, return list of dataframes. If FALSE, return list of matrices.
#'
#' @examples
#' x <- mtcars[1:3, 1:6]
#' x  |> split_bycol()
#' x  |> split_bycol(as.list=TRUE)
#' x  |> split_bycol(as.list=FALSE)
#' x  |> split_bycol(idx=c(1,1,1,2,2,3,3,3))
#' ## x  |> split_bycol(idx=c(1,1,7,2,2,3,3,3)) ## Gives error
#' 
#' x <- mtcars[1:6, 1:6]

#' x  |> split_byrow()
#' x  |> split_byrow(idx=c(1,1,2,2))
#' 


#' @export
#' @rdname split_byrow_bycol
split_bycol <- function(x, idx=NULL, as.list=FALSE) {
  if (!inherits(x, c("matrix", "data.frame")))
    stop("'x' must be matrix or dataframe\n")

   if (ncol(x) == 0) {
       return(list()) 
   } 

  isdf <- inherits(x, "data.frame")

  if (as.list){
      out <- as.list(as.data.frame(x))            
  } else {
      if (is.null(idx))
          idx <- 1:ncol(x)
      if (!(is.numeric(idx))) {
          stop("'idx' must be numeric\n")
      }
      if (!(length(idx) == ncol(x))){
          idx <- rep(idx, length.out = ncol(x))
      }
      if (max(idx)>ncol(x))
          stop("max idx exceeds number of columns\n")

      out <- lapply(seq_along(unique(idx)),
                    function(i) {
                        zz <- x[, idx == i, drop=FALSE]
                        names(zz) <- names(x)[idx == i]
                        zz
                    })
  }

  if (!isdf){
      out <- lapply(out, as.matrix)
  }
  return(out)
}    

#' @examples
#' m <- as.matrix(x)
#' u <- x |> split_byrow(idx=c(1,1,2,2))
#' y <- m |> split_byrow(idx=c(1,1,2,2))
#' 
#' @export
#' @rdname split_byrow_bycol
split_byrow <- function (x, idx=NULL)  {
    if (!inherits(x, c("matrix", "data.frame"))) 
        stop("'x' must be matr ix or dataframe\n")

  isdf <- inherits(x, "data.frame")
  
   if (nrow(x) == 0) { 
        return(list())
    }

    if (is.null(idx)) {
        idx <- 1:nrow(x)
    }

    if (!(length(idx) == nrow(x))){
      idx <- rep(idx, length.out = nrow(x))
    }
  
    out <- split(x, idx)

    if (!isdf){
        out <- lapply(out, function(z){
          matrix(z, ncol=ncol(x))
    })
    }
    

    if (!is.null(colnames(x))) {
        out <- lapply(out, function(z){
          colnames(z) <- colnames(x)
          z
        })
    }
    return(out)
}


# split_bycol <- function(x) {
#     if (!inherits(x, c("matrix", "data.frame")))
#         stop("'x' must be matrix or dataframe\n")
#  
#      if (ncol(x) == 0) {
#          return(list()) 
#      } 
#   
#      as.list(as.data.frame(x))
# } 
# 
# 

# 



# x <- mtcars[1:5, 1:8]
# 
# 
# ## case 1
# as.list(x)
# 
# ## case 2
# out <- lapply(as.list(x), as.data.frame)
# out <- lapply(seq_along(out), function(i) {
#   zz <- out[[i]]
#   names(zz) <- names(x)[i]
#   zz
# })
# out
# 
# #case 3 ## always dataframe
# idx <- c(1,1,1,2,2, 3,3,3)
# split_bycol(x, idx)
# lapply(seq_along(unique(idx)),
#        function(i) {
#          zz <- x[, idx == i]
#          names(zz) <- names(x)[idx == i]
#          zz
#        })
# 
# 
# f <- 1:nrow(x)
# f <- c(1,1,1,2,2)
# out <- split(x, f)
# #    if (!is.null(rownames(x))) {
# #        names(out) <- rownames(x)
# #    }
# out
# 
