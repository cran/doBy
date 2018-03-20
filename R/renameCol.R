#' Rename columns in a matrix or a dataframe.
#' 
#' Rename columns in a matrix or a dataframe.
#' 
#' 
#' @param indata A dataframe or a matrix
#' @param src Source: Vector of names of columns in 'indata' to be renamed. Can
#' also be a vector of column numbers.
#' @param tgt Target: Vector with corresponding new names in the output.
#' @return A dataframe if 'indata' is a dataframe; a matrix in 'indata' is a
#' matrix.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}

#' @keywords utitlities
#' @examples
#' 
#' 
#' renameCol(CO2, 1:2, c("kk", "ll"))
#' renameCol(CO2, c("Plant", "Type"), c("kk", "ll"))
#' 
#' # These fail - as they should:
#' # renameCol(CO2, c("Plant", "Type", "conc"), c("kk", "ll"))
#' # renameCol(CO2, c("Plant", "Type", "Plant"), c("kk", "ll"))
#' 
#' @export renameCol
renameCol <- function(indata, src, tgt){

  if (inherits(indata, "data.frame")) {
    isDF <- TRUE
    dfnames <- names(indata)
  } else {
    if (inherits(indata, "matrix")) {
      isDF <- FALSE
      dfnames <- colnames(indata)
    } else {
      stop("'indata' must be either a dataframe or a matrix")
    }
  }
  
  if (length(src)!=length(unique(src))){
    stop("A src name has been repeated")
  }

  if (length(tgt)!=length(unique(tgt))){
    stop("A tgt name has been repeated")
  }

  if (length(src)!=length(tgt)){
    stop("length of src not equal to length of tgt")
  }
    
  if (is.numeric(src)){
    idx <- src
    iii <- intersect(seq_along(dfnames), src)
    iii <- setdiff(src, iii)
    if (length(iii)>0){
      sss <- sprintf("Column(s) %s are not in 'indata'", toString(iii))
      stop(sss)
    }
  } else {
    idx <- match(src, dfnames)
    if (any(is.na(idx))){
      sss <- sprintf("Column names %s are not in 'indata'", toString(src[is.na(idx)]))
      stop(sss)
    }
  }
  
  ans <- indata
  if (isDF){
    names(ans)[idx] <- tgt
  } else {
    colnames(ans)[idx] <- tgt
  }

  return(ans)
}
