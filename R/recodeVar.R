#' Recode values of a vector
#' 
#' Recodes a vector with values, say 1,2 to a variable with values, say 'a',
#' 'b'
#' 
#' @param x A vector; the variable to be recoded.
#' @param src The source values: a subset of the present values of x
#' @param tgt The target values: the corresponding new values of x
#' @param default Default target value for those values of x not listed in
#'     `src`. When default=NULL, values of x which are not given in `src` will
#'     be kept in the output.
#' @param keep.na If TRUE then NA's in x will be retained in the output
#' @return A vector
#' @section Warning : Care should be taken if x is a factor. A safe approach may
#'     be to convert x to a character vector using as.character.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link[base]{cut}}, \code{\link[base]{factor}},
#'     \code{\link[doBy]{recodeVar}}
#' @keywords utilities
#' @examples
#' 
#' x <- c("dec", "jan", "feb", "mar", "apr", "may")
#' src1 <- list(c("dec", "jan", "feb"), c("mar", "apr", "may"))
#' tgt1 <- list("winter", "spring")
#' recodeVar(x, src=src1, tgt=tgt1)
#' #[1] "winter" "winter" "winter" "spring" "spring" "spring"
#' 
#' x <- c(rep(1:3, 3))
#' #[1] 1 2 3 1 2 3 1 2 3
#' 
#' ## Simple usage:
#' recodeVar(x, src=c(1, 2), tgt=c("A", "B"))
#' #[1] "A" "B" NA  "A" "B" NA  "A" "B" NA 
#' 
#' ## Here we need to use lists
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"))
#' #[1] "A" "A" NA  "A" "A" NA  "A" "A" NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L")
#' #[1] "A" "A" "L" "A" "A" "L" "A" "A" "L"
#' recodeVar(x, src=list(c(1, 2), 3), tgt=list("A", "B"), default="L")
#' #[1] "A" "A" "B" "A" "A" "B" "A" "A" "B"
#' 
#' ## Dealing with NA's in x
#' x<-c(NA,rep(1:3, 3),NA)
#' #[1] NA  1  2  3  1  2  3  1  2  3 NA
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"))
#' #[1] NA  "A" "A" NA  "A" "A" NA  "A" "A" NA  NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L")
#' #[1] NA  "A" "A" "L" "A" "A" "L" "A" "A" "L" NA 
#' recodeVar(x, src=list(c(1, 2)), tgt=list("A"), default="L", keep.na=FALSE)
#' #[1] "L" "A" "A" "L" "A" "A" "L" "A" "A" "L" "L"
#' 
#' x <- c("no", "yes", "not registered", "no", "yes", "no answer")
#' recodeVar(x, src = c("no", "yes"), tgt = c("0", "1"), default = NA)
#' 
#' 
#' @export recodeVar
recodeVar <- function(x, src, tgt, default=NULL, keep.na=TRUE){

  if (length(src)!=length(tgt)){
    stop("length of src not equal to length of tgt")
  }
  mtc <- lapply(src, function(zzz){which(x %in% zzz)})
  idx <- seq_along(x)
  unmatch <- setdiff(idx, unlist(mtc))
  
  if (is.factor(x)){
    val <- as.character(x)
  } else {
    val <- x
  }
  for (ii in 1:length(tgt))
    val[mtc[[ii]]] <- tgt[[ii]]

  if (!is.null(default)){
    if (keep.na){
      iii <- intersect(which(!is.na(x)), unmatch)
      val[iii] <- default
    } else {
      val[unmatch] <- default
    }
  }
  
  if (is.factor(x))
    val <- as.factor(val)
  val

  return(val)
}
