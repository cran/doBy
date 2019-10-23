#' @title Find sub-sequences of identical elements in a vector.
#' 
#' @description Find sub-sequences of identical elements in a vector.
#' 
#' @param x An atomic vector.
#' @param item Optionally a specific value to look for in 'x'.
#' @return A dataframe.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{rle}}
#' @keywords utilities
#' @examples
#' 
#' x <- c(1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 3)
#' (ans <- subSeq(x))
#' ans$value
#' # Notice: Same results below
#' subSeq(x, item=1)
#' subSeq(x, item="1")
#' 
#' x <- as.character(c(1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 3))
#' (ans<-subSeq(x))
#' ans$value
#' # Notice: Same results below
#' subSeq(x, item="1")
#' subSeq(x, item=1)
#' 
#' @export subSeq

subSeq <- function (x, item = NULL) {
    rrr <- rle(x)
    len <- rrr$lengths
    val <- rrr$values

    first <- last <- rep.int(NA, length(val))
    first[1] <- 1
    last [1] <- len[1]
    if (length(val) > 1){
	for (kk in 2:length(val)){
            first[kk] <- last[kk-1]+1
            last [kk] <- last[kk-1]+len[kk]
	}
    }
    midp <- floor(first + len / 2)

    ans <- cbind(first=first, last=last, slength=len, midpoint=midp)

    if (!is.null(item)) {
        iii <- val==item
        ans <- as.data.frame.matrix(ans[iii,,drop=FALSE], stringsAsFactors=FALSE)
        ans$value <- val[iii]
    } else {
        ans <- as.data.frame.matrix(ans, stringsAsFactors=FALSE)
        ans$value <- val
    }
    ans
}


