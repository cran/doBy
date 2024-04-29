#' @title Find sub-sequences of identical elements in a vector. 
#' @description Find sub-sequences of identical elements in a vector.
#' @name sub_seq
#' 
#' @param x An atomic vector or a factor.
#' @param item Optionally a specific value to look for in `x`.
#' @return A dataframe.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{rle}}
#'
#' @details
#'
#' * `sub_seq` is synonymous with `subSeq`
#' 
#' * `rle2` is identical to `rle` (from base) but `rle2` works on
#'     factors as input (a factor is coerced to character).
#'
#' * `is_grouped` checks if the values in `x` are clustered into the
#' smallest number of clusters.
#' 
#' @keywords utilities
#' @examples
#' 
#' x <- c(1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 3)
#' (ans <- subSeq(x))
#' ans$value
#' 
#' ## Notice: Same results below
#' subSeq(x, item=1)
#' subSeq(x, item="1")
#' 
#' xc <- as.character(x)
#' (ans<-subSeq(xc))
#' ans$value
#'
#' ## Notice: Same results below
#' subSeq(xc, item="1")
#' subSeq(xc, item=1)
#'
#' is_grouped(x)
#' is_grouped(sort(x))
#' is_grouped(xc)
#' is_grouped(sort(xc))


#' @rdname sub_seq
#' @export subSeq
subSeq <- function (x, item = NULL) {
    rrr <- rle2(x)
    len <- rrr$lengths
    val <- rrr$values

    first <- last <- rep.int(NA, length(val))
    first[1] <- 1
    last [1] <- len[1]
    if (length(val) > 1) {
	for (kk in 2:length(val)) {
            first[kk] <- last[kk - 1] + 1
            last [kk] <- last[kk - 1] + len[kk]
	}
    }
    midp <- floor(first + len / 2)

    ans <- cbind(first=first, last=last, slength=len, midpoint=midp)

    if (!is.null(item)) {
        iii <- val == item
        ans <- as.data.frame.matrix(ans[iii,, drop=FALSE], stringsAsFactors=FALSE)
        ans$value <- val[iii]
    } else {
        ans <- as.data.frame.matrix(ans, stringsAsFactors=FALSE)
        ans$value <- val
    }
    ans
}

#' @rdname sub_seq
#' @export
sub_seq <- subSeq

#' @rdname sub_seq
#' @export
is_grouped <- function(x) {
    !any(table(rle2(x)$values) > 1)    
}

#' @rdname sub_seq
#' @export
rle2 <- function(x) {
    if (is.factor(x)) {        
        rle(as.character(x))
    } else {
        rle(x)
    }
}
















