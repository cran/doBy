#######################################################################
#'
#' @title Formula based version of lapply.
#' @description This function is a wrapper for calling lapply on the
#'     list resulting from first calling splitBy.
#' @name by-lapply
#' 
#######################################################################
#' @param formula A formula describing how data should be split.
#' @param data A dataframe.
#' @param FUN A function to be applied to each element in the splitted
#'    list, see 'Examples' below.
#'
#' @return A list.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{lapplyBy}}
#'
#' @keywords utilities
#'
#' @examples
#' data(dietox)
#' 
#' ## Calculate weekwise feed efficiency = weight gain / feed intake
#' dietox <- orderBy(~Pig + Time, data=dietox)
#' v <- lapplyBy(~Pig, data=dietox,
#'               function(d) c(NA, diff(d$Weight) / diff(d$Feed)))
#' dietox$FE <- unlist(v)
#' 
#' ## Technically this is the same as 
#' dietox <- orderBy(~Pig + Time, data=dietox)
#' wdata <- splitBy(~Pig, data=dietox)
#' v <- lapply(wdata, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
#' dietox$FE <- unlist(v)
#' 


#' @export
#' @rdname by-lapply
lapply_by <- function(data, formula, FUN=mean){
    cl   <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("lapplyBy")
    eval(cl)
}

#' @export
#' @rdname by-lapply
lapplyBy <- function (formula, data = parent.frame(), FUN) 
{
    out <- splitBy(formula, data = data)
    gr  <- unique(attr(out, "grps"))
    ##print(gr)    
    out <- lapply(out, FUN)
    out <- out[gr]
    out
}


