## Note to self: The 'modern' packages tibble and broom are only used
## in this file

#' @importFrom tibble as_tibble
#' @importFrom broom tidy

#' @title Tidy a linest object
#' @description Tidy summarizes information about the components of the object.
#' @name tidy-linest
#'
#' @param x A 'linest_class' object (produced by \code{linest} methods).
#' @param conf.int Should confidence intervals be added.
#' @param conf.level Desired confidence level.
#' @param ... Additional arguments; currently not used.
#' @export
tidy.linest_class <- function(x, conf.int = FALSE, conf.level = 0.95, ...){
    co <- stats::coef(x)
    rownames(co) <- NULL
    
    if (ncol(co)==5){ ## There are degreeso of freedom in the output.
        co <- co[,c(1,2,3,5,4)]
        nn <- c("estimate", "std.error", "statistic", "p.value", "df")
    } else
        nn <- c("estimate", "std.error", "statistic", "p.value")

    names(co) <- nn

    if (conf.int){
        ci <- .ci_fun(co, level=conf.level)
        colnames(ci) <- c("conf.low", "conf.high")    
        co <- cbind(co, ci)
    }
    as_tibble(co)
}

#' @title Tidy an esticon object
#' @description Tidy summarizes information about the components of the object.
#' @name tidy-esticon
#'
#' @param x A 'esticon_class' object (produced by \code{esticon} methods).
#' @param conf.int Should confidence intervals be added.
#' @param conf.level Desired confidence level.
#' @param ... Additional arguments; currently not used.
#' @export
tidy.esticon_class <- function(x, conf.int = FALSE, conf.level = 0.95, ...){
    co <- x[,1:6]
    rownames(co) <- NULL

    if (conf.int){
        ci <- confint(x, level=conf.level)
        colnames(ci) <- c("conf.low", "conf.high")    
        co <- cbind(co, ci)
    }
    as_tibble(co)
}
