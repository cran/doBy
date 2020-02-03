#' @title Extract components from a formula with "conditioning bar"
#' 
#' @description Extract components from a formula with the form
#'     \code{y ~ x1 + ... + xn | g1 + ... + gm}
#' 
#' @param form A formula of the form \code{y ~ x1 + ... + xn | g1 + ... + gm}
#' @return If the formula is \code{y ~ x1 + x2 | g1 + g2} the result is
#'     \item{model}{\code{y ~ x1 + x2}} \item{groups}{\code{ g1 + g2}}
#'     \item{groupFormula}{\code{~ g1 + g2}}
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' gf <- parseGroupFormula(y ~ x1 + x2 | g1 + g2)
#' gf 
#' 
#' @export parseGroupFormula
parseGroupFormula <- function(form)
{
    if (!inherits(form, "formula") || length(form) != 3)
        stop("formula must be a two-sided formula object")
    rhs <- form[[3]]

    if (!inherits(rhs, "call") || rhs[[1]] != as.symbol('|'))
        stop("rhs of formula must be a conditioning expression")
    form[[3]] <- rhs[[2]]
    groups <- rhs[[3]]
    grpFormula <- as.formula(paste("~", deparse(groups)))
    list(model = form, groups = groups, groupFormula=grpFormula)
}

#' @title Convert right hand sided formula to a list
#' @description Convert right hand sided formula to a list
#' @param f A right hand sided formula

.rhsf2list <- function (f) {
    if (is.character(f))    list(f) 
    else if (is.numeric(f)) lapply(list(f), "as.character")
    else if (is.list(f))    lapply(f, "as.character")
    else {
        .xxx. <- f[[length(f)]]
        f1 <- unlist(strsplit(paste(deparse(.xxx.), collapse = ""), 
                              " *\\+ *"))
        f2 <- unlist(lapply(f1, strsplit, " *\\* *| *: *| *\\| *"), 
                     recursive = FALSE)
        f2
    }
}
