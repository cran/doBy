#' @title Fast summary of microbenchmark object
#'
#' @description Fast summary of microbenchmark object. The default
#'     summary method from the microbenchmark package is fairly slow
#'     in producing a summary (due to a call to a function from the
#'     multcomp package.)
#' 
#' @name mb_summary
#'
#' @param object A microbenchmark object
#' @param unit The time unit to be used
#' @param add.unit Should time unit be added as column to resulting
#'     dataframe.
#' @param ... Additional arguments; currently not used.
NULL

#' @rdname mb_summary
#' @export
mb_summary <- function (object, unit, add.unit=TRUE, ...) 
{
    if (missing(unit)) {
        unit <- if (!is.null(attr(object, "unit"))) 
            attr(object, "unit")
        else getOption("microbenchmark.unit", "t")
    }
    if (unit != "relative") 
        object$time <- .convert_to_unit(object$time, unit)
    res <- aggregate(time ~ expr, object, function(z) {
        tmp <- c(stats::fivenum(z), mean(z), length(z))
        tmp[c(1, 2, 6, 3, 4, 5, 7)]
    })
    res <- cbind(res$expr, as.data.frame(res$time))
    colnames(res) <- c("expr", "min", "lq", "mean", "median", 
        "uq", "max", "neval")
    if (unit == "relative") {
        min <- res[which.min(res$median), , drop = FALSE]
        min$neval <- 1
        res[-1] <- res[-1]/as.list(min[-1])
        attr(res, "unit") <- "relative"
    }
    else {
        attr(res, "unit") <- attr(object$time, "unit")
    }
    ## if (requireNamespace("multcomp", quietly = TRUE) && nrow(res) > 
    ##     1 && all(res["neval"] > 1)) {
    ##     tryCatch({
    ##         ops <- options(warn = -1)
    ##         mdl <- lm(time ~ expr, object)
    ##         comp <- multcomp::glht(mdl, multcomp::mcp(expr = "Tukey"))
    ##         res$cld <- multcomp::cld(comp)$mcletters$monospacedLetters
    ##     }, error = function(e) FALSE, finally = options(ops))
    ## }

    if (add.unit)
        res$unit <- attr(res, "unit")
    res
}


.convert_to_unit <- 
function (x, unit = c("ns", "us", "ms", "s", "t", "hz", "khz", 
    "mhz", "eps", "f")) 
{
    unit <- match.arg(unit)
    switch(unit, t = unit <- sprintf("%ss", .find_prefix(x * 1e-09, 
        minexp = -9, maxexp = 0, mu = FALSE)), f = unit <- sprintf("%shz", 
        .find_prefix(1e+09/x, minexp = 0, maxexp = 6, mu = FALSE)))
    unit <- tolower(unit)
    switch(unit, ns = {
        attr(x, "unit") <- "nanoseconds"
        unclass(x)
    }, us = {
        attr(x, "unit") <- "microseconds"
        unclass(x/1000)
    }, ms = {
        attr(x, "unit") <- "milliseconds"
        unclass(x/1e+06)
    }, s = {
        attr(x, "unit") <- "seconds"
        unclass(x/1e+09)
    }, eps = {
        attr(x, "unit") <- "evaluations per second"
        unclass(1e+09/x)
    }, hz = {
        attr(x, "unit") <- "hertz"
        unclass(1e+09/x)
    }, khz = {
        attr(x, "unit") <- "kilohertz"
        unclass(1e+06/x)
    }, mhz = {
        attr(x, "unit") <- "megahertz"
        unclass(1000/x)
    }, stop("Unknown unit '", unit, "'."))
}

.find_prefix <- 
function (x, f = min, minexp = -Inf, maxexp = Inf, mu = TRUE) 
{
    prefixes <- c("y", "z", "a", "f", "p", "n", "u", "m", "", 
        "k", "M", "G", "T", "P", "E", "Z", "Y")
    if (mu) 
        prefixes[7] <- "mu"
    if (is.numeric(minexp)) 
        minexp <- floor(minexp/3)
    if (is.numeric(minexp)) 
        maxexp <- floor(maxexp/3)
    e3 <- floor(log10(f(x))/3)
    e3 <- max(e3, minexp, -8)
    e3 <- min(e3, maxexp, 8)
    prefixes[e3 + 9]
}


#' @rdname mb_summary
#' @export
summary_mb <- mb_summary
