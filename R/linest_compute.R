## #################################################################
##
## Calculates L %*% beta and related quantities for various
## type of model objects.
##
## #################################################################

#' @title Compute linear estimates
#' 
#' @description Compute linear estimates for a range of models. One example of
#'     linear estimates is population means (also known as LSMEANS).
#'
#' @name linest
#' 
#' @aliases linest linest.lm linest.glm linest.geeglm linest.lmerMod
#'     linest.merMod linest.default
#' @param object Model object
#' @param L Either \code{NULL} or a matrix with p columns where p is
#'     the number of parameters in the systematic effects in the
#'     model. If \code{NULL} then \code{L} is taken to be the p times
#'     p identity matrix
#' @param level The level of the (asymptotic) confidence interval.
#' @param confint Should confidence interval appear in output.
#' @param ...  Additional arguments; currently not used.
#' @return A dataframe with results from computing the contrasts.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{LSmeans}}, \code{\link{LE_matrix}}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## Make balanced dataset
#' dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## Make unbalanced dataset
#' #   'BB' is nested within 'CC' so BB=1 is only found when CC=1
#' #   and BB=2,3 are found in each CC=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
#' 
#' mod.bal  <- lm(y ~ AA + BB * CC, data=dat.bal)
#' mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)
#' 
#' L <- LE_matrix(mod.nst, effect=c("BB", "CC"))
#' linest( mod.nst, L )
#' 
#' @export linest

#' @export
linest <- function(object, L=NULL, ...){
    UseMethod("linest")
}

#' @export
linest.lm <- function(object, L=NULL, ...){
    bhat <- coef(object)
    L  <- .constructL(L, bhat)    
    is.est <- is_estimable(L, null_basis( object ))
    VV0  <- vcov(object, complete=FALSE)

    ddf.vec <- rep(object$df.residual, nrow( L ))
    res    <- .getLb2( L, bhat, VV0, ddf.vec, is.est)
    
    p.value <- 2 * pt(abs(res[,"statistic"]), df=res[,"df"], lower.tail=FALSE)
    res <- as.data.frame(cbind(res, p.value))
    
    .finalize_linest(res, L)
}


#' @export
linest.glm <- function(object, L=NULL, ...){


    bhat <- coef(object)
    L  <- .constructL(L, bhat)        
    is.est <- is_estimable(L, null_basis( object ))
    VV0  <- vcov(object, complete=FALSE)
    ddf.vec <- rep(object$df.residual, nrow( L ))
    res     <- .getLb2(L, bhat, VV0, ddf.vec, is.est)

    if( family(object)[1] %in% c("poisson", "binomial", "Gamma", "quasipoisson", "quasibinomial")){
        p.value <- 2*pnorm(abs(res[,"statistic"]), lower.tail=FALSE)
        res <- res[, -4] # NO df's
        ## qq <- qnorm(1-(1-level)/2)
    } else {
        p.value <- 2*pt(abs(res[,"statistic"]), df=res[,"df"], lower.tail=FALSE)
        ## qq <- qt(1-(1-level)/2, df=res[,"df"])
    }
    res <- as.data.frame(cbind(res, p.value))
    .finalize_linest(res, L)
}

#' @export
linest.geeglm <- function(object, L=NULL, ...){

    bhat <- coef(object)
    L  <- .constructL(L, bhat)    
    is.est <- is_estimable(L, null_basis( object ))
    VV0  <- summary(object)$cov.scaled
    ddf.vec <- rep(1, nrow(L))
    res     <- .getLb2( L, bhat, VV0, ddf.vec, is.est)

    res <- res[, 1:3]
    p.value <- 2*pnorm(abs(res[,"statistic"]), lower.tail=FALSE)
    res <- as.data.frame(cbind(res, p.value))
    .finalize_linest(res, L)
}

#' @export
linest.lmerMod <- function(object, L=NULL, adjust.df=TRUE, ...){

    bhat <- lme4::fixef(object)
    L  <- .constructL(L, bhat)    
    is.est <- is_estimable(L, null_basis( object ))

    if (adjust.df){
        if (requireNamespace("pbkrtest", quietly=TRUE)){
            VVu  <- vcov(object, complete=FALSE)
            VV   <- pbkrtest::vcovAdj(object)
            ddf.vec <- unlist(lapply(1:nrow(L),
                                     function(ii) pbkrtest::ddf_Lb(VV , L[ii,], VVu)))
        } else {
            stop("adjustment of degrees of freedom requires that 'pbkrtest' is installed")
        }
    } else {
        a   <- logLik(object)
        ddf <- attributes(a)$nall - attributes(a)$df
        ddf.vec <- rep(ddf, length(bhat))
        VV  <- vcov(object, complete=FALSE)
    }

    res     <- .getLb2( L, bhat, VV, ddf.vec, is.est)
    p.value <- 2*pt(abs(res[,"statistic"]), df=res[,"df"], lower.tail=FALSE)

    res <- as.data.frame(cbind(res, p.value))
    .finalize_linest(res, L)
}

#' @export
linest.merMod <- function(object, L=NULL, conf.int=FALSE, conf.level=0.95, ...){
    cl <- match.call()
    cl[[1]] <- as.name("linest.lmerMod")
    cl$adjust.df <- FALSE
    eval(cl)
}

.finalize_linest <- function(.coef, L){

    rownames(.coef) <- NULL
    
    if (!is.null(rownames(L)))
        rownames(.coef) <- rownames(L)

    ## .coef <- as.data.frame(.coef)
    
    res  <- list(coef=.coef, grid=attr(L, "grid"), L=L)
    class(res) <- "linest_class"
    res
}


.getLb2 <- function(L, bhat, VV, ddf.vec, is.est, level=0.95){
    off <- attr(L, "offset")

    used       <- which(!is.na(bhat))
    bhat.used  <- bhat[used]
    L   <- L[, used, drop=FALSE]
    res <- matrix(NA, nrow=nrow(L), ncol=3)
    
    for (ii in 1:nrow(res)){
        kk <- L[ii,]
        if (is.est[ii]){
            est  <- sum(kk * bhat.used)
            se   <- sqrt(sum(kk * (VV %*% kk)))
            df2  <- ddf.vec[ii]
            res[ii,] <- c(est, se, df2)
        }
    }

    if (!is.null(off))
        res[,1] <- res[,1] + off[[1]]


    
    colnames(res) <- c("estimate","std.error","df") 
    statistic        <- res[,"estimate"] / res[,"std.error"]
    res <- cbind(res[,1:2, drop=FALSE], statistic, df=res[,3])

    rownames(res) <- NULL
    ##print(res)
    res
}


#' @export
#' @rdname linest
#' @param x A 'linest_class' object (produced by \code{linest} methods).
#' @param conf.int Should confidence intervals be added.
#' @param conf.level Desired confidence level.
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


.ci_fun <- function (object, parm, level = 0.95, ...) 
{
    ##object <- coef(object)
    cf <- object$estimate #cf <- coef(object)
    pnames <- 1:nrow(object) ##names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- a 
    DF <- object$df
    if (!is.null(DF))
        fac <- t(sapply(DF, function(d) qt(a, d)))
    else
        fac <- matrix(rep(qnorm(a), length(parm)), nrow=2)
       
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ses  <- object$std.error[parm]
    #print(list(a=a, DF=DF, ses=ses, fac=fac, cf=cf[parm], ci=ci))
    #ci[] <- cf[parm] + ses %o% fac
    ci[] <- cf[parm] + fac * ses
    ci
}


### UTILITIES ###

#' @export
#' @rdname linest
#' @param parm Specification of the parameters estimates for which
#'     confidence inctervals are to be calculated.
confint.linest_class <- function (object, parm, level = 0.95, ...) 
{
    co <- coef(object) 
    .ci_fun(co, parm, level, ...)
}

.constructL <- function(L, bhat){
    .defineL <- function( bhat ){
        L <- diag(1, length(bhat))
        rownames(L) <- names(bhat)
        L
    }

    if (is.null(L))
        L <- .defineL(bhat)
    if (!is.matrix(L))
        L <- matrix(L, nrow=1)
    L
}

#' @export
#' @rdname linest
coef.linest_class <- function (object, ...) {
    object$coef
}

#' @export
print.linest_class <- function (x, ...){
    cat("Coefficients:\n")
    printCoefmat(x$coef)
}

#' @export
#' @rdname linest
summary.linest_class <- function (object, ...) 
{
    cat("Coefficients:\n")
    printCoefmat(object$coef)
    cat("\n")

#    if (!is.null(object$grid)){
        cat("Grid:\n")
        print(object$grid)
        cat("\n")
#    }

    cat("L:\n")
    print(object$L)
    cat("\n")

    invisible(object)
}


setOldClass("linest_class")
setAs("linest_class", "data.frame",   function(from)
{
    out <- as.data.frame(coef(from))
    if (!is.null(from$grid))
        out <- cbind(out, from$grid)
    attr(out, "K") <- from$K
    out
})





## .finalize_linest <- function(.coef, L, lwr, upr, confint){
##     if (!is.null(rownames(L)))
##         rownames(.coef) <- rownames(L)
##     .coef <- as.data.frame(.coef)
##     if (confint)
##         .coef <- cbind(.coef, lwr, upr)
    
##     res  <- list(coef=.coef, grid=attr(L, "grid"), L=L)
##     class(res) <- "linest_class"
##     res
## }
