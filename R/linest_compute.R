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
linest <- function(object, L=NULL, level=0.95, ...){
    UseMethod("linest")
}


linest.lm <- function(object, L=NULL, level=0.95, ...){
    bhat <- coef(object)
    if (is.null(L))
        L <- .defineL( bhat )
    if (!is.matrix(L))
        L <- matrix(L, nrow=1)

    is.est <- is_estimable(L, null_basis( object ))

    ##VV0  <- vcov(object)
    VV0  <- vcov(object, complete=FALSE)
    ddf  <- object$df.residual
    ddf.vec <- rep(ddf, nrow( L ))
    res    <- .getLb( L, bhat, VV0, ddf.vec, is.est)

    p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)

    qq  <- qt(1-(1-level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]
    #res <- cbind(res, p.value, lwr, upr)
    res <- cbind( res, p.value )

    .finalize_linest(res, L)
}

linest.glm <- function(object, L=NULL, level=0.95, type=c("link", "response"), ...){
    type <- match.arg(type)
    bhat <- coef(object)
    if (is.null(L))
        L <- .defineL( bhat )
    if (!is.matrix(L))
        L <- matrix(L, nrow=1)

    is.est <- is_estimable(L, null_basis( object ))

    ##VV0  <- vcov(object)
    VV0  <- vcov(object, complete=FALSE)
    
    ddf.vec <- rep(object$df.residual, nrow( L ))
    res     <- .getLb( L, bhat, VV0, ddf.vec, is.est)

    if(family(object)[1] %in% c("poisson","binomial","quasipoisson","quasibinomial")){
        p.value <- 2*pnorm(abs(res[,"t.stat"]), lower.tail=FALSE)
        qq <- qnorm(1-(1-level)/2)
        colnames(res)[4] <- "z.stat"
        res <- res[,-3] # NO df's
    } else {
        p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)
        qq <- qt(1-(1-level)/2, df=res[,"df"])
    }

    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]

    if (type=="response"){
        fit    <- family(object)$linkinv(res[,"estimate"])
        se.fit <- res[,"se"] * abs(family(object)$mu.eta(res[,"estimate"]))
        res[,"estimate"]  <- fit
        res[,"se"] <- se.fit
        lwr <- family(object)$linkinv(lwr)
        upr <- family(object)$linkinv(upr)
    }

    ##res <- cbind(res, p.value, lwr, upr)
    res <- cbind( res, p.value )
    .finalize_linest(res, L)

}


linest.geeglm <- function(object, L=NULL, level=0.95, type=c("link","response"), ...){
    type <- match.arg(type)
    bhat <- coef(object)
    if (is.null(L))
        L <- .defineL( bhat )
    if (!is.matrix(L))
        L <- matrix(L, nrow=1)

    is.est <- is_estimable(L, null_basis( object ))

    VV0  <- summary(object)$cov.scaled
    ddf.vec <- rep(1, nrow(L))
    res     <- .getLb( L, bhat, VV0, ddf.vec, is.est)

    p.value <- 2*pnorm(abs(res[,"t.stat"]), lower.tail=FALSE)
    qq <- qnorm(1-(1-level)/2)
    colnames(res)[4] <- "z.stat"
    res <- res[,-3, drop=FALSE] # NO df's
    ## print("00000000000000000")
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]


    if (type=="response"){
        fit    <- family(object)$linkinv(res[,"estimate"])
        se.fit <- res[,"se"] * abs(family(object)$mu.eta(res[,"estimate"]))
        res[,"estimate"]  <- fit
        res[,"se"] <- se.fit
        lwr <- family(object)$linkinv(lwr)
        upr <- family(object)$linkinv(upr)
    }

    ## res <- cbind(res, p.value, lwr, upr)
    res <- cbind( res, p.value )
    .finalize_linest(res, L)
}

linest.lmerMod <- function(object, L=NULL, level=0.95, adjust.df=TRUE, ...){

    bhat <- lme4::fixef(object)

    if (is.null(L))
        L <- .defineL( bhat )
    if (!is.matrix(L))
        L <- matrix(L, nrow=1)

    is.est <- is_estimable(L, null_basis( object ))

    if (adjust.df){
        if (requireNamespace("pbkrtest", quietly=TRUE)){
            ##VVu  <- vcov(object)
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
        ##VV  <- vcov(object)
        VV  <- vcov(object, complete=FALSE)
    }


    res     <- .getLb( L, bhat, VV, ddf.vec, is.est)
    p.value <- 2*pt(abs(res[,"t.stat"]), df=res[,"df"], lower.tail=FALSE)
    qq  <- qt(1-(1-level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]
    ##res <- cbind(res, p.value, lwr, upr)
    res <- cbind( res, p.value )
    .finalize_linest(res, L)

}

linest.merMod <- function(object, L=NULL, level=0.95, ...){
    cl <- match.call()
    cl[[1]] <- as.name("linest.lmerMod")
    cl$adjust.df <- FALSE
    eval(cl)
}



### UTILITIES ###

.defineL <- function( bhat ){
    L <- diag( 1, length( bhat ) )
    rownames( L ) <- names( bhat )
    L
}

.createL <- function(L, bhat){
    if (is.null(L))
        L <- .defineL( bhat )
    L
}

.getLb <- function(L, bhat, VV, ddf.vec, is.est, level=0.95){
    #' cat(".getLb")
    #' print(attributes(L))
    off <- attr(L, "offset")
    #' print(off)

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

    colnames(res) <- c("estimate","se","df")
    t.stat        <- res[,"estimate"] / res[,"se"]
    cbind(res, t.stat)
}





.finalize_linest <- function(.coef, L){
    if (!is.null(rownames(L)))
        rownames(.coef) <- rownames(L)
    .coef <- as.data.frame(.coef)
    res  <- list(coef=.coef, grid=attr(L, "grid"), L=L)
    class(res) <- "linest_class"
    res
}

#' @rdname linest
coef.linest_class <- function (object, ...) {
    object$coef
}

print.linest_class <- function (x, ...){
    cat("Coefficients:\n")
    printCoefmat(x$coef)
}

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




#' @rdname linest
#' @param parm Specification of the parameters estimates for which
#'     confidence inctervals are to be calculated.
confint.linest_class <- function (object, parm, level = 0.95, ...) 
{
    object <- coef(object) 
    cf <- object$estimate #cf <- coef(object)
    pnames <- 1:nrow(object) ##names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- a ## FIXME ::: gives problems. stats:::format.perc(a, 3)
    if (!is.null(DF <- coef(object)$df))
        fac <- qt(a, DF)
    else
        fac <- qnorm(a)
       
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
        pct))
    #ses <- sqrt(diag(vcov(object)))[parm]
    ses  <- object$se[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
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
