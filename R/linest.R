
linest <- function(object, K, level=0.95, ...){
    UseMethod("linest")
}

linest.lm <- function(object, K, level=0.95, ...){
    is.est <- .is_estimable(K, .get_null_basis( object ))

    bhat <- coef(object)
    VV0  <- vcov(object)
    ddf  <- object$df.residual
    ddf.vec <- rep(ddf, nrow( K ))
    res    <- .getKb( K, bhat, VV0, ddf.vec, is.est)

    p.value <- 2*pt(res[,"t.stat"], df=res[,"df"], lower.tail=FALSE)

    qq<-qt(1-(1-level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]
    res <- cbind(res, p.value, lwr, upr)

    .finalize(res, K)

}

linest.glm <- function(object, K, level=0.95, type=c("link","response"), ...){
    type <- match.arg(type)
    is.est <- .is_estimable(K, .get_null_basis( object ))

    bhat <- coef(object)
    VV0  <- vcov(object)
    ddf.vec <- rep(object$df.residual, nrow( K ))
    res     <- .getKb( K, bhat, VV0, ddf.vec, is.est)

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

    res <- cbind(res, p.value, lwr, upr)

    .finalize(res, K)

}


linest.geeglm <- function(object, K, level=0.95, type=c("link","response"), ...){
    type <- match.arg(type)
    is.est <- .is_estimable(K, .get_null_basis( object ))

    bhat <- coef(object)
    VV0  <- summary(object)$cov.scaled
    ddf.vec <- rep(1, nrow(K))
    res     <- .getKb( K, bhat, VV0, ddf.vec, is.est)

    p.value <- 2*pnorm(abs(res[,"t.stat"]), lower.tail=FALSE)
    qq <- qnorm(1-(1-level)/2)
    colnames(res)[4] <- "z.stat"
    res <- res[,-3] # NO df's

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

    res <- cbind(res, p.value, lwr, upr)

    .finalize(res, K)
}

linest.lmerMod <- function(object, K, level=0.95, adjust.df=TRUE, ...){
    is.est <- .is_estimable(K, .get_null_basis( object ))

    bhat <- fixef(object)

    if (adjust.df){
        if (require(pbkrtest)){
            VV0  <- vcovAdj(object)
            ddf.vec <- unlist(lapply(1:nrow(K),
                                     function(ii) ddf_Lb(VV0, K[ii,])))
                                     ##function(ii) ddf_Lb(VV0, VV0, K[ii,], 0)))
        } else {
            stop("adjustment of degrees of freedom requires that 'pbkrtest' is installed")
        }
    } else {
        a <- logLik(object)
        ddf<-attributes(a)$nall - attributes(a)$df
        ddf.vec <- rep(ddf, length(bhat))
        VV0 <- vcov(object)
    }

    res    <- .getKb( K, bhat, VV0, ddf.vec, is.est)
    p.value <- 2*pt(res[,"t.stat"], df=res[,"df"], lower.tail=FALSE)
    qq<-qt(1-(1-level)/2, df=res[,"df"])
    lwr <- res[,"estimate"] - qq * res[,"se"]
    upr <- res[,"estimate"] + qq * res[,"se"]
    res <- cbind(res, p.value, lwr, upr)

    .finalize(res, K)

}

linest.merMod <- function(object, K, level=0.95, ...){
    cl <- match.call()
    cl[[1]] <- as.name("linest.lmerMod")
    cl$adjust.df <- FALSE
    eval(cl)
}





### UTILITIES ###

.getK <- function(object, effect=NULL, at=NULL){
    ## cat(".getK()\n");
    ## cat("effect:"); print(effect);
    ## cat("at:"); print(at)

    ## print(K)
    ## if (is.null(K)){

    ## } else {
    ##     if (!inherits(K, "matrix")) stop("'K' must be a matrix\n")
    ## }
    ## K

    K <- LSmatrix(object, effect=effect, at=at)
    ## print(K)
    ## cat("done - getK\n")
    K
}

.getKb <- function(K, bhat, VV, ddf.vec, is.est, level=0.95){
    used       <- which(!is.na(bhat))
    bhat.used  <- bhat[used]
    K   <- K[, used, drop=FALSE]
    res <- matrix(NA, nrow=nrow(K), ncol=3)
    for (ii in 1:nrow(res)){
        kk <- K[ii,]
        if (is.est[ii]){
            est  <- sum(kk * bhat.used)
            se   <- sqrt(sum(kk * (VV %*% kk)))
            df2  <- ddf.vec[ii]
            res[ii,] <- c(est, se, df2)
        }
    }
    colnames(res) <- c("estimate","se","df")
    t.stat <- res[,"estimate"]/res[,"se"]
    cbind(res, t.stat)
}


.finalize <- function(.coef, K){
    res   <- list(coef=.coef, grid=attr(K,"grid"), K=K)
    class(res) <- "LSmeans"
    res
}

print.LSmeans <- function(x, ...){
    print(cbind(x$coef, x$grid))
    invisible(x)
}

