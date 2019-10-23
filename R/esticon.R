#' @title Contrasts for lm, glm, lme, and geeglm objects
#' 
#' @description Computes linear functions (i.e. weighted sums) of the estimated
#'     regression parameters. Can also test the hypothesis, that such a function
#'     is equal to a specific value.
#'
#' @name esticon
#' 
#' @details Let the estimated parameters of the model be
#' \deqn{\beta_1, \beta_2, \dots, \beta_p}
#'
#' A linear function of the estimates is of the form \deqn{l=\lambda_1
#'     \beta_1+\lambda_2 \beta_2+ \dots+\lambda_p \beta_p} where
#'     \eqn{\lambda_1, \lambda_2, \dots,\lambda_p} is specified by the
#'     user.
#' 
#' The esticon function calculates l, its standard error and by default also a
#' 95 pct confidence interval.  It is sometimes of interest to test the
#' hypothesis \eqn{H_0: l=\beta_0} for some value \eqn{\beta_0}
#' given by the user. A test is provided for the hypothesis \eqn{H_0:
#' l=0} but other values of \eqn{\beta_0} can be specified.
#' 
#' In general, one can specify r such linear functions at one time by
#' speficying L to be an \eqn{r\times p} matrix where each row consists
#' of p numbers \eqn{\lambda_1,\lambda_2,\dots, \lambda_p}. Default is
#' then that \eqn{\beta_0} is a p vector of 0s but other values can be
#' given.
#' 
#' It is possible to test simulatneously that all speficied linear functions
#' are equal to the corresponding values in \eqn{\beta_0}.
#' 
#' For computing contrasts among levels of a single factor, 'contrast.lm' may
#' be more convenient.
#' 
#' @aliases esticon esticon.geeglm esticon.glm esticon.gls esticon.lm
#'     esticon.lme esticon.mer esticon.merMod esticon.coxph
#' 
#' @param obj Regression object (of type lm, glm, lme, geeglm).
#' @param x A linear contrast object (as returned by \code{esticon()}. 
#' @param L Matrix (or vector) specifying linear functions of the regresson
#'     parameters (one
#'     linear function per row).  The number of columns must match the number of
#'     fitted regression parameters in the model. See 'details' below.
#' @param parm a specification of which parameters are to be given
#'          confidence intervals, either a vector of numbers or a vector
#'         of names.  If missing, all parameters are considered.
#' @param beta0 A vector of numbers
#' @param conf.int TRUE
#' @param conf.level The desired confidence level.
#' @param level The confidence level
#' @param joint.test Logical value. If TRUE a 'joint' Wald test for the
#'     hypothesis L beta = beta0 is made. Default is that the 'row-wise' tests are
#'     made, i.e. (L beta)i=beta0i.  If joint.test is TRUE, then no confidence
#'     inteval etc. is calculated.
#' @param ... Additional arguments; currently not used.
#' @return Returns a matrix with one row per linear function.  Columns contain
#'     estimated coefficients, standard errors, t values, degrees of freedom,
#'     two-sided p-values, and the lower and upper endpoints of the 1-alpha
#'     confidence intervals.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' data(iris)
#' lm1  <- lm(Sepal.Length ~ Sepal.Width + Species + Sepal.Width : Species, data=iris)
#' ## Note that the setosa parameters are set to zero
#' coef(lm1)
#' 
#' ## Estimate the intercept for versicolor
#' lambda1 <- c(1, 0, 1, 0, 0, 0)
#' esticon(lm1, L=lambda1)
#' 
#' ## Estimate the difference between versicolor and virgica intercept
#' ## and test if the difference is 1
#' lambda2 <- c(0, 1, -1, 0, 0, 0)
#' esticon(lm1, L=lambda2, beta0=1)
#' 
#' ## Do both estimates at one time
#' esticon(lm1, L=rbind(lambda1, lambda2), beta0=c(0, 1))
#' 
#' ## Make a combined test for that the difference between versicolor and virgica intercept
#' ## and difference between versicolor and virginica slope is zero:
#' lambda3 <- c(0, 0, 0, 0, 1, -1)
#' esticon(lm1, L=rbind(lambda2, lambda3), joint.test=TRUE)
#' 
#' # Example using esticon on coxph objects (thanks to Alessandro A. Leidi).
#' # Using dataset 'veteran' in the survival package
#' # from the Veterans' Administration Lung Cancer study
#' 
#' if (require(survival)){
#' data(veteran)
#' sapply(veteran, class)
#' levels(veteran$celltype)
#' attach(veteran)
#' veteran.s <- Surv(time, status)
#' coxmod <- coxph(veteran.s ~ age + celltype + trt, method='breslow')
#' summary(coxmod)
#' 
#' # compare a subject 50 years old with celltype 1
#' # to a subject 70 years old with celltype 2
#' # both subjects on the same treatment
#' AvB <- c(-20, -1, 0, 0, 0)
#' 
#' # compare a subject 40 years old with celltype 2 on treat=0
#' # to a subject 35 years old with celltype 3 on treat=1
#' CvB <- c(5, 1, -1, 0, -1)
#' 
#' est <- esticon(coxmod, L=rbind(AvB, CvB))
#' est
#' ##exp(est[, c(2, 7, 8)])
#' }
#' 
#' 
#' @export esticon
esticon <- function(obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...)
  UseMethod("esticon")

#' @rdname esticon
esticon.gls <- function (obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...){
    if (joint.test) .wald(obj, L, beta0)
    else {
        stat.name <- "X2.stat"
        ##vcv <- vcov(obj)
        vcv <- vcov(obj, complete=FALSE)
        coef.mat  <- matrix(coef(obj))
        df  <- 1
        .esticonCore(obj, L, beta0, conf.int=conf.int,level,coef.mat,vcv,df,stat.name)
    }
}

#' @rdname esticon
esticon.geeglm <- function (obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...){
    if (joint.test) .wald(obj, L, beta0)
    else {
        stat.name <- "X2.stat"
        coef.mat  <- summary(obj)$coef
        vcv <- summary(obj)$cov.scaled
        df  <- 1
        .esticonCore(obj, L, beta0, conf.int=conf.int,level,coef.mat,vcv,df,stat.name)
    }
}

#' @rdname esticon
esticon.lm <- function (obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...){
    if (joint.test) .wald(obj, L, beta0)
    else {
        stat.name <- "t.stat"
        coef.mat  <- summary.lm(obj)$coefficients
        coef.vec  <- coef(obj)
        vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
        df  <- obj$df.residual
        .esticonCore(obj, L, beta0, conf.int=conf.int, level, coef.mat, vcv, df, stat.name, coef.vec=coef.vec)
  }
}

#' @rdname esticon
esticon.glm <- function (obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...){
    if (joint.test) .wald(obj, L, beta0)
    else {
        coef.mat <- summary.lm(obj)$coefficients
        vcv <- summary(obj)$cov.scaled
        if(family(obj)[1] %in% c("poisson", "binomial")){
            stat.name <- "X2.stat"
            df <- 1
        } else {
            stat.name <- "t.stat"
            df <- obj$df.residual
        }
        .esticonCore(obj, L, beta0, conf.int=conf.int,level=level,coef.mat,vcv,df,stat.name)
    }
}

#' @rdname esticon
esticon.mer <- esticon.merMod <- function (obj, L, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE,...){
    if (joint.test) .wald(obj, L, beta0)
    else {
        stat.name <- "X2.stat"
        coef.mat  <- matrix(lme4::fixef(obj))
        ##vcv <- as.matrix(vcov(obj))
        vcv <- as.matrix(vcov(obj, complete=FALSE))

        
        df  <- 1
        .esticonCore(obj, L, beta0, conf.int=conf.int, level, coef.mat, vcv, df, stat.name)
    }
}

#' @rdname esticon
esticon.coxph <-
    function (obj, L, beta0, conf.int = TRUE, level = 0.95, joint.test = FALSE, ...){
        if (joint.test == TRUE) .wald(obj, L, beta0)
        else {
            cf <- summary(obj)$coefficients
            vcv <- obj$var
            stat.name <- "X2.stat"
            df <- 1
            .esticonCore(obj, L, beta0, conf.int = conf.int, level = level,
                         cf, vcv, df, stat.name)
        }
    }


### ######################################################
###
### esticon.lme needs better testing at some point
###
### ######################################################

#' @rdname esticon
esticon.lme <- function (obj, L, beta0, conf.int = NULL, level=0.95, joint.test=FALSE,...){
  warning("The esticon function has not been thoroughly teste on 'lme' objects")
  if (joint.test) .wald(obj, L, beta0)
  else {
      stat.name <- "t.stat"
      coef.mat <- summary(obj)$tTable
      rho <- summary(obj)$cor
      vcv <- rho * outer(coef.mat[, 2], coef.mat[, 2])
      tmp <- L
      tmp[tmp == 0] <- NA
      df.all <- t(abs(t(tmp) * obj$fixDF$X))
      df <- apply(df.all, 1, min, na.rm = TRUE)
      problem <- apply(df.all != df, 1, any, na.rm = TRUE)
      if (any(problem))
          warning(paste("Degrees of freedom vary among parameters used to ",
                        "construct linear contrast(s): ",
                        paste((1:nrow(tmp))[problem],
                              collapse = ","),
                        ". Using the smallest df among the set of parameters.",
                        sep = ""))
      df <- min(df)
      .esticonCore(obj, L, beta0, conf.int=conf.int,level,coef.mat,vcv,df,stat.name)
  }
}


### .functions below here ###

.wald <- function (obj, L, beta0)
{
    if (!is.matrix(L) && !is.data.frame(L))
        L <- matrix(L, nrow = 1)

    if (missing(beta0))
      beta0 <- rep(0, nrow(L))

    df <- nrow(L)
    if ("geese" %in% class(obj)) {
      coef.mat  <- obj$beta
      vcv <- obj$vbeta
    } else if ("geeglm" %in% class(obj)) {
      coef.mat  <- obj$coef
      vcv <- summary(obj)$cov.scaled
    } else if ("gls" %in% class(obj)) {
        ##vcv <- vcov(obj)
        vcv <- vcov(obj, complete=FALSE)
      coef.mat  <- matrix(coef(obj))
    } else if ("gee" %in% class(obj)) {
      coef.mat  <- obj$coef
      vcv <- obj$robust.variance
    }
    else if ("lm" %in% class(obj)) {
      coef.mat  <- summary.lm(obj)$coefficients[, 1]
      vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
      if ("glm" %in% class(obj)) {
        vcv <- summary(obj)$cov.scaled
      }
    }
    else if ("coxph" %in% class(obj)) {
      coef.mat <- obj$coef
      vcv <- obj$var
    }
    else
      stop("obj must be of class 'lm', 'glm', 'aov', 'gls', 'gee', 'geese', 'coxph'")
    u      <- (L %*% coef.mat)-beta0
    vcv.u  <- L %*% vcv %*% t(L)
    W      <- t(u) %*% solve(vcv.u) %*% u
    prob   <- 1 - pchisq(W, df = df)
    out <- as.data.frame(cbind(W, df, prob))
    names(out) <- c("X2.stat", "DF", "Pr(>|X^2|)")
    as.data.frame(out)
}


.esticonCore <- function (obj, L, beta0, conf.int = NULL, level, coef.mat,
                          vcv, df, stat.name, coef.vec=coef.mat[,1]) {

    ## Notice
    ## coef.mat: summary(obj)$coefficients
    ## coef.vec: coef(obj)
    ## cl <- match.call(); print(cl);  print(coef.mat)
    
    if (missing(L)) stop("Contrast matrix 'L' is missing")
    if (!is.matrix(L) && !is.data.frame(L))
        L <- matrix(L, nrow = 1)
    if (missing(beta0))
        beta0 <- rep(0, nrow(L))

    ##print(list(L=L, beta0=beta0))
    
    idx <- !is.na(coef.vec) ## Only want columns of L for identifiable parameters
    L   <- L[ , idx, drop=FALSE]
    
    if (!dim(L)[2] == dim(coef.mat)[1])
        stop(paste("\n Dimension of ",
                   deparse(substitute(L)),
                   ": ", paste(dim(L), collapse = "x"),
                   ", not compatible with no of parameters in ",
                   deparse(substitute(obj)), ": ", dim(coef.mat)[1], sep = ""))

    ct      <- L %*% coef.mat[, 1]
    ct.diff <- L %*% coef.mat[, 1] - beta0
    vcv.L   <- L %*% vcv %*% t(L)

    if (is.null(vn <- rownames(L)))
        vn <- 1:nrow(L)
    rownames(vcv.L)  <- colnames(vcv.L) <- vn

    vc      <- sqrt(diag(vcv.L))

    
    switch(stat.name,
           t.stat = {
               prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
           },
           X2.stat = {
               prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
           })

                      
    if (stat.name == "X2.stat") {
        out <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2,
                     df = df, prob = prob )
    }
    else if (stat.name == "t.stat") {
        out <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc,
                     df = df, prob = prob)
    }

    dim.names <- list(NULL, c("beta0","estimate","std.error",
                              "statistic","df","p.value"))    
    dimnames(out) <- dim.names
    out <- out[, c(2,3,4,6,1,5), drop=FALSE]
    
    conf.int <- "wald"
    if (!is.null(conf.int)) {
        if (level <= 0 || level >= 1)
            stop("level should be betweeon 0 and 1. Usual values are 0.95, 0.90")
        
        alpha <- 1 - level
        switch(stat.name,
               t.stat  = { quant <- qt(1 - alpha/2, df)  },
               X2.stat = { quant <- qnorm(1 - alpha/2) })
        
        cint <- cbind(ct.diff - vc * quant, ct.diff + vc * quant)
        colnames(cint) <- c("lwr", "upr")

        out <- cbind(out, cint)
    }
    out <- as.data.frame(out)
    ## FIXME esticon_class added; not sure if this is good idea?
    class(out) <- c("esticon_class", "data.frame")
    attr(out, "L") <- L
    attr(out, "vcv") <- vcv.L
    out
}


#' @rdname esticon
#' @param object An \code{esticon_class} object. 

coef.esticon_class <- function (object, ...) {
    out <- object$estimate
    names(out) <- rownames(object)
    out
}


#' @rdname esticon
summary.esticon_class <- function (object, ...) 
{
    cat("Coefficients:\n")
    ##printCoefmat(object$coef)
    printCoefmat(object)
    cat("\n")

    cat("L:\n")
    ##print(object$L)
    print(attr(object, "L"))
    cat("\n")

    invisible(object)
}


print.esticon_class <- function(x, ...){
    printCoefmat(x[,1:6])
}


#' @rdname esticon
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


#' @rdname esticon
confint.esticon_class <- function (object, parm, level = 0.95, ...) 
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)

    ## stats:::confint.default code
    ## pct <- stats:::format.perc(a, 3)
    ## fac <- qnorm(a)

    ## replaced with
    pct <- a 
    DF <- object$df
    fac  <- if (!is.null(DF)) qt(a, DF[1])
            else fac <- qnorm(a)
    ## to here
    
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                               pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ##str(list(cf=cf, parm=parm, fac=fac, ses=ses))
    ci[] <- cf[parm] + ses %o% fac
    ci
}


#' @rdname esticon
vcov.esticon_class <- function (object, ...){
    attr(object, "vcv")
}

