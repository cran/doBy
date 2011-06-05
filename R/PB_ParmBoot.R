##########################################################
###
### Likelihood ratio statistic
###
##########################################################

## LRT <- function(largeModel, smallModel){
##   UseMethod("LRT")
## }

.LRT_mer <- function(largeModel, smallModel){
  ll.small <- logLik(smallModel, REML=FALSE)
  ll.large <- logLik(largeModel, REML=FALSE)
  tobs     <- max(0,2*(ll.large-ll.small))
  df11     <- attr(ll.large,"df") - attr(ll.small,"df")
  p.X2     <- 1-pchisq(tobs, df11)
  c(tobs=tobs, df=df11, p=p.X2)
}

##########################################################
###
### Bartlett corrected LRT
###
##########################################################

BCmodcomp <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){
  UseMethod("BCmodcomp")
}

BCmodcomp.mer <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){

  if (is.null(ref))
    ref <- PBrefdist.mer(largeModel, smallModel, nsim=nsim, cl=cl, details=details)

  lrt   <- .LRT_mer(largeModel, smallModel)
  tobs  <- unname(lrt['tobs'])
  ndf   <- unname(lrt['df'])
  m.tref  <- mean(ref)
  v.tref  <- var(ref)

  ## Bartlett correction - X2 distribution
  BCstat  <- ndf * tobs/m.tref
  p.BC    <- 1-pchisq(BCstat,df=ndf)

  ## Fit to gamma distribution
  scale   <- v.tref/m.tref
  shape   <- m.tref^2/v.tref
  p.Ga    <- 1-pgamma(tobs, shape=shape, scale=scale)
  ##cat(sprintf("nsim=%i p.BC=%f, p.GA=%f \n", length(ref), p.BC, p.Ga))

  res <- c(lrt, p.BC=p.BC, BCstat=BCstat, m.tref=m.tref)
  res <- list(res, f.large=formula(largeModel), f.small=formula(smallModel))
  class(res) <- c("BCmodcomp", "IImodcomp")
  res

  ans <- list(type="X2test",
              f.large=formula(largeModel),
              f.small=formula(smallModel),
              LRT = c(lrt),
              Bartlett = c(tobs=BCstat,  df=ndf, p=p.BC),
              Gamma    = c(tobs=tobs,    df=NA,  p=p.Ga)
              )
  attr(ans,"moment") <- c(mean=m.tref, var=v.tref)
  attr(ans,"gamma")  <- c(scale=scale, shape=shape)
  class(ans) <- "XXmodcomp"
  ans
}


##########################################################
###
### Parametric bootstrap anova
###
##########################################################


PBmodcomp <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){
  UseMethod("PBmodcomp")
}

PBmodcomp.mer <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){

  if (is.null(ref))
    ref <- PBrefdist.mer(largeModel, smallModel, nsim=nsim, cl=cl, details=details)

  lrt   <- .LRT_mer(largeModel, smallModel)
  tobs  <- unname(lrt['tobs'])
  ndf   <- unname(lrt['df'])

  m.tref  <- mean(ref)
  v.tref  <- var(ref)

  n.extreme <- sum(tobs < ref)
  p.PB  <- n.extreme / length(ref)

  res <- c(lrt, p.PB=p.PB)
  res <- list(res, f.large=formula(largeModel), f.small=formula(smallModel))
  class(res) <- c("PBmodcomp","IImodcomp")
  res

  ans <- list(type="PBtest",
              f.large=formula(largeModel),
              f.small=formula(smallModel),
              LRT = c(lrt),
              PBtest=c(tobs=tobs,    df=NA, p=p.PB)
              )
  attr(ans,"moment") <- c(mean=m.tref, var=v.tref)
  class(ans) <- "XXmodcomp"
  ans

}


##########################################################
###
### F-approximated scaled LRT
###
##########################################################

.FFmodcomp <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){
  UseMethod(".FFmodcomp")
}

.FFmodcomp.mer <- function(largeModel, smallModel, nsim=200, ref=NULL, cl=NULL, details=0){

  if (is.null(ref))
      ref <- PBrefdist.mer(largeModel, smallModel, nsim=nsim, cl=cl, details=details)

  lrt   <- .LRT_mer(largeModel, smallModel)
  ndf <- unname(lrt['df'])
  names(lrt)[1] <- "tobs"
  tobs <- unname(lrt['tobs'])

  m.tref  <- mean(ref)
  v.tref  <- var(ref)

  df2        <- round(2 * m.tref/ndf / ( m.tref/ndf - 1 ), 2)
  df2.ratio  <- df2/(df2-2)
  FFstat     <- tobs/ndf
  FFstat2    <- df2/(df2-2)* tobs/(m.tref)

  p.FF     <- 1-pf(FFstat,  ndf, df2)
  p.FF2    <- 1-pf(FFstat2, ndf, df2)

  ans <- list(type="Ftest",
              f.large=formula(largeModel),
              f.small=formula(smallModel),
              LRT = c(lrt,df2=NA),
              F=c(tobs=FFstat,  df=ndf, p=p.FF, df2=df2),
              F2=c(tobs=FFstat2, df=ndf, p=p.FF2, df2=df2)
              )

  attr(ans,"moment") <- c(mean=m.tref, var=v.tref)
  class(ans) <- "XXmodcomp"
  ans
}

print.XXmodcomp <- function(x, ...){

    cat("large : "); print(x$f.large)
    cat("small : "); print(x$f.small)

    ans <- as.data.frame(do.call(rbind, x[-c(1:3)]))
    ans$p<-round(ans$p,options("digits")$digits)
    ans$tobs<-round(ans$tobs,options("digits")$digits)

    print(ans)
}

as.data.frame.XXmodcomp <- function(x, row.names = NULL, optional = FALSE, ...){
    as.data.frame(do.call(rbind, x[-c(1:3)]))
}


### ###########################################################
###
### Parallel computing of reference distribution
###
### ###########################################################

PBrefdist <- function(largeModel, smallModel, nsim=200, cl=NULL, details=0){
    UseMethod("PBrefdist")
}

PBrefdist.mer <- function(largeModel, smallModel, nsim=200, cl=NULL, details=0){

    t0 <- proc.time()

    if(is.null(smallModel@call$REML) || smallModel@call$REML)
        smallModel <- update(smallModel,REML=FALSE)

    if(is.null(largeModel@call$REML) || largeModel@call$REML)
        largeModel <- update(largeModel,REML=FALSE)

    if (!is.null(cl)){
        if (!inherits(cl, "cluster")){
            cat("'cl' is not a cluster; can not do parallel computing. \nProceeding in non-parallel mode\n")
            cl <- NULL
        }
    }

    if (is.null(cl)){
        simdata   <- as.matrix(simulate(smallModel,nsim=nsim))
        ref     <- rep(NA, nsim)

        for (kk in seq_len(nsim)){
            yyy <- simdata[,kk]
            small      <- suppressWarnings(refit(smallModel, newresp=yyy))
            large      <- suppressWarnings(refit(largeModel, newresp=yyy))

            ttt   <- as.numeric(2*(logLik(large, REML=FALSE)-logLik(small, REML=FALSE)))
            ref[kk]  <- ttt
        }
        ref <- ref[ref>0]
    } else {
        nsim2 <- round(nsim/length(cl))
        xxx <- clusterCall(cl,
                           function(ll,ss,nsim=200){
                               simdata   <- as.matrix(simulate(ss,nsim=nsim))
                               ref     <- rep(NA, nsim)
                               for (kk in seq_len(nsim)){
                                   yyy <- simdata[,kk]
                                   small      <- suppressWarnings(refit(ss, newresp=yyy))
                                   large      <- suppressWarnings(refit(ll, newresp=yyy))
                                   ttt   <- as.numeric(2*(logLik(large, REML=FALSE) -
                                                          logLik(small, REML=FALSE)))
                                   ref[kk]  <- ttt
                               }
                               ref
                           },
                           largeModel,smallModel,nsim2
                           )

        ref <- c(xxx, recursive=TRUE)
        ref <- ref[ref>0]
    }

    if (details>0)
        cat(sprintf("Reference distribution with %i samples; computing time: %5.2f secs. \n", length(ref), (proc.time()-t0)[3]))

    ref
}

