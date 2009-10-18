 
esticon <- function(obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE)
  UseMethod("esticon")


esticon.gls <- function (obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else {
    stat.name <- "X2.stat"
    vcv <- vcov(obj)
    cf  <- matrix(coef(obj))
    df <- 1
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level,cf,vcv,df,stat.name)
  }
}

esticon.mer <- function (obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else {
    stat.name <- "X2.stat"
    cf <- matrix(fixef(obj))
    vcv <- vcov(obj)
    df <- 1
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level,cf,vcv,df,stat.name)
  }
}

esticon.geeglm <- function (obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "X2.stat"
    cf  <- summary(obj)$coef
    vcv <- summary(obj)$cov.scaled
    df <- 1
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level,cf,vcv,df,stat.name)
  }
}

esticon.lm <- function (obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "t.stat"
    cf <- summary.lm(obj)$coefficients
    vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
    df <- obj$df.residual
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level,cf,vcv,df,stat.name)
  }
}

esticon.glm <- function (obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test=FALSE){
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    cf <- summary.lm(obj)$coefficients
    vcv <- summary(obj)$cov.scaled
    if(family(obj)[1] %in% c("poisson","binomial")){
      stat.name <- "X2.stat"            
      df <- 1
    } else {              
      stat.name <- "t.stat"
      df <- obj$df.residual
    }
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level=level,cf,vcv,df,stat.name)
  }
}


##########################################################################################
###
### esticon.lme needs better testing at some point
###
##########################################################################################
esticon.lme <- function (obj, cm, beta0, conf.int = NULL, level=0.95, joint.test=FALSE){
  warning("The esticon function has not been thoroughly teste on 'lme' objects")
  if (joint.test==TRUE){
    .wald(obj, cm, beta0)
  } else { 
    stat.name <- "t.stat"
    cf <- summary(obj)$tTable
    rho <- summary(obj)$cor
    vcv <- rho * outer(cf[, 2], cf[, 2])
    tmp <- cm
    tmp[tmp == 0] <- NA
    df.all <- t(abs(t(tmp) * obj$fixDF$X))
    df <- apply(df.all, 1, min, na.rm = TRUE)
    #print(df)
    problem <- apply(df.all != df, 1, any, na.rm = TRUE)
    #print(problem)
    if (any(problem)) 
      warning(paste("Degrees of freedom vary among parameters used to ", 
                    "construct linear contrast(s): ",
                    paste((1:nrow(tmp))[problem], 
                          collapse = ","),
                    ". Using the smallest df among the set of parameters.", 
                    sep = ""))
    df <- min(df)
    .esticonCore(obj, cm, beta0, conf.int=conf.int,level,cf,vcv,df,stat.name)
  }    
}


### .functions below here ###

.wald <- function (obj, cm,beta0)
{
    if (!is.matrix(cm) && !is.data.frame(cm)) 
        cm <- matrix(cm, nrow = 1)

    if (missing(beta0))
      beta0 <- rep(0,nrow(cm))

    df <- nrow(cm)
    if ("geese" %in% class(obj)) {
      cf <- obj$beta
      vcv <- obj$vbeta
    } else if ("geeglm" %in% class(obj)) {
      cf <- obj$coef
      vcv <- summary(obj)$cov.scaled
    } else if ("gls" %in% class(obj)) {
      vcv <- vcov(obj)
      cf  <- matrix(coef(obj))
    } else if ("gee" %in% class(obj)) {
      cf <- obj$coef
      vcv <- obj$robust.variance
    }
    else if ("lm" %in% class(obj)) {
      cf <- summary.lm(obj)$coefficients[, 1]
      vcv <- summary.lm(obj)$cov.unscaled * summary.lm(obj)$sigma^2
      if ("glm" %in% class(obj)) {
        vcv <- summary(obj)$cov.scaled
      }
    }
    else if ("coxph" %in% class(obj)) {
      cf <- obj$coef
      vcv <- obj$var
    }
    else
      stop("obj must be of class 'lm', 'glm', 'aov', 'gls', 'gee', 'geese', 'coxph'")
    u      <- (cm %*% cf)-beta0
    vcv.u  <- cm %*% vcv %*% t(cm)
    W      <- t(u) %*% solve(vcv.u) %*% u
    prob   <- 1 - pchisq(W, df = df)
    retval <- as.data.frame(cbind(W, df, prob))
    names(retval) <- c("X2.stat", "DF", "Pr(>|X^2|)")
    return(as.data.frame(retval))
}

.esticonCore <- function (obj, cm, beta0, conf.int = NULL, level,cf,vcv,df,stat.name ) {

  if (conf.int != FALSE){
    conf.int <- "wald"
    cat("Confidence interval (", toupper(conf.int), ") level =",level,"\n")
  } else
  conf.int <- NULL
  
  if (!is.matrix(cm) && !is.data.frame(cm)) 
    cm <- matrix(cm, nrow = 1)
   if (missing(beta0))
    beta0 <- rep(0,nrow(cm))   

  if (is.null(cm)) 
    cm <- diag(dim(cf)[1])
  
  if (!dim(cm)[2] == dim(cf)[1]) 
    stop(paste("\n Dimension of ",
               deparse(substitute(cm)), 
               ": ", paste(dim(cm), collapse = "x"),
               ", not compatible with no of parameters in ", 
               deparse(substitute(obj)), ": ", dim(cf)[1], sep = ""))
  ct <- cm %*% cf[, 1] 
  ct.diff <- cm %*% cf[, 1] - beta0      
  vc <- sqrt(diag(cm %*% vcv %*% t(cm)))

  if (is.null(rownames(cm))) 
    rn <- paste("(", apply(cm, 1, paste, collapse = " "), ")", sep = "")
  else rn <- rownames(cm)

  rn <- NULL
  switch(stat.name,
         t.stat = {
           prob <- 2 * (1 - pt(abs(ct.diff/vc), df))
         },
         X2.stat = {
           prob <- 1 - pchisq((ct.diff/vc)^2, df = 1)
         })
  
  if (stat.name == "X2.stat") {
    retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = (ct.diff/vc)^2, 
                    df = df, prob = prob )
    dimnames(retval) <-
      list(rn, c("beta0","Estimate","Std.Error","X2.value","DF","Pr(>|X^2|)"))
  }
  else if (stat.name == "t.stat") {
    retval <- cbind(hyp=beta0, est = ct, stderr = vc, t.value = ct.diff/vc, 
                    df = df, prob = prob)
    dimnames(retval) <-
      list(rn, c("beta0","Estimate","Std.Error","t.value","DF","Pr(>|t|)"))
  }
  
  if (!is.null(conf.int)) {
    if (level <= 0 || level >= 1) 
      stop("level should be betweeon 0 and 1. Usual values are 0.95, 0.90")

    alpha <- 1 - level
    switch(stat.name,
           t.stat  = { quant <- qt(1 - alpha/2, df  )  },
           X2.stat = { quant <- qnorm(1 - alpha/2) })
                                        #X2.stat = { quant <- qt(1 - alpha/2, 1000) })
  
    switch(tolower(conf.int),
           "wald"= {vvv <- cbind(ct.diff-vc*quant, ct.diff+vc*quant)},
           "lr"  = {vvv <- NULL
                    for(i in 1:nrow(cm))
                      vvv <- rbind(vvv, .cilambda(cm[i,],obj,level)-beta0[i] )
                  })
    colnames(vvv) <- c("Lower.CI", "Upper.CI")  
    retval <- cbind(retval, vvv)
  }
  retval[,6] <- round(retval[,6],7)
  return(as.data.frame(retval))
}

