###############################################################################
##
#' @title Function to calculate groupwise summary statistics
#' @description Function to calculate groupwise summary statistics,
#'     much like the summary procedure of SAS
#' @name by-summary
##
###############################################################################
#'
#' @details Extra arguments ('...') are passed onto the functions in
#'     FUN. Hence care must be taken that all functions in FUN accept
#'     these arguments - OR one can explicitly write a functions which
#'     get around this.  This can particularly be an issue in
#'     connection with handling NAs. See examples below.  Some code
#'     for this function has been suggested by Jim
#'     Robison-Cox. Thanks.
#' 
#' @param formula A formula object, see examples below.
#' @param data A data frame.
#' @param FUN A list of functions to be applied, see examples below.
#' @param id A formula specifying variables which data are not grouped by but
#'     which should appear in the output. See examples below.
#' @param keep.names If TRUE and if there is only ONE function in FUN, then the
#'     variables in the output will have the same name as the variables in the
#'     input, see 'examples'.
#' @param p2d Should parentheses in output variable names be replaced by dots?
#' @param order Should the resulting dataframe be ordered according to the
#'     variables on the right hand side of the formula? (using \link{orderBy}
#' @param full.dimension If TRUE then rows of summary statistics are repeated
#'     such that the result will have the same number of rows as the input
#'     dataset.
#' @param var.names Option for user to specify the names of the variables on the
#'     left hand side.
#' @param fun.names Option for user to specify function names to apply to the
#'     variables on the left hand side.
#' @param \dots Additional arguments to FUN. This could for example be NA actions.

#' @return A dataframe.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ave}}, \code{\link{descStat}}, \code{\link{orderBy}},
#'     \code{\link{splitBy}}, \code{\link{transformBy}}
#' @keywords univar
#' @examples
#' 
#' data(dietox)
#' dietox12    <- subset(dietox,Time==12)
#'
#' fun <- function(x){
#'   c(m=mean(x), v=var(x), n=length(x))
#' }
#' 
#' summaryBy(cbind(Weight, Feed) ~ Evit + Cu, data=dietox12,
#'           FUN=fun)
#' 
#' summaryBy(list(c("Weight", "Feed"), c("Evit", "Cu")), data=dietox12,
#'           FUN=fun)
#'
#' ## Computations on several variables is done using cbind( )
#' summaryBy(cbind(Weight, Feed) ~ Evit + Cu, data=subset(dietox, Time > 1),
#'    FUN=fun)
#' 
#' ## Calculations on transformed data is possible using cbind( ), but
#' # the transformed variables must be named
#' 
#' summaryBy(cbind(lw=log(Weight), Feed) ~ Evit + Cu, data=dietox12, FUN=mean)
#'  
#' ## There are missing values in the 'airquality' data, so we remove these
#' ## before calculating mean and variance with 'na.rm=TRUE'. However the
#' ## length function does not accept any such argument. Hence we get
#' ## around this by defining our own summary function in which length is
#' ## not supplied with this argument while mean and var are:
#' 
#' sumfun <- function(x, ...){
#'   c(m=mean(x, na.rm=TRUE, ...), v=var(x, na.rm=TRUE, ...), l=length(x))
#' }
#' summaryBy(cbind(Ozone, Solar.R) ~ Month, data=airquality, FUN=sumfun )
#' 
#' ## Using '.' on the right hand side of a formula means to stratify by
#' ## all variables not used elsewhere:
#' 
#' data(warpbreaks)
#' summaryBy(breaks ~ wool + tension, warpbreaks, FUN=mean)
#' summaryBy(breaks ~ ., warpbreaks, FUN=mean)
#' summaryBy(. ~ wool + tension, warpbreaks, FUN=mean)
#' 

#' @export
#' @rdname by-summary
summary_by <- function(data, formula, id=NULL, FUN=mean,
                       keep.names=FALSE,
                       p2d=FALSE, order=TRUE, full.dimension=FALSE,
                       var.names=NULL, fun.names=NULL,
                       ...){
    cl   <- match.call(expand.dots = TRUE)
    cl[[2]] <- formula
    cl[[3]] <- data
    names(cl)[2:3] <- c("formula", "data")
    cl[[1]] <- as.name("summaryBy")
    eval(cl)
}


#' @export
#' @rdname by-summary
summaryBy <- function (formula, data=parent.frame(), id=NULL, FUN=mean,
                       keep.names=FALSE,
                       p2d=FALSE, order=TRUE, full.dimension=FALSE,
                       var.names=NULL, fun.names=NULL,
                       ...){

    if (!inherits(data, "tbl_df")) is.tib = FALSE
    else {is.tib = TRUE; data = as.data.frame(data)}
      
    debug.info <- 0

    zzz <- .get_variables(formula, data, id, debug.info) ## ; str(zzz)
    lhs.num <- zzz$lhs.num
    rhs.grp <- zzz$rhs.grp
    ids.var <- zzz$form.ids.var

##    str(list(lhs.num=lhs.num))
    
    rh.trivial <- length( rhs.grp ) == 0 #; cat(sprintf("rh.trivial=%d\n", rh.trivial))

    rh.string <- .get_rhs_string( data, rhs.grp )
    rh.unique <- unique(rh.string)
    rh.idx    <- match(rh.unique, rh.string)
    rh.string.factor <- factor(rh.string, levels=rh.unique) ## This is important

### Get data for id.vars; use ids.var, data, rh.idx
    if (length(ids.var)>0){
      id.data <-  data[ rh.idx, ids.var, drop=FALSE ] ##; print(id.data)
    }

### Get lhs data; use lhs.num, data

    ## Below, we need lhs in the form cbind(y1, y2, y3) for further computations.
    ##
    ## It is OK to have computations in this form a la cbind(y1, y2,
    ## y3, y4=y1+y2, y5=log(y2)).
    ##
    ## If lhs is y1 this is translated into cbind(y1).
    ##
    ## If lhs is ., this is translated into c(y1, y2, y3).
    ##
    ## It is also so (alas) that one may write y1 + y2 + y3,
    ## which is also translated into c(y1, y2, y3)

    
    if (length(lhs.num) > 1) {
        ## If lhs.num is vector of length > 1, wrap with cbind:
        lhs.num <- paste0("cbind( ", toString(lhs.num), " )")
    } else {
        ## strip cbind( ... ) if it is there; then put cbind around
        ## (if lhs is simply y1, then there is no cbind around).
        ff2 <- gsub("^cbind\\((.*)\\)$", "\\1", lhs.num)
        lhs.num <- paste0("cbind( ", ff2, " )")
    }
        
    ## aa <- lapply(paste(lhs.num), function(x)eval(parse(text=x), data))
    ## lh.data <- do.call(cbind, aa)
    ## replace two lines above with
    lh.data <- eval(parse(text=lhs.num), data)

    ## Hack: redefine lhs.num : name of variables in data frame
    lhs.num <- colnames(lh.data)
    
    ##colnames(lh.data) <- lhs.num


### Function names; use FUN
    funNames <- .get_fun_names( FUN )

    ##print(rh.string.factor)
### Calculate groupwise statistics
    if (!is.list(FUN))
        FUN <- list(FUN)

      out <- NULL
      for (ff in 1:length(FUN)) {  ## loop over functions
          ##currFUN <- FUN[[ff]]
          currFUN <- match.fun( FUN[[ff]] )
          for (vv in 1:length(lhs.num)) {  ## loop over variables
              
              currVAR <- lh.data[,lhs.num[vv]]
              
              zzz <- tapply(currVAR, rh.string.factor,
                            function(x){ currFUN(x, ...) }, simplify=FALSE)
              zzz  <- do.call(rbind, zzz)
              out  <- cbind(out, zzz)
          }
      }
      
      
      
      if (!is.null(var.names) && length(var.names)==length(lhs.num))
          lhs.names <- var.names
      else
          lhs.names <- lhs.num
      
### Set names for columns
      
      ##print(funNames)
      if (!is.null(fun.names) ) ##&& length(fun.names)==length(funNames))
          funNames <- fun.names
      
      newnames <- .get_col_names(ncol(out), colnames(out), funNames,
                                 lhs.names, keep.names)
      ##cat(sprintf("newnames    = %s\n", toString( newnames )))
      colnames(out) <- newnames
      out <- as.data.frame(out)
      
      
### Pad the rhs data to the result
      if (!rh.trivial){
          out <- cbind(data[rh.idx, rhs.grp, drop=FALSE], out)
      }
      
### Pad id.data to result
      ##print(id.data)
      if (length(ids.var)>0){
          out <- cbind(out, id.data)
      }
      
### Must the result have full dimension?
      if (full.dimension){
          rrr <- as.numeric(rh.string.factor)
          out <- out[rrr,, drop=FALSE]
      }
      
### Order the result by the rhs
      if (order && !rh.trivial){
          rhs.string <- paste (rhs.grp, collapse='+')
          out <- orderBy(as.formula(paste("~", rhs.string)), data=out)
      }
      
### Replace '('s and ')'s with '.'s
      if (p2d)
          names(out) <-  gsub("\\)","\\.", gsub("\\(","\\.",names(out)))
      
### Finalize
      rownames(out) <- 1:nrow(out)
      if (length(unique(names(out))) != length(names(out)))
          warning("dataframe contains replicate names \n", call.=FALSE)
      
      if (is.tib) as_tibble(out) else out      
      ## out      
}





.get_rhs_string <- function(data, rhs.var, sep.string="@"){

  if (length(rhs.var)==0){
    rep.int("1", nrow(data))
  } else {
    rh.string <- paste(data[,rhs.var[1]])
    if (length( rhs.var ) > 1){
      for (ii in 2:length( rhs.var )){
        rh.string <- paste(rh.string, sep.string, data[, rhs.var[ii]], sep='')
      }
    }
    rh.string
  }
}


.get_fun_names <- function( FUN ){

    if (!is.list(FUN))
      funNames <- paste(deparse(substitute(FUN, env=parent.frame())), collapse = " ")
    else
      funNames <- unlist(lapply(substitute(FUN, env=parent.frame())[-1], function(a) paste(a)))

    ##cat(sprintf("funNames   = %s\n", toString(funNames)))
    funNames
}

.get_col_names <- function(ncol.ans, colNames, funNames, lhs.num, keep.names){
### Names for new variables

  ## Does the columns of ans have names??
  oldnames <- colNames #colnames(ans)
  if (is.null(oldnames))
    hasNames <- 0
  else {
    hasNames <- 1*(prod(nchar(oldnames))>0)
  }
  ##cat(sprintf("hasNames=%i\n", hasNames))

  ## Dim of response (per variable on LHS)
  dimr     <- (ncol.ans)/length(lhs.num)

  only.one.response <- (ncol.ans==length(lhs.num))
  if ( keep.names && only.one.response ){
    newnames <- lhs.num
  } else {
    if (hasNames>0){ ## newnames = var.fun
      funNames <- colNames[1:dimr]
      newnames <- unlist(lapply(lhs.num, function(v){paste(v, funNames, sep='.')}))
    } else {
      if (length(funNames) != dimr){
        funNames <- paste("FUN", 1:dimr, sep='')
        newnames <- unlist(lapply(lhs.num, function(v){paste(v, funNames, sep='.')}))
      } else {
        newnames <- unlist(lapply(funNames, function(x) paste(lhs.num, x, sep='.')))
      }
      if (length(newnames)!=ncol.ans){
        funNames <- paste(funNames, 1:dimr, sep=".")
        newnames  <- unlist(lapply(funNames, function(x) paste(lhs.num, x, sep='.')))
      }
    }
  }
  newnames
}


.get_variables <- function(formula, data, id, debug.info){

    data.var  <- names(data)

    if (!inherits(formula, c("formula", "list")))
        stop("'formula' must be a formula or a list")
    
    if (inherits(formula, "formula")){
        if (length(formula) != 3) stop("Formula must have a left hand side")
        rhs       <- formula[[3]]
        form.rhs.var   <- all.vars(rhs) ## May contain "." and "1"
        
        lhs       <- formula[[2]]
        form.lhs.var   <- all.vars(lhs) ## May contain "."
                                        #print(form.lhs.var)
        
        zz <- .lhsParse(lhs)
        form.lhs.var <-
            if (length(zz)==1)
                paste(zz)
            else
                paste(unlist(.lhsParse(lhs)))
        
        ##print(form.lhs.var)
        
    } else {
        if (length(formula)>=2){
            lhs <- formula[[1]]
            rhs <- formula[[2]]
            form.lhs.var <- lhs
            form.rhs.var <- rhs
        } else {
            stop("Invalid specification of formula")
        }
    }
    
    if (is.null(id)){
        form.ids.var <- character(0)
    } else {
        if (!inherits(id, c("formula", "character"))){
            stop("'id' must be a formula or a character vector")
        }

        if (inherits(id, "formula")){
            form.ids.var   <- all.vars(id)
        } else {
            form.ids.var   <- id
        }
    }
    
    data.cls  <- lapply(data, class)
    data.num.idx   <- data.cls %in% c("numeric","integer")
    data.num.var   <- data.var[ data.num.idx  ]
    data.fac.var   <- data.var[ !data.num.idx ]
    
    ##   print(form.lhs.var)
    ##   print(data.num.var)
    lhs.num   <- intersect( form.lhs.var, data.num.var )
    rhs.num   <- intersect( form.rhs.var, data.num.var )
    ids.num   <- intersect( form.ids.var, data.num.var )
    lhs.fac   <- intersect( form.lhs.var, data.fac.var )
    rhs.fac   <- intersect( form.rhs.var, data.fac.var )
    ids.fac   <- intersect( form.ids.var, data.fac.var )
    
    lll <- list(data.var=data.var,
                form.lhs.var=form.lhs.var, form.rhs.var=form.rhs.var, form.ids.var=form.ids.var,
                lhs.num=lhs.num, rhs.num=rhs.num, ids.num=ids.num,
                lhs.fac=lhs.fac, rhs.fac=rhs.fac, ids.fac=ids.fac )
    
                                        #if (debug.info>=1)
    ##{ cat("status:\n"); str(lll, vec.len=20) }
    
    if ( "." %in% form.lhs.var ){ ## need all numeric variables not metioned elswhere on lhs
        form.lhs.var <- setdiff(form.lhs.var, ".")
        lhs.num <- union( form.lhs.var, setdiff(data.num.var, c(rhs.num, ids.num)))
        if ( length( lhs.fac ) > 0 ){
            isSpecial <- rep(NA, length( lhs.fac ))
            for (j in 1:length(lhs.fac)){
                isSpecial[j]<- (class(data[,lhs.fac[j]])[1] %in% c("POSIXt", "Date"))
            }
            lhs.num <- union( lhs.num, lhs.fac[ isSpecial ] )
        }
    } else {
        lhs.num <- form.lhs.var
    }
    
    ## The grouping variable
    if ("." %in% form.rhs.var){ ## need all factors not mentioned elsewhere as grouping factors
        free.fac <- setdiff(data.fac.var, c(lhs.fac, ids.fac))
        rhs.grp  <- c(setdiff(form.rhs.var, "."), free.fac)
    } else {
        rhs.grp <- form.rhs.var
    }
    rhs.grp <- intersect( rhs.grp, data.var )
    
    rrr <- list(lhs.num=lhs.num, rhs.fac=rhs.fac, form.ids.var=form.ids.var,
                form.rhs.var=form.rhs.var, rhs.grp=rhs.grp)
    ##str(rrr)
    rrr
}



.lhsParse <- function(x){
    ##cat(".lhsParse:"); print(x); print(class(x))
    if (inherits(x, 'name')){
        value <- x
    } else {
        s <- paste(x[[1]])
        value <- switch(s,
                        '+'={  c(.lhsParse(x[[2]]),.lhsParse(x[[3]]))},
                        'I'={  x[[2]]},
                        {  deparse(x)})
    }
    value
}



