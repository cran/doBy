#' @title restrict
#' @description Restrict a functions domain by fixing certain
#'   arguments of a function call.
#' @name restrict
#'
#' @param fun Function to be restricted
#' @param args List of the form name=value
#' @param method Either "env" (default, using an auxillary argument
#'   for storing restricted values) or "sub" (based on substituting
#'   fixed values into the function).
#'
#' @param envir Environment
#' @param object An object from restrict (a scaffold object).
#'
#' @details `restrict` is a wrapper for calling `restrict_env` (default) or `restrict_sub`.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} based on code
#'   adapted from the curry package.
#' @examples
#'
#' f1  <- function(x, y){x + y}
#' f1_ <- restrict(f1, list(y=10))
#' f1_
#' f1_(x=1)
#' restrictions(f1_)
#' original_fun(f1_)
#' 
#' f2 <- function(x){
#'   x <- x + 2
#'   x
#' }
#' f2_ <- restrict(f2, list(x=1)) 
#' f2_()
#'
#' # Notice that this is absurd:
#' restrict(f2, list(x=10), method="sub")
#' 
#' rnorm10 <- restrict(rnorm, list(n=10)) 
#' rnorm(10)
NULL


#' @rdname restrict
#' @export
restrict <- function(fun, args, method="env") {
  method <- match.arg(method, c("env", "sub"))
  if (identical(method, "env"))
    restrict_env(fun, args)
  else
    restrict_sub(fun, args)
}

#' @rdname restrict
#' @export
restrict_sub <- function(fun, args, envir=parent.frame()){
  body1 <- as.expression(body(fun))
  body2 <- do.call("substitute", list(body1[[1]], args))

  fmls  <- formals(fun)
  idx <- match(names(args), names(fmls))
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0){
    fmls  <- fmls[-idx]
  }

  out <- as.function(c(fmls, body2), envir=envir)
  environment(out) <- environment(fun)
  out
}




#' @rdname restrict
#' @export
restrict_env <- function(fun, args) {
    fun <- as.scaffold(fun)
    .apply_args(fun, args)
    ##.partial(fun, args)
}

as.scaffold <- function(fun) {
  if (!inherits(fun, c("function", "scaffold")))
    stop("Can not create scaffold\n")

  if (inherits(fun, 'scaffold')) {
    scaffold_update(fun)
  } else {
    from <- parent.frame()
    scaffold_create(fun, from)
  }
}


scaffold_create <- function(fun, from = parent.frame()) {

  arg_env <- new.env(parent = emptyenv())
  assign('args',     list(), envir = arg_env)
  assign('args_end', list(), envir = arg_env)
  
  fmls <- get_formals(fun)
  arg_getter <- getArgs(arg_env)
  do_scaffold(fun, arg_env, fmls, from)  
}



scaffold_update <- function(fun, from = parent.frame()) {
  #cat("scaffold_update\n")
  arg_env <- clone_environment(attr(fun, "arg_env"))
  fmls <- get_formals(fun)
  #cat("formals:\n"); print(fmls)

  fmls[names(restrictions(fun))] <- NULL
  
  fun_orig <- environment(fun)$fun
  arg_getter <- getArgs(arg_env)
  do_scaffold(fun_orig, arg_env, fmls, from)
}

do_scaffold <- function(fun, arg_env, fmls, from){
  ## fmls <- get_formals(fun)
  arg_getter <- getArgs(arg_env)
  
  new_fun <- function() {}    
  formals(new_fun) <- fmls
  body(new_fun) <- bquote({
    args <- arg_getter()
    do.call(.(fun), args)
  }, list(fun = substitute(fun, from)))
  
  structure(new_fun, class = 'scaffold', arg_env = arg_env)
}

get_formals <- function(fun){
  if (is.primitive(fun)) {
    fmls <- formals(args(fun))
  } else {
    fmls <- formals(fun)
  }
  fmls
}




## SOME UTILITIES


#' @rdname restrict
#' @export
restrictions <- function(object){
  if (!inherits(object, "scaffold")) stop("'object' must be scaffold object\n")
  attr(object, "arg_env")$args
}

#' @rdname restrict
#' @export
original_fun <- function(object){
  if (!inherits(object, "scaffold")) stop("'object' must be scaffold object\n")
  environment(object)$fun
}

#' @export
print.scaffold <- function(x, ...){

  x2 <- x
  attributes(x2) <- NULL
  print.default(x2)
  ## cat("args:\n ")
  ## print(names(formals(x)))
  ## cat("function: \n")
  ## print(environment(x)$fun)
  ## cat("restrictions: \n")
  ## print(attr(x, "arg_env")$args)
  invisible(x)
}


#' @export
summary.scaffold <- function(object, ...){

  cat("function: \n")
  print(environment(object)$fun)
  cat("restrictions: \n")
  print(attr(object, "arg_env")$args)
  invisible(object)  
}

getArgs <- function(added_env) {
  function() {
    ## cat("in getArgs:\n")
    env <- parent.frame()
    args <- names(formals(sys.function(sys.parent(1))))
    ## cat("args:\n ");print(args) ## To be specified in call

    if (length(args) > 0) {
      vals <- mget(args, envir = env)
      ## cat("vals:\n"); str(vals)  ## To be specified in call
    
      ellipsis <- names(vals) == '...'
      if (any(ellipsis)) {
        vals <- append(vals, eval(quote(list(...)), env), which(ellipsis))
        vals[ellipsis] <- NULL
      }
      vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
    } else vals=NULL
    ## cat("when done: \n")
    ## str(list(args=added_env$args, vals=vals, args_end=added_env$args_end))   
    out <- c(added_env$args, vals, added_env$args_end)
    ## str(out)
    out
  }
}


is_missing_arg <- function(x) identical(x, quote(expr = ))

clone_environment <- function(e1){
  as.environment(as.list(e1, all.names=TRUE))
}

.apply_args <- function(fun, args, last = FALSE) {
  fmls <- formals(fun)
  #cat("+++ formals: \n"); print(fmls)
  arg_env <- attr(fun, 'arg_env')

  fmls[names(args)] <- NULL
  formals(fun) <- fmls[!names(fmls) %in% names(args)]
  #cat("+++ +++ formals: \n"); print(formals(fun))
  if (last) {
    assign('args_end', append(args, arg_env$args_end), envir = arg_env)
  } else {
    ##arg_env <- clone_environment(arg_env)
    common <- intersect(names(arg_env$args), names(args))

    if (length(common) > 0)
      arg_env$args[common] <- NULL    
    assign('args', append(arg_env$args, args), envir = arg_env)
  }
  structure(fun, class = 'scaffold', arg_env = arg_env)
}








## .partial <- function(fun, args) {
##     ## fmls_names <- names(formals(fun))
##     ## if (!'...' %in% fmls_names && !all(names(args) %in% fmls_names)) {
##     ##     stop('The provided arguments to ',
##     ##          deparse(substitute(fun, parent.frame())),
##     ##          ' does not match its definition',
##     ##          call. = FALSE)
##     ## }
##     .apply_args(fun, args)
## }


## ' dobq <- function(fnlist){
## '   lapply(fnlist, function(g) bquote(.(g)()))
## ' }
## ' 
## ' a1 <- dobq(b1.list)
## ' a2 <- dobq(b2.list)


## args <- list(y=10, u=222, z=-5)

## arg_env<-new.env()
## arg_env$args <- list(x=1, y=10, z=100)
## rlang::env_print(arg_env)$args

## common <- intersect(names(arg_env$args), names(args))
## if (length(common)>0)
##   arg_env$args[common] <- NULL
## assign('args', append(arg_env$args, args), envir=arg_env)
## arg_env$args


## env$args




## env$x <- 1
## env$y <- 10
## env$z <- 100






## is.scaffold <- function(fun)
##   inherits(fun, 'scaffold')

## as.scaffold <- function(fun) {

##   if (is.scaffold(fun)) {
##     fun
##   } else {
##     from <- parent.frame()
##     scaffold(fun, from)
##   }
## }

## scaffold <- function(fun, from = parent.frame()) {

##   arg_env <- new.env(parent = emptyenv())
##   assign('args',     list(), envir = arg_env)
##   assign('args_end', list(), envir = arg_env)

##   fmls <- get_formals(fun)
##   arg_getter <- getArgs(arg_env)

##   do_scaffold(fun, arg_env, from)  
##   ## new_fun <- function() {}    
##   ## formals(new_fun) <- fmls
##   ## body(new_fun) <- bquote({
##   ##     args <- arg_getter()
##   ##     do.call(.(fun), args)
##   ##   }, list(fun = substitute(fun, from)))

##   ## structure(new_fun, class = 'scaffold', arg_env = arg_env)
## }
