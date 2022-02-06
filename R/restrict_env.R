#' @title restrict
#' 
#' @description Restrict a functions domain by fixing certain
#'     arguments of a function call.
#'
#' @name restrict_fun
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} based on code
#'   adapted from the curry package.
#' 
#' @param fun Function to be restricted
#' @param args List of the form name=value
#' @param method Either "env" (for environment; the default, using an
#'     auxillary argument for storing restricted values) or "sub" (for
#'     substitute; based on substituting fixed values into the
#'     function).
#'
#' @param envir Environment
#' @param object An object from restrict_fun (a scaffold object).
#'
#' @details `restrict_fun` is a wrapper for calling `restrict_fun_env`
#'     (default) or `restrict_fun_sub`.
#'
#' @return A new function: The input function `fun` but with certain
#'     arguments fixed at specific values.
#' 
#' @examples
#'
#' f1  <- function(x, y){x + y}
#' f1_ <- restrict_fun(f1, list(y=10))
#' f1_
#' f1_(x=1)
#' get_restrictions(f1_)
#' get_fun(f1_)
#' 
#' f2 <- function(x){
#'   x <- x + 2
#'   x
#' }
#' f2_ <- restrict_fun(f2, list(x=1)) 
#' f2_()
#'
#' # Notice that this is absurd, because arguments are modified in the
#' # body
#' restrict_fun(f2, list(x=10), method="sub")
#'
#' # A safe(r) alternative is:
#' f3 <- function(x){
#'   x_ <- x + 2
#'   x_
#' }
#' restrict_fun(f3, list(x=10), method="sub")
#' 
#' rnorm10 <- restrict_fun(rnorm, list(n=10)) 
#' rnorm(10)
NULL

#' @rdname restrict_fun
#' @export
restrict_fun <- function(fun, args, method="env") {
  method_ <- match.arg(method, c("env", "sub"))
  if (identical(method_, "env"))
    restrict_fun_env(fun, args)
  else
    restrict_fun_sub(fun, args)
}

#' @rdname restrict_fun
#' @export
restrict_fun_sub <- function(fun, args, envir=parent.frame()){
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

#' @rdname restrict_fun
#' @export
restrict_fun_env <- function(fun, args) {
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

  fmls[names(get_restrictions(fun))] <- NULL
  
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

#' @rdname restrict_fun
#' @export
get_restrictions <- function(object){
    if (!inherits(object, "scaffold"))
        stop("'object' must be scaffold object\n")
    attr(object, "arg_env")$args
}

#' @rdname restrict_fun
#' @export
get_fun <- function(object){
    if (!inherits(object, "scaffold"))
        stop("'object' must be scaffold object\n")
    environment(object)$fun
}

#' @export
print.scaffold <- function(x, ...){
  x2 <- x
  attributes(x2) <- NULL
  print.default(x2)
  return(invisible(x))
}


#' @export
summary.scaffold <- function(object, ...){

    cat("Original function: \n")
    print(get_fun(object))

    cat("Restrictions: \n")
    str(get_restrictions(object))

    return(invisible(object))
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

