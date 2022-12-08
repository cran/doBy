#' @title section
#' 
#' @description Section a functions domain by fixing certain
#'     arguments of a function call.
#'
#' @name section_fun
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} based on code
#'   adapted from the curry package.
#' 
#' @param fun Function to be sectioned
#' @param nms Either a named list of the form name=value where each
#'     name is the name of an argument of the function (in which case
#'     `vls` is ignored) or a character vector of names of arguments.
#' @param vls A vector or list of values of the arguments
#' 
#' @param method Either "env" (for environment; the default, using an
#'     auxillary argument for storing sectioned values) or "sub" (for
#'     substitute; based on substituting fixed values into the
#'     function).
#'
#' @param envir Environment
#' @param object An object from section_fun (a scaffold object).
#'
#' @details Let E be a subset of the cartesian product X x Y where X
#'     and Y are some sets. Consider a function f(x,y) defined on
#'     E. Then for any x in X, the section of E defined by x (denoted
#'     Ex) is the set of $y$s in Y such that (x, y) is in
#'     E. Correspondingly, the section of f(x,y) defined by x is the
#'     function $f_x$ defined on Ex given by $f_x(y)=f(x,y)$.
#'
#' `section_fun` is a wrapper for calling `section_fun_sub`
#'     (default) or `section_fun_env`.
#'
#' @return A new function: The input function `fun` but with certain
#'     arguments fixed at specific values.
#' 
#' @examples
#'
#' f  <- function(x, y){x + y}
#' f_ <- section_fun(f, list(y = 10))
#' f_ <- section_fun(f, nms="y", vls=10) ## SAME AS ABOVE
#' f_
#' f_(x=1)
#'
#' f_ <- section_fun(f, list(y = 10), method="env")
#' f_ <- section_fun(f, nms="y", vls=10, method="env") ## SAME AS ABOVE
#' f_
#' f_(x=1)
#' get_section(f_)
#' get_fun(f_)
#'
#' ## With moder complicated values:
#' g <- function(A, B) {
#'   A + B
#' }
#' g_ <- section_fun(g, list(A = matrix(1:4, nrow=2)))
#' g_ <- section_fun(g, "A", list(matrix(1:4, nrow=2)))
#' g_(diag(1, 2))
#'
#' g_ <- section_fun(g, list(A = matrix(1:4, nrow=2)), method="env")
#'
#' ## Using built in function
#' set.seed(123)
#' rnorm5 <- section_fun(rnorm, list(n=5)) 
#' rnorm5(0, 1)
#'
#' set.seed(123)
#' rnorm(5)
#'
#' 
NULL


#' @rdname section_fun
#' @export
section_fun <- function(fun, nms, vls=NULL, method="sub") {
    method_ <- match.arg(method, c("env", "sub"))

    args <- nms_vls_to_list(nms, vls)

    if (identical(method_, "env"))
        section_fun_env_worker(fun, args)
    else
        section_fun_sub_worker(fun, args)
}


#' @rdname section_fun
#' @export
section_fun_sub <- function(fun, nms, vls=NULL, envir=parent.frame()){

    args <- nms_vls_to_list(nms, vls)
    section_fun_sub_worker(fun, args)
}


section_fun_sub_worker <- function(fun, args, envir=parent.frame()){

    fmls  <- formals(fun)
    idx <- match(names(args), names(fmls))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0){
        fmls  <- fmls[-idx]
    }

    hd <- paste0("function(", paste0(names(fmls), collapse=", "), ")")
    hd
    
    aux <- sapply(1:length(args),
                  function(i){
                      nm <- names(args)[i]
                      paste0(nm, " = ", deparse(args[[i]]))
                  })
    
    
    bd1 <- paste0("\n ## section\n ", paste0(aux, collapse=";\n "), "\n ## section (end)")
    bd1
    
    bd2 <- deparse(body(fun))
    bd2 <- bd2[2:(length(bd2) - 1)]
    bd2 <- gsub("^ *", "", bd2) ## Remove leading whites
    bd2 <- paste0(paste0(bd2, collapse=";\n "))
    bd2
    
    bd <- paste0("\n{ ", paste0(c(bd1, bd2), collapse=";\n "), "\n}")
    
    ff <- paste0(c(hd, bd), collapse="")
    out <- eval(parse(text=ff))
    out
    environment(out) <- environment(fun)
    out

}



nms_vls_to_list <- function(nms, vls){

    if (inherits(nms, "list")){
        if (!is.null(vls)) {
            warning("vls ignored")
        }
        return(nms)
    } else {
        if (length(nms) != length(vls))
            stop("'nms' and 'vls' must have same length\n")
        vls <- as.list(vls)
        names(vls) <- nms
        return(vls)        
    }
}



#' @rdname section_fun
#' @export
section_fun_env <- function(fun, nms, vls=NULL) {

    args <- nms_vls_to_list(nms, vls)
    section_fun_env_worker(fun, args)
    ##.partial(fun, args)
}


section_fun_env_worker <- function(fun, args){
    fun <- as.scaffold(fun)
    .apply_args(fun, args)
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

  fmls[names(get_section(fun))] <- NULL
  
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

#' @rdname section_fun
#' @export
get_section <- function(object){
    if (!inherits(object, "scaffold"))
        stop("'object' must be scaffold object\n")
    attr(object, "arg_env")$args
}

#' @rdname section_fun
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

    cat("Section: \n")
    str(get_section(object))

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






## #' @rdname section_fun
## #' @export
## section_fun_sub <- function(fun, nms, vls, envir=parent.frame()){

##     if (inherits(nms, "list")){
##         if (!missing(vls)) {
##             warning("vls ignored")
##         }
##         args <- nms
##     } else {
##         args <- nms_vls_to_list(nms, vls)
##     }

##     body1 <- as.expression(body(fun))
##     body2 <- do.call("substitute", list(body1[[1]], args))
    
##     fmls  <- formals(fun)
##     idx <- match(names(args), names(fmls))
##     idx <- idx[!is.na(idx)]
##     if (length(idx) > 0){
##         fmls  <- fmls[-idx]
##     }
    
##     out <- as.function(c(fmls, body2), envir=envir)
##     environment(out) <- environment(fun)
##     out
## }


## ' @rdname section_fun
## ' @export
## section_fun <- function(fun, args, method="env") {
  ## method_ <- match.arg(method, c("env", "sub"))
  ## if (identical(method_, "env"))
    ## section_fun_env(fun, args)
  ## else
    ## section_fun_sub(fun, args)
## }
