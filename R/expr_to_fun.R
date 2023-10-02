#' Convert expression into function object.
#'
#' @param expr_ R expression.
#' @param order desired order of function argument.
#' @param vec_arg should the function take vector valued argument.
#'
#' @examples
#'
#' ee <- expression(b1 + (b0 - b1)*exp(-k*x) + b2*x)
#' ff <- expr_to_fun(ee)
#'
#' ee <- expression(matrix(c(x1+x2, x1-x2, x1^2+x2^2, x1^3+x2^3), nrow=2))
#' ff <- expr_to_fun(ee)
#'
#' ee <- expression(
#'   matrix(
#'     c(8 * x1 * (4 * x1^2 - 625 * x2^2 - 2 * x2
#'     - 1) + 9 * x1 - 20 * x2 * (x3 + 0.473809523809524 + exp(-x1 *
#'     x2)/20) * exp(-x1 * x2) - 3 * cos(x2 * x3) - 4.5,
#'     -20 * x1 * (x3 + 0.473809523809524 + exp(-x1 * x2)/20) * exp(-x1 * x2) + 3 * x3 *
#'     (x1 - cos(x2 * x3)/3 - 0.5) * sin(x2 * x3) + (-1250 * x2 - 2) * (4
#'     * x1^2 - 625 * x2^2 - 2 * x2 - 1),
#'     3 * x2 * (x1 - cos(x2 * x3)/3 -
#'     0.5) * sin(x2 * x3) + 400 * x3 + 189.52380952381 + 20 * exp(-x1 *
#'     x2)), nrow = 3))
#' ff <- expr_to_fun(ee)
#' 
#' @export
expr_to_fun <- function(expr_, order=NULL, vec_arg=FALSE) {
    if (vec_arg) {
        expr_to_one_param_fun(expr_, order=order)
    } else {
        expr_to_multi_param_fun(expr_, order=order)
    }    
}

handle_order <- function(e, order) {
    if (is.null(order))
        return(sort(all.vars(e)))
    
    nms <- all.vars(e)
    ss <- setdiff(nms, order)
    if (length(ss) > 0) {        
        stop("some arguments are not given in 'order'")
    }
    return(order)
}
  
    
expr_to_one_param_fun <- function(e, order=NULL) {

    nms <- handle_order(e, order) 
    e_str <- expr_to_string(e)
    
    if (length(nms)) {
        aux <- sapply(1:length(nms),
                      function(i) {
                          nm <- nms[i]
                          paste0(nm, " = parm[", i, "]")
                      }
                      )
    } else {
        aux <- NULL
    }

    comb <- c(aux, e_str)    
    
    fun_str <- "function(parm)"
    
    bd <- paste0("\n{ \n", paste0(comb, collapse=";\n "), "\n}")

    ff <- paste0(fun_str, bd)
    fun <- eval(parse(text=ff))
    return(fun)
}



expr_to_multi_param_fun <- function(ee, order=NULL) {

    nms <- handle_order(ee, order)   
    ee_str <- expr_to_string(ee)
    
    fun_str <- paste0("function(", paste0(nms, collapse=", "), ")")
        
    bd <- paste0("\n{ \n", paste0(ee_str, collapse=";\n "), "\n}")
    ff <- paste0(fun_str, bd)
    fun <- eval(parse(text=ff))
    return(fun)
}

expr_to_string <- function(ee) {
    ee_str <- lapply(ee, deparse)
    
    ee_str <-
        lapply(ee_str,
               function(e_) {
                   paste0(e_, collapse="\n")               
               })
    ee_str
}


## expr_to_fun <- function(e){
    ## vn <- all.vars(e)
    ## fmls <- vector("list", length(vn))
    ## names(fmls) <- vn
    
    ## out <- function(){}

    ## formals(out) <- fmls
    ## body(out) <- e
    ## return(out)
## }
