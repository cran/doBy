#' Convert expression into function object.
#'
#' @param e expression
#'
#' @examples
#'
#' ee <- expression(b1 + (b0 - b1)*exp(-k*x) + b2*x)
#' ff <- expr_to_fun(ee)
#' @export
expr_to_fun <- function(e){
    vn <- all.vars(e)
    fmls <- vector("list", length(vn))
    names(fmls) <- vn
    
    out <- function(){}

    formals(out) <- fmls
    body(out) <- e
    return(out)
}
