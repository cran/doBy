#' Backquote a list of functions
#'
#' @param fun_list List of functions
#'
#' @seealso [base::bquote()], [set_default()], [section_fun()]
#' @examples
#'
#' ## Evaluate a list of functions
#' f1 <- function(x){x + 1}
#' f2 <- function(x){x + 8}
#'
#' f1_ <- set_default(f1, list(x=10))
#' f2_ <- set_default(f2, list(x=10))
#'
#' f1_(); f2_()
#' 
#' fn_list  <- list(f1_, f2_)
#' fn_list_ <- bquote_fun_list(fn_list)
#' 
#' eval(fn_list[[1]])     ## No
#' sapply(fn_list, eval)  ## No
#' 
#' eval(fn_list_[[1]])    ## Yes
#' sapply(fn_list_, eval) ## Yes
#' 
#' @export
bquote_fun_list <- function(fun_list){
    if (!inherits(fun_list, "list"))
        stop("'fun_list' is not a list.")
    cls <- c("function", "scaffold")
    if (!all(sapply(fun_list, inherits, cls)))
        stop("Not all elements in 'fun_list' are functions or scaffold objects.")
    lapply(fun_list, function(g) {
        bquote(.(g)())
    }
    )
}


