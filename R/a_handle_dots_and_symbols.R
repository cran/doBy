tochar <- function(...){
    dots <- match.call(expand.dots = FALSE)$...
    dots2char(dots)
}

dots2char <- function(dots){
    ## dots <- match.call(expand.dots = FALSE)$...
    dots1 <- dots[[1]]

    if (inherits(dots1, "call")){
        dots1 <- eval(dots1)
    }
    if (inherits(dots1, c("formula", "call"))){
        varRHS <- all.vars(dots1[[2]])
        return(varRHS)
    }

    if (is.character(dots1)){
        varRHS <- dots1
        return(varRHS)        
    }
       
    varRHS <- lapply(seq_along(dots),
                     function(i) {
                         y <- if (is.symbol(dots[[i]])) {
                                  deparse(dots[[i]])
                              }
                              else {
                                  dots[[i]]
                              }
                         return(y)
                     })
    varRHS <- unlist(varRHS)
    return(varRHS)
}


