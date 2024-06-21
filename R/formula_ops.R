## Move to doBy
##
## https://stackoverflow.com/questions/70735907/how-to-evaluate-in-a-formula-in-r




##' @title Formula operations and coercion.
##'
##' @description Formula operations and coercion as a supplement to `update.formula()`
##'
##' @name formula_ops
##' 
##' @param chr1 Character vector to be coerced to formulas.
##' @param frm1,frm2 Formulas to be coerced to character vectors.
##' @param y Response
##' @param terms Character string.
##' @param op Either "+" (default) or "-".
##' @param object Character vector or formula.
##' @param noint Boolean.
##' @param string Boolean.
##' @param n Positive integer.
##' 
##' @examples
##'
##' formula_poly("z", 2)
##' formula_poly("z", 2, noint=TRUE)
##'
##' as_rhs_chr(c("a", "b", "z"))
##' as_rhs_chr(c("a*b", "z"))
##'
##' as_rhs_chr(y~a+b+z)
##' as_rhs_chr(y~a+b+z, string=TRUE)
##' as_rhs_chr(y~a+b+z)
##' as_rhs_chr(y~a*b+z)
##' as_rhs_chr(y~a*b+z, string=TRUE)
##'
##' as_lhs_chr(y~a*b+z)
##' as_lhs_chr(log(y)~a*b+z)      ## Not what one might expect
##' as_lhs_chr(cbind(y, u)~a*b+z) ## Not what one might expect
##'
##' formula_chr_to_form(c("a*b", "z"))
##' formula_chr_to_form(c("a*b", "z"), "y")
##' formula_chr_to_form(c("a*b", "z"), "log(y)")
##'
##' formula_add(y~a*b+z, ~-1)
##' formula_add(y~a*b+z, ~a:b)
##'
##' formula_add_str(y~x1 + x2, "x3")
##' formula_add_str(y~x1 + x2, "x1")
##' formula_add_str(y~x1 + x2, "x1", op="-")
##'

##' @rdname formula_ops
#' @export
formula_add_str <- function(frm1, terms, op="+"){
    ## FIXME: Need to handle case where frm1 is a rhs-formula
    ch <- as.character(frm1)
    ch[3] <- paste0(ch[3], op, terms)
    
    frm12 <- as.formula(paste(ch[2], ch[1], ch[3]))
    frm1 <- update(frm1, frm12)
    return(frm1)
}


##' @rdname formula_ops
##' @export
formula_add <- function(frm1, frm2){

    stopifnot_formula(frm1)
    stopifnot_formula(frm2)

    ## Right-hand-side    
    frm1_rhs <- as_rhs_chr(frm1)
    frm2_rhs <- as_rhs_chr(frm2)
    o_rhs <- c(frm1_rhs, frm2_rhs)

    o_rhs <- o_rhs|> paste0(collapse="+") 
    ## str(o_rhs)
    ## Remove redunancies    
    o_rhs <- simplify_rhs(o_rhs)
    
    ## Left-hand-side
    frm1_lhs <- as_lhs_chr(frm1)
    frm2_lhs <- as_lhs_chr(frm2)
    
    if ((length(frm1_lhs) > 0) && (length(frm2_lhs) > 0))
        stop("Can not handle two left-sides\n")

    o_lhs <- c(frm1_lhs, frm2_lhs)
    formula_chr_to_form(o_rhs, o_lhs)
}


##' @rdname formula_ops
##' @export
formula_poly <- function(chr1, n, noint=FALSE, y=NULL){
    if (n > 1){
        b <- paste0(chr1, "^", 2:n)
        o <- paste0("I(", b, ")", collapse = "+")
        o <- paste0(chr1, "+", o)
    } else {
        o <- chr1
    }

    if (noint){
        o <- paste0(y, "-1 +", o)
    }
    formula(paste(y, "~", o))
}

##' @rdname formula_ops
##' @export
formula_nth <- function(frm1, n){
    fs <- as_rhs_chr(frm1, string=TRUE)
    fs2 <- paste0("(", fs, ")^", n)
    o <- as_rhs_frm(fs2)
    o
}

##' @rdname formula_ops
##' @export
formula_to_interaction_matrix <- function(frm1){
    ## aa <- as_rhs_chr(frm1)  |> strsplit(":")
    aa <- terms_labels(frm1)
    aa <- aa |> strsplit(":")

    nms <- unique(unlist(aa))
    mm <- matrix(0, nrow=length(nms), ncol=length(nms), dimnames=list(nms, nms))
    for(i in 1:length(aa)){
        g <- aa[[i]]
        mm[g, g] <- 1
    }
    mm
}


##' @rdname formula_ops
##' @param rhs,lhs right-hand-side and left-hand-side for formula (as characters)
##' @export
formula_chr_to_form <- function(rhs, lhs=character(0)){
    rhs <- to_str(rhs)
    o <- paste0(lhs, "~", rhs)    
    as.formula(o)
}

## UTILITIES

##' @rdname formula_ops
##' @param collapse Character to use as separator.
##' @export
to_str <- function(chr1, collapse="+"){
    paste0(chr1, collapse=collapse)
}

##' @rdname formula_ops
##' @export
terms_labels <- function(frm1){
    terms(frm1)  |> attr("term.labels") |> sort()
}

##' @rdname formula_ops
##' @export
simplify_rhs <- function(object){
    UseMethod("simplify_rhs")
}

##' @rdname formula_ops
##' @export
simplify_rhs.formula <- function(object){
    l <- terms_labels(object)
    to_str(l) |> as_rhs_frm()
}

##' @rdname formula_ops
##' @export
simplify_rhs.character <- function(object){
    o <- as_rhs_frm(object)  |>
        terms_labels() |>
        to_str()
    o
}

## Bad name
formula_rhs_to_chr <- function(frm1, string=TRUE){
    frm1 <- terms_labels(frm1)
    if (length(frm1) == 0)
        frm1 <- "-1"
    if (string)
        frm1 <- frm1 |> to_str()
    frm1
}


is_rhsf <- function(object){
    is(object, "formula") && ((object |> terms() |> attr("response")) == 0)
}

stopifnot_rhsf <- function(object){
    if (!is_rhsf(object))
        stop("argument is not a right-hand-sided formula\n")
}

stopifnot_formula <- function(a){
    if (!is(a, "formula"))
        stop("argument is not a formula")
}

stopifnot_chr <- function(a){
    if (!is(a, "character"))
        stop("argument is not a character")
}



## RETURNING FORMULAS

##' @export
as_rhs_frm.character <- function(object){
    object <- object |> to_str()
    paste0("~", object)  |> as.formula()
}

##' @export
as_rhs_frm.formula <- function(frm1){
    formula(delete.response(terms(frm1)))    
}

##' @export
as_lhs_frm.character <- function(object){
    stopifnot_chr(object)
    paste0(object, "~ 1")  |> as.formula()
}

formula_lhs_to_chr <- function(frm1){
    if (is(frm1, "formula")){
        r <- terms(frm1) |> attr("response")
        if (r > 0)
            frm1 <- (terms(frm1) |> attr("variables"))[[r+1]] |> as.character()
        else
            frm1 <- character(0)
    }
    frm1
}

##' @export
as_lhs_frm.formula <- function(frm1){
    o <- frm1 |> formula_lhs_to_chr()
    as.formula(paste0(o, "~1"))
}


## RETURNING CHARACTERS

##' @export
as_rhs_chr.character <- function(object, string=TRUE){
    ob <- object |> to_str(collapse = "")
    rev(strsplit(ob,"\\s~\\s")[[1]])[1]
}

##' @export
as_rhs_chr.formula <- function(object, string=TRUE){
    formula_rhs_to_chr(object, string=string)    
}

##' @export
as_lhs_chr.character <- function(object, string=TRUE){
    object2 <- strsplit(object,"\\s~\\s")[[1]]
    if (length(object2) == 2)
        object2[1]
    else
        character(0)
}

##' @export
as_lhs_chr.formula <- function(object, string=FALSE){
    r <- terms(object) |> attr("response")
    if (r > 0)
        object <- (terms(object) |> attr("variables"))[[r+1]] |> as.character()
    else
        object <- character(0)
    object
}


## EXPORTED

##' @rdname formula_ops
##' @export
as_rhs_frm <- function(object){
    UseMethod("as_rhs_frm")    
}

##' @rdname formula_ops
##' @export
as_lhs_frm <- function(object){
    UseMethod("as_lhs_frm")
}

##' @rdname formula_ops
##' @export
as_rhs_chr <- function(object, string=FALSE){
    UseMethod("as_rhs_chr")
}

##' @rdname formula_ops
##' @export
as_lhs_chr <- function(object, string=FALSE){
    UseMethod("as_lhs_chr")
}


##' @rdname formula_ops
##' @param list_of_formulas list of formulas
##' @export
unique_formula <- function(list_of_formulas){

    l2 <- lapply(list_of_formulas, function(x){
        environment(x) <- NULL
        x
    }
    )

    o <- list_of_formulas[!duplicated(l2)]
    return(o)
}

## ##' @export
## formula_to_rhs <- function(frm1){
##     terms(frm1) |> delete.response() |> formula()
## }

## FIXME: REPLACE
## formula_chr_to_rhs <- function(chr1){
##     o <- to_str(chr1)
##     as.formula(paste("~", o))
## }



