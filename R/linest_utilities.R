#' @title Finds the basis of the (right) null space.
#' 
#' @description Finds the basis of the (right) null space of a matrix, a vector
#'     (a 1-column matrix) or a model object for which a model matrix can be
#'     extracted. I.e. finds basis for the (right) null space x : Mx = 0.
#'
#' @name null-basis
#' 
#' @param object A matrix, a vector (a 1-column matrix) or a model object for
#'     which a model matrix can be extracted (using \code{model.matrix}).
#' @return A matrix (possibly with zero columns if the null space consists only
#'     of the zero vector).
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link[MASS]{Null}}
#' @keywords utilities
#' @examples
#' 
#' M <- matrix(c(1,1,1,1,1,1,0,0,0,0,1,1), nrow=4)
#' null_basis(M)
#' MASS::Null(t(M))
#' 
#' M <- c(1,1,1,1)
#' null_basis(M)
#' MASS::Null(t(M))
#' 
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#' null_basis(m0)
#' MASS::Null(t(model.matrix(m0)))
#' 
#' ## Make balanced dataset
#' dat.bal   <- expand.grid(list(A=factor(1:2), B=factor(1:3), C=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## Make unbalanced dataset: 'B' is nested within 'C' so B=1 is only
#' ## found when C=1 and B=2,3 are found in each C=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$C <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
#' xtabs(y ~ C+B+A , data=dat.nst)
#' 
#' mod.bal  <- lm(y ~ A + B*C, data=dat.bal)
#' mod.nst  <- lm(y ~ A + B*C, data=dat.nst)
#' 
#' null_basis( mod.bal )
#' null_basis( mod.nst )
#' 
#' null_basis( model.matrix(mod.bal) )
#' null_basis( model.matrix(mod.nst) )
#' 
#' MASS::Null( t(model.matrix(mod.bal)) )
#' MASS::Null( t(model.matrix(mod.nst)) )
#' 
#' @export null_basis

#' @export
null_basis <- function(object){

    .null_basis <- function(object){
        S <- svd( object )
        id <- S$d<1e-15
        if (any(id)){
            null.basis <- S$v[,id, drop=FALSE]
            null.basis
        }
        else {
            matrix(nrow=ncol(object), ncol=0)
        }
    }
    
    if (is.character(object)){
        stop("'object' of type 'character' not valid")
    }
    
    if (is.vector(object)){
        object <- matrix(object, ncol=1)
    }


    ##if (class(object) %in% c("matrix","Matrix")){
    ##    .null_basis(object)

    if (inherits(object, c("matrix", "Matrix"))){
        .null_basis(object)
    } else {
        m <- try(model.matrix(object), silent=TRUE)
        ##if (class(m) != "try-error")
        if (!inherits(m, "try-error"))
            .null_basis(m)
        else
            stop("Can not find null basis for 'object'")
    }
}




#' @title Determines if contrasts are estimable.
#' 
#' @description Determines if contrasts are estimable, that is, if the contrasts
#'     can be written as a linear function of the data.
#'
#' @name is-estimable
#' 
#' @details Consider the setting \eqn{E(Y)=Xb}. A linear function of \eqn{b},
#'     say \eqn{l'b} is estimable if and only if there exists an \eqn{r} such
#'     that \eqn{r'X=l'} or equivalently \eqn{l=X'r}. Hence \eqn{l} must be in
#'     the column space of \eqn{X'}, i.e. in the orthogonal complement of the
#'     null space of \eqn{X}. Hence, with a basis \eqn{B} for the null space,
#'     \code{is_estimable()} checks if each row \eqn{l} of the matrix \eqn{K} is
#'     perpendicular to each column basis vector in \eqn{B}.
#' 
#' @param K A matrix.
#' @param null.basis A basis for a null space (can be found with
#'     \code{null_basis()}). 
#' @return A logical vector. 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{null_basis}}
#' @references \url{http://web.mit.edu/18.06/www/Essays/newpaper_ver3.pdf}
#' @keywords utilities
#' @examples
#' 
#' ## TO BE WRITTEN
#' 
#' @export is_estimable
is_estimable <- function(K, null.basis){
    if (is.null(null.basis) ||
        (is.matrix(null.basis) && ncol(null.basis)==1) ){
        rep(TRUE, nrow(K))
    } else {
        out <- lapply(1:nrow(K),
                      function(i){
                          k <- K[i,]
                          all(abs(apply(null.basis, 2, function(x) sum(k * x))) < 1e-04)
                      })
        unlist( out )
    }
}


