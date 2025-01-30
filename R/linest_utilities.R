
#' @title Determines if contrasts are estimable.
#' 
#' @description Determines if contrasts are estimable, that is, if the contrasts
#'     can be written as a linear function of the data.
#'
#' @name is_estimable
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
#' @references \url{http://web.mit.edu/18.06/www/Essays/newpaper_ver3.pdf}
#' @keywords utilities
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


null_basis <- function(M){
    MASS::Null(t(M))
}
