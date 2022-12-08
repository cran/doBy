## #############################################################################
#'
#' @title Compute  LS-means (aka population means or
#'     marginal means)
#' @description LS-means (least squares means, also known as
#'     population means and as marginal means) for a range of model types.
#' @name ls-means
#' 
## #############################################################################
#' 
#' @details There are restrictions on the formulas allowed in the model object.
#'     For example having \code{y ~ log(x)} will cause an error. Instead one
#'     must define the variable \code{logx = log(x)} and do \code{y ~ logx}.
#' 
#' @aliases LSmeans LSmeans.default LSmeans.lmerMod popMeans
#'     popMeans.default popMeans.lmerMod
#' 
#' @param object Model object
#' @param effect A vector of variables. For each configuration of
#'     these the estimate will be calculated.
#' @param at A list of values of covariates (including levels of some
#'     factors) to be used in the calculations
#' @param level The level of the (asymptotic) confidence interval.
#' @param adjust.df Should denominator degrees of freedom be adjusted?
#' @param \dots  Additional arguments; currently not used.
#' 
#' @return A dataframe with results from computing the contrasts.
#'
#' @note \code{LSmeans} and \code{popMeans} are synonymous. Some of
#'     the code has been inspired by the \bold{lsmeans} package.
#' 
#' @section Warning: Notice that \code{LSmeans} and \code{LE_matrix}
#'     fails if the model formula contains an offset (as one would
#'     have in connection with e.g. Poisson regression. 
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{LE_matrix}}, \code{\link{linest}}
#' @keywords utilities
#' @examples
#' 
#' ## Two way anova:
#' 
#' data(warpbreaks)
#' 
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#' m1 <- lm(breaks ~ wool * tension, data=warpbreaks)
#' LSmeans(m0)
#' LSmeans(m1)
#' 
#' ## same as:
#' K <- LE_matrix(m0);K
#' linest(m0, K)
#' K <- LE_matrix(m1);K
#' linest(m1, K)
#' 
#' LE_matrix(m0, effect="wool")
#' LSmeans(m0, effect="wool")
#' 
#' LE_matrix(m1, effect="wool")
#' LSmeans(m1, effect="wool")
#' 
#' LE_matrix(m0, effect=c("wool", "tension"))
#' LSmeans(m0, effect=c("wool", "tension"))
#' 
#' LE_matrix(m1, effect=c("wool", "tension"))
#' LSmeans(m1, effect=c("wool", "tension"))
#' 
#' 
#' ## Regression; two parallel regression lines:
#' 
#' data(Puromycin)
#' 
#' m0 <- lm(rate ~ state + log(conc), data=Puromycin)
#' ## Can not use LSmeans / LE_matrix here because of
#' ## the log-transformation. Instead we must do:
#' Puromycin$lconc <- log( Puromycin$conc )
#' m1 <- lm(rate ~ state + lconc, data=Puromycin)
#' 
#' LE_matrix(m1)
#' LSmeans(m1)
#' 
#' LE_matrix(m1, effect="state")
#' LSmeans(m1, effect="state")
#' 
#' LE_matrix(m1, effect="state", at=list(lconc=3))
#' LSmeans(m1, effect="state", at=list(lconc=3))
#' 
#' ## Non estimable contrasts
#' 
#' ## ## Make balanced dataset
#' dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3),
#'                             CC=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## ## Make unbalanced dataset
#' #      'BB' is nested within 'CC' so BB=1 is only found when CC=1
#' #       and BB=2,3 are found in each CC=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$CC <-factor(c(1, 1, 2, 2, 2, 2, 1, 1, 3, 3,
#'                       3, 3, 1, 1, 4, 4, 4, 4))
#' 
#' mod.bal  <- lm(y ~ AA + BB * CC,    data=dat.bal)
#' mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)
#' 
#' LSmeans(mod.bal, effect=c("BB", "CC"))
#' LSmeans(mod.nst, effect=c("BB", "CC"))
#' LSmeans(mod.nst, at=list(BB=1, CC=1))
#' 
#' LSmeans(mod.nst, at=list(BB=1, CC=2))
#' ## Above: NA's are correct; not an estimable function
#' 
#' if( require( lme4 )){
#'  warp.mm <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)
#'  LSmeans(warp.mm, effect="tension")
#'  class(warp.mm)
#'  fixef(warp.mm)
#'  coef(summary(warp.mm))
#'  vcov(warp.mm)
#'  if (require(pbkrtest))
#'    vcovAdj(warp.mm)
#' }
#' 
#' LSmeans(warp.mm, effect="tension")
#' 
#' @export LSmeans
#'


#' @export
#' @rdname ls-means
LSmeans <- function(object, effect=NULL, at=NULL, level=0.95, ...){
    UseMethod("LSmeans")
}

#' @export
#' @rdname ls-means
LSmeans.default <- function(object, effect=NULL, at=NULL, level=0.95, ...){
    K   <- LE_matrix(object, effect=effect, at=at)
    out <- linest(object, K, level=level, ...)
    out
}

#' @export
#' @rdname ls-means
LSmeans.lmerMod <- function(object, effect=NULL, at=NULL, level=0.95, adjust.df=TRUE, ...){
    K   <- LE_matrix(object, effect=effect, at=at)
    out <- linest(object, K, level=level, adjust.df=adjust.df, ...)
    out
}

#' @export
#' @rdname ls-means
popMeans         <- LSmeans

#' @export
#' @rdname ls-means
popMeans.default <- LSmeans.default

#' @export
#' @rdname ls-means
popMeans.lmerMod <- LSmeans.lmerMod


