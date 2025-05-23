% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linest_compute.R
\name{linest}
\alias{linest}
\alias{linest.lm}
\alias{linest.glm}
\alias{linest.geeglm}
\alias{linest.lmerMod}
\alias{linest.merMod}
\alias{linest.default}
\alias{confint.linest_class}
\alias{coef.linest_class}
\alias{summary.linest_class}
\title{Compute linear estimates}
\usage{
linest(object, L = NULL, level = 0.95, ...)

\method{confint}{linest_class}(object, parm, level = 0.95, ...)

\method{coef}{linest_class}(object, ...)

\method{summary}{linest_class}(object, ...)
}
\arguments{
\item{object}{Model object}

\item{L}{Either \code{NULL} or a matrix with p columns where p is
the number of parameters in the systematic effects in the
model. If \code{NULL} then \code{L} is taken to be the p times
p identity matrix}

\item{level}{The level of the (asymptotic) confidence interval.}

\item{\dots}{Additional arguments; currently not used.}

\item{parm}{Specification of the parameters estimates for which
confidence intervals are to be calculated.}

\item{confint}{Should confidence interval appear in output.}
}
\value{
A dataframe with results from computing the contrasts.
}
\description{
Compute linear estimates, i.e. \code{L \%*\% beta} for a range of models. One example of
linear estimates is population means (also known as LSMEANS).
}
\examples{

## Make balanced dataset
dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset
#   'BB' is nested within 'CC' so BB=1 is only found when CC=1
#   and BB=2,3 are found in each CC=2,3,4
dat.nst <- dat.bal
dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))

mod.bal  <- lm(y ~ AA + BB * CC, data=dat.bal)
mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)

L <- LE_matrix(mod.nst, effect=c("BB", "CC"))
linest( mod.nst, L )

}
\seealso{
\code{\link{LSmeans}}, \code{\link{LE_matrix}}
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{utilities}
