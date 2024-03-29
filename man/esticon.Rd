% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esticon.R
\name{esticon}
\alias{esticon}
\alias{coef.esticon_class}
\alias{summary.esticon_class}
\alias{confint.esticon_class}
\alias{vcov.esticon_class}
\title{Contrasts for lm, glm, lme, and geeglm objects}
\usage{
esticon(obj, L, beta0, conf.int = TRUE, level = 0.95, joint.test = FALSE, ...)

\method{coef}{esticon_class}(object, ...)

\method{summary}{esticon_class}(object, ...)

\method{confint}{esticon_class}(object, parm, level = 0.95, ...)

\method{vcov}{esticon_class}(object, ...)
}
\arguments{
\item{obj}{Regression object (of type lm, glm, lme, geeglm).}

\item{L}{Matrix (or vector) specifying linear functions of the regression
parameters (one
linear function per row).  The number of columns must match the number of
fitted regression parameters in the model. See 'details' below.}

\item{beta0}{A vector of numbers}

\item{conf.int}{TRUE}

\item{level}{The confidence level}

\item{joint.test}{Logical value. If TRUE a 'joint' Wald test for the
hypothesis L beta = beta0 is made. Default is that the 'row-wise' tests are
made, i.e. (L beta)i=beta0i.  If joint.test is TRUE, then no confidence
interval etc. is calculated.}

\item{\dots}{Additional arguments; currently not used.}

\item{object}{An \code{esticon_class} object.}

\item{parm}{a specification of which parameters are to be given
confidence intervals, either a vector of numbers or a vector
of names.  If missing, all parameters are considered.}
}
\value{
Returns a matrix with one row per linear function.  Columns contain
estimated coefficients, standard errors, t values, degrees of freedom,
two-sided p-values, and the lower and upper endpoints of the 1-alpha
confidence intervals.
}
\description{
Computes linear functions (i.e. weighted sums) of the
estimated regression parameters. Can also test the hypothesis,
that such a function is equal to a specific value.
}
\details{
Let the estimated parameters of the model be
\deqn{\beta_1, \beta_2, \dots, \beta_p}

A linear function of the estimates is of the form \deqn{l=\lambda_1
    \beta_1+\lambda_2 \beta_2+ \dots+\lambda_p \beta_p} where
\eqn{\lambda_1, \lambda_2, \dots,\lambda_p} is specified by the
user.

The esticon function calculates l, its standard error and by default also a
95 pct confidence interval.  It is sometimes of interest to test the
hypothesis \eqn{H_0: l=\beta_0} for some value \eqn{\beta_0}
given by the user. A test is provided for the hypothesis \eqn{H_0:
l=0} but other values of \eqn{\beta_0} can be specified.

In general, one can specify r such linear functions at one time by
specifying L to be an \eqn{r\times p} matrix where each row consists
of p numbers \eqn{\lambda_1,\lambda_2,\dots, \lambda_p}. Default is
then that \eqn{\beta_0} is a p vector of 0s but other values can be
given.

It is possible to test simultaneously that all specified linear functions
are equal to the corresponding values in \eqn{\beta_0}.

For computing contrasts among levels of a single factor, 'contrast.lm' may
be more convenient.
}
\examples{

data(iris)
lm1  <- lm(Sepal.Length ~ Sepal.Width + Species + Sepal.Width : Species, data=iris)
## Note that the setosa parameters are set to zero
coef(lm1)

## Estimate the intercept for versicolor
lambda1 <- c(1, 0, 1, 0, 0, 0)
esticon(lm1, L=lambda1)

## Estimate the difference between versicolor and virgica intercept
## and test if the difference is 1
lambda2 <- c(0, 1, -1, 0, 0, 0)
esticon(lm1, L=lambda2, beta0=1)

## Do both estimates at one time
esticon(lm1, L=rbind(lambda1, lambda2), beta0=c(0, 1))

## Make a combined test for that the difference between versicolor and virgica intercept
## and difference between versicolor and virginica slope is zero:
lambda3 <- c(0, 0, 0, 0, 1, -1)
esticon(lm1, L=rbind(lambda2, lambda3), joint.test=TRUE)

# Example using esticon on coxph objects (thanks to Alessandro A. Leidi).
# Using dataset 'veteran' in the survival package
# from the Veterans' Administration Lung Cancer study

if (require(survival)){
data(veteran)
sapply(veteran, class)
levels(veteran$celltype)
attach(veteran)
veteran.s <- Surv(time, status)
coxmod <- coxph(veteran.s ~ age + celltype + trt, method='breslow')
summary(coxmod)

# compare a subject 50 years old with celltype 1
# to a subject 70 years old with celltype 2
# both subjects on the same treatment
AvB <- c(-20, -1, 0, 0, 0)

# compare a subject 40 years old with celltype 2 on treat=0
# to a subject 35 years old with celltype 3 on treat=1
CvB <- c(5, 1, -1, 0, -1)

est <- esticon(coxmod, L=rbind(AvB, CvB))
est
##exp(est[, c(2, 7, 8)])
}
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{utilities}
