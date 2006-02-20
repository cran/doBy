\name{esticon}
\alias{esticon}
\alias{esticon.geeglm}
\alias{esticon.glm}
\alias{esticon.lm}
\alias{esticon.lme}
\alias{esticon.lmer}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Contrasts for lm, glm, lme, and geeglm objects}
\description{
  Computes linear functions (i.e. weighted sums) of the estimated
  regression parameters.

  Can also test the hypothesis, that such a function is equal to a
  specific value.
}
\usage{
esticon(obj, cm, beta0, conf.int = TRUE, level=0.95, joint.test = FALSE)
%esticon2(obj, cm, beta0, conf.int = "wald", level=0.95, joint.test = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Regression object (of type lm, glm, lme, geeglm)}
  \item{cm}{Matrix specifying linear functions of the regresson parameters
    (one linear function per row).
    The number of columns must match the number of
    fitted regression parameters in the model. See 'details' below.}
  \item{beta0}{A vector of numbers}
  \item{conf.int}{TRUE}
  \item{level}{The confidence level}
  \item{joint.test}{Logical value. If TRUE a 'joint' Wald test for the
    hypothesis L beta=beta0 is made. Default is that the 'row-wise'
    tests are made, i.e. (L beta)i=beta0i.
    If joint.test is TRUE, then no confidence inteval etc. is calculated.
  }
}
\details{
  Let the estimated rparameters of the model be 
  \deqn{latex}{\beta_1, \beta_2, \dots,\beta_p}
  A linear function of the estimates is of the form
  \deqn{latex}{c=\lambda_1 \beta_1+\lambda2 \beta_2+ \dots+\lambda_p \beta_p}
  where \eqn{latex}{\lambda_1, \lambda_2, \dots,\lambda_p} is specified
  by the user.

  The esticon function calculates c, its standard error and by default
  also a 95 pct confidence interval. 
  It is sometimes of interest to test the hypothesis \eqn{latex}{H_0:
    c=\beta_0} for some value \eqn{latex}{\beta_0} given by the user. A
  test is provided for the hypothesis \eqn{latex}{H_0:
    c=0} but other values of \eqn{latex}{\beta_0} can be specified.
  
  In general, one can specify r such linear functions at one time by
  speficying cm to be an \eqn{latex}{r\times p} matrix where each row 
  consists of p numbers \eqn{latex}{\lambda_1,\lambda_2,\dots,
    \lambda_p}. Default is then that \eqn{latex}{\beta_0} is a p vector
  of 0s but other values can be given.

  It is possible to test simulatneously that all speficied linear
  functions are equal to the corresponding values in
  \eqn{latex}{\beta_0}.
  
   For computing contrasts among levels of
   a single factor, 'contrast.lm' may be more convenient.

     
}
\value{
  Returns a matrix with one row per linear function.  Columns
     contain estimated coefficients, standard errors, t values, degrees
     of freedom, two-sided p-values, and the lower and  upper endpoints
     of the 1-alpha confidence intervals.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{'esticon' is modification of the 'estimable' function from the
  gregmisc package: First, 'esticon' works on geese objects from the
  geepack package (for Generalized Estimating Equations). Secondly, and
  contrary to 'estimable', bases all inference for glms on the Wald
  statistic.
}

%\seealso{\code{\link{wald}}}
\examples{
data(iris)
lm1  <- lm(Sepal.Length~Sepal.Width+Species+Sepal.Width:Species, data=iris)
## Note that the setosa parameters are set to zero
coef(lm1)

## Estimate the intercept for versicolor
lambda1 <- c(1,0,1,0,0,0)
esticon(lm1,lambda1)

## Estimate the difference between versicolor and virgica intercept
## and test if the difference is 1
lambda2 <- c(0,1,-1,0,0,0)
esticon(lm1,lambda2,beta0=1)

## Do both estimates at one time
esticon(lm1,rbind(lambda1,lambda2),beta0=c(0,1))

## Make a combined test for that the difference between versicolor and virgica intercept
## and difference between versicolor and virginica slope is zero:
lambda3 <- c(0,0,0,0,1,-1)
esticon(lm1,rbind(lambda2,lambda3),joint.test=TRUE)
}
\keyword{utilities}% at least one, from doc/KEYWORDS

