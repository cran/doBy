\name{lsmeans}
\alias{lsmeans}
\alias{lsmeans.lm}
\alias{print.lsmeansTable}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculates LSMEANS (population means)
}
\description{
  LSMEANS is SAS-jargon for what is sometimes called population means. 
}
\usage{
lsmeans(object, eff, at = NULL)
\method{lsmeans}{lm}(object, eff, at = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A model object; currently only available for 'lm' objects
  }
  \item{eff}{
    A vector of variables for which the LSMEANS should be calculated.
}
  \item{at}{
    A list of covariate values
}
}
\details{
  FIXME: To be written
}
\value{
  An 'lsmeansTable' object which is a dataframe with a few additional
  attributes. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard, sorenh@agrsci.dk
}
%\note{
%%  ~~further notes~~
%}

\section{Warning}{
  This function is a recent addition to the 'doBy' package and it is not
  thoroughly tested. Use with caution!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  FIXME: To be added %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat$y <- rnorm(nrow(dat))
dat$x <- rnorm(nrow(dat))
dat$x2 <- dat$x^2

## Examples

## 1) LSMEANS with factors only.
mod1 <- lm(y ~ AA + BB*CC + x, data=dat)
## Average over AA for each combination of (BB,CC); evaluate at x=mean(x)
lsmeans(mod1, c("BB","CC"))
## Average over (AA,BB) for each value of CC; evaluate at x=mean(x)
lsmeans(mod1, c("CC"))

## 2) The call to lsmeans() below is eqivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##  lsmeans CC BB*CC / stderr;
## run;
##
mod2 <- lm(y ~ AA + BB*CC + x + x2, data=dat)
lsmeans(mod2, "CC")

## Notice the difference to:
mod3 <- lm(y ~ AA + BB*CC + x + I(x^2), data=dat)
lsmeans(mod3, "CC")

## The difference arises because in the former case, x2 is evaluated at mean(x2) whereas
## in the latter case x is evaluated at mean(x)^2

## 3) Plug in particular values of covariates
## The call to lsmeans() below is eqivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x2;
##  lsmeans CC BB*CC / at x=2 stderr;
## run;
lsmeans(mod2, c("CC"), at=list(x=2))
## Above, x=2 is used while x2 is set to mean(x2)

## The call to lsmeans() below is eqivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##    lsmeans CC BB*CC / at x=2 stderr;
## run;
##
lsmeans(mod3, c("CC"), at=list(x=2))
## Above, x=2 is used while I(x^2) is set to mean(x^2)=4. Notice that setting
## lsmeans(mod3, c("CC"), at=list(x=2,"I(x^2)"=123))
## has no effect: I(x^2) is still 4 because x=2. Hence the following two results
## are identical

lsmeans(mod2, c("CC"), at=list(x=2, x2=4))
lsmeans(mod3, c("CC"), at=list(x=2))

## END 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilites}% __ONLY ONE__ keyword per line
