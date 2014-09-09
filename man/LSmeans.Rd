\name{LSmeans}
\alias{LSmeans}
\alias{LSmeans.default}
\alias{LSmeans.lmerMod}
\alias{popMeans}
\alias{popMeans.default}
\alias{popMeans.lmerMod}

%% \alias{LSmeans.glm}
%% \alias{LSmeans.lm}
%% \alias{LSmeans.geeglm}
%% \alias{LSmeans.merMod}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute linear estimates, including LS-means (aka population means or
marginal means)
}
\description{
Compute linear estimates for a range of models. One example of linear
estimates is LS-means (least squares means, also known as population
means and as marginal means).
}
\usage{
LSmeans(object, effect = NULL, at = NULL, level=0.95,
      %only.at = TRUE, grid = TRUE,
      %df.adjust = FALSE,
      ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    Model object
  }
  \item{effect}{
    A vector of variables. For each configuration of these the estimate
    will be calculated.
  }
  \item{at}{
    A list of values of covariates (including levels of some factors)
    to be used in the calculations
  }
  \item{level}{
    The level of the (asymptotic) confidence interval.
  }

  \item{...}{
    Additional arguments; currently not used.
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  A dataframe with results from computing the contrasts.
  }
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The \code{LSmeans} method is a recent addition to the package, and it
  will eventually replace the \code{popMeans} method.

  Please report unexpected behaviour.

  Some of the code has been adapted from the \bold{lsmeans} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{LSmatrix}}
  \code{\link{linest}}
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

mod.bal  <- lm(y ~ AA + BB*CC,    data=dat.bal)
mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)

LSmeans(mod.bal, effect=c("BB", "CC"))
LSmeans(mod.nst, effect=c("BB", "CC"))

LSmeans(mod.nst, at=list(BB=1, CC=1))

LSmeans(mod.nst, at=list(BB=1, CC=2))
## Above: NA's are correct; not an estimable function




if( require( lme4 )){
warp.mm <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)
LSmeans(warp.mm, effect="tension")
class(warp.mm)
fixef(warp.mm)
coef(summary(warp.mm))
vcov(warp.mm)
if (require(pbkrtest ))
vcovAdj(warp.mm)
}



%% LSmeans(warp.mm, effect="tension")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

