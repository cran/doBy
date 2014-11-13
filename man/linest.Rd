\name{linest}
\alias{linest}
\alias{linest.lm}
\alias{linest.glm}
\alias{linest.geeglm}
\alias{linest.lmerMod}
\alias{linest.merMod}
\alias{linest.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute linear estimates
}
\description{
Compute linear estimates for a range of models. One example of linear
estimates is population means (also known as LSMEANS).
}
\usage{
linest(object, K=NULL, level=0.95, ...) %df.adjust = FALSE,
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    Model object
  }
  \item{K}{
    Either \code{NULL} or a matrix with p columns where p is the number
    of parameters in the systematic
    effects in the model. If \code{NULL} then \code{K} is taken to be
    the p times p identity matrix
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
%% \note{
%%   The \code{linest} method is a recent addition to the package, and it
%%   will eventually replace the \code{popMeans} method.

%%   Please report unexpected behaviour.

%%   Some of the code has been adapted from the \bold{lsmeans} package.
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{LSmeans}}
   \code{\link{LSmatrix}}
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

K <- LSmatrix(mod.nst, effect=c("BB","CC"))
linest( mod.nst, K )
}



%% linest(warp.mm, effect="tension")


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

