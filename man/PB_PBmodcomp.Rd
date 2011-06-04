\name{PBmodcomp}
\alias{PBmodcomp}
\alias{PBmodcomp.mer}
\alias{BCmodcomp}
\alias{BCmodcomp.mer}

\alias{as.data.frame.XXmodcomp}
\alias{print.XXmodcomp}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Model comparison of mixed models using parametric bootstrap methods.
}
\description{
    Model comparison of mixed models using parametric bootstrap methods.
}
\usage{
PBmodcomp(largeModel, smallModel, nsim = 200, ref = NULL, cl = NULL, details = 0)
\method{PBmodcomp}{mer}(largeModel, smallModel, nsim = 200, ref = NULL, cl = NULL, details = 0)
BCmodcomp(largeModel, smallModel, nsim = 200, ref = NULL, cl = NULL, details = 0)
\method{BCmodcomp}{mer}(largeModel, smallModel, nsim = 200, ref = NULL, cl = NULL, details = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{largeModel}{
    A linear mixed effects model as fitted with the \code{lmer()}
    function in the \pkg{lme4} package. This model muse be larger than
    \code{smallModel} (see below).
  }
  \item{smallModel}{
    A linear mixed effects model as fitted with the \code{lmer()}
    function in the \pkg{lme4} package. This model muse be smaller than
    \code{largeModel} (see above).
  }
  \item{nsim}{
    The number of simulations to form the reference distribution.
  }
  \item{ref}{
    Vector containing samples from the reference distribution. If NULL,
    this vector will be generated using PBrefdist(). }
  \item{cl}{
    A vector identifying a cluster; used for
    calculating the reference distribution using several cores. See
    examples below.
  }
  \item{details}{
    The amount of output produced. Mainly relevant for debugging
    purposes.
  }
}
\details{
  ## To be added...
}
\value{
  ## To be added...
}
\references{
  ## To be added...
}
\author{
  Søren Højsgaard <sorenh at agrsci dot dk>
}
\note{
  This functionality is a recent addition to the doBy package. The
  functionality is not thoroughly tested and should be used with
  care. Please do report bugs etc.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{KRmodcomp}}
}
\examples{
library(doBy)
data(beets)
head(beets)
beet0<-lmer(sugpct~block+sow+harvest+(1|block:harvest), data=beets, REML=FALSE)
beet_no.harv <- update(beet0, .~.-harvest)
BCmodcomp(beet0, beet_no.harv, nsim=20)

\dontrun{
## Vanilla
PBmodcomp(beet0, beet_no.harv)
BCmodcomp(beet0, beet_no.harv)
## Simulate reference distribution separately:
rr <- PBrefdist(beet0, beet_no.harv, nsim=20)
PBmodcomp(beet0, beet_no.harv, ref=rr)
BCmodcomp(beet0, beet_no.harv, ref=rr)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{inference}