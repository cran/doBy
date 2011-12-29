\name{doBy}
\alias{doBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Various utilities which includes functions for creating groupwise
  calculations etc.}
\description{
  The core doBy functions were developed to make it easy to split data
  into groups (defined by the levels of a set of factors) and performing
  some actions on each of these groups. Thus, these functions mimic what
  can be achieved using the BY statement in various SAS procedures.

  In addition hereto the doBy package containts various other utilities.
  }

\details{
  Functions summaryBy, splitBy, orderBy, sampleBy, transformBy are the core doBy
  functions

  There is no need for a  plotBy function -- the 
  xyplot function in the lattice package already fulfills these needs

  The esticon function calculates linear functions of parameter
  estimates under various types of models.

  There are various other utility functions in the package.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}

\seealso{
  \code{\link{summaryBy}},
  \code{\link{orderBy}},
  \code{\link{transformBy}},
  \code{\link{splitBy}},
  \code{\link{sampleBy}}
  }


\examples{

data(dietox)

summaryBy(Weight+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean,var),
na.rm=TRUE, use="pair")  

orderBy(~Time+Evit, data=dietox)

splitBy(formula = ~Evit+Cu, data = dietox)

sampleBy(formula = ~Evit+Cu, frac=.1, data = dietox)

}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line



