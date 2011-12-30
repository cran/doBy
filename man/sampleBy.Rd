\name{sampleBy}
\alias{sampleBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sampling from a data frame}
\description{
  A data frame is split according to some variables in a formula, and a
  sample of a certain fraction of each is drawn.
}
\usage{
sampleBy(formula, frac = 0.1, replace=FALSE, data = parent.frame(),systematic=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula defining the grouping of the data frame}
  \item{frac}{The part of data to be sampled.}
  \item{replace}{Is the sampling with replacement}
  \item{data}{A data frame}
  \item{systematic}{Should sampling be systematic.}
}
\details{
  If systematic=FALSE (default) then frac gives the fraction of data
  sampled. If systematic=TRUE and frac=.2 then every 1/.2 i.e. every 5th
  observation is taken out. 
}
\value{
A data frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
  \code{\link{orderBy}},
  \code{\link{summaryBy}},
  \code{\link{transformBy}},
  \code{\link{splitBy}},
}


\examples{
data(dietox)
sampleBy(formula = ~Evit+Cu, frac=.1, data = dietox)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
