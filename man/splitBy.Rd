\name{splitBy}
\alias{splitBy}
\alias{print.splitByData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a data frame}
\description{
  This is essentially a wrapper for the 'split' function,
  - the important difference being that variables to order by
  can be given by a model formula. 
}
\usage{
splitBy(formula, data = parent.frame(), drop=TRUE, return.matrix=FALSE)
\method{print}{splitByData}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{The right hand side of a formula}
  \item{data}{A data frame}
  \item{drop}{Logical indicating if levels that do not occur should be
    dropped }
  \item{return.matrix}{Should the returned list consist of dataframs or
    matrices, see 'details' below}
  \item{x}{A splitByData object (basically a list).}
  \item{...}{Additional arguments, currently not used.}
}
\details{
  The funcion transform the dataframe 'data' into a numerical matrix
  (using the 'asNumericMatrix' function from the Hmisc package) and
  makes the splitting operation on this. If return.matrix is TRUE, then
  these matrices are returned, otherwise the matrices are turned into
  dataframes and then these are returned. 
}
\value{
  A list of dataframes of matrices
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}


\seealso{
  \code{\link{orderBy}},
  \code{\link{summaryBy}},
  \code{\link{transformBy}},
  %\code{\link{splitBy}},
  \code{\link{lapplyBy}},
}

\examples{
data(dietox)
splitBy(formula = ~Evit+Cu, data = dietox)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
