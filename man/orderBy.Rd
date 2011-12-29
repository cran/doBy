\name{orderBy}
\alias{orderBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ordering (sorting) rows of a data frame}
\description{
 Ordering (sorting) rows of a data frame by the certain variables in the
 data frame. This function is essentially a wrapper for the order()
 function - the important difference being that variables to order by
 can be given by a model formula.
}
\usage{
orderBy(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{The right hand side of a formula}
  \item{data}{A data frame}
%  \item{na.last}{for controlling the treatment of 'NA's. If 'TRUE', missing
%          values in the data are put last; if 'FALSE', they are put
%          first; if 'NA', they are removed. }
%  \item{decreasing}{logical. Should the sort order be increasing or decreasing?}
}
\details{
  The sign of the terms in the formula determines whether sorting should
  be ascending or decreasing; see examples below
}
\value{
  The ordered data frame
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk and Kevin Wright}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
  %\code{\link{orderBy}},
  \code{\link{summaryBy}},
  \code{\link{transformBy}},
  \code{\link{splitBy}},
  \code{\link{lapplyBy}},
}

\examples{
data(dietox)
orderBy(~Time+Evit, data=dietox)
## Sort decreasingly by Time
orderBy(~-Time+Evit, data=dietox)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
