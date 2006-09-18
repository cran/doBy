\name{splitBy}
\alias{splitBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a data frame}
\description{
  This is essentially a wrapper for the 'split' function,
- the important difference being that variables to order by
 can be given by a model formula. 
}
\usage{
splitBy(formula, data = parent.frame(), drop=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{The right hand side of a formula}
  \item{data}{A data frame}
  \item{drop}{Logical indicating if levels that do not occur should be dropped }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list of data frames
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(dietox)
splitBy(formula = ~Evit+Cu, data = dietox)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
