\name{subsetBy}
\alias{subsetBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finds subsets of a dataframe which is split by variables in a formula.}
\description{
  A data frame is split by a formula into groups. Then subsets are found
  within each group, and the result is collected into a data frame.
  
}
\usage{
subsetBy(formula, subset, data = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula to split by}
  \item{subset}{A string defining the subset operation}
  \item{data}{A data frame}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A data frame.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{See Also \code{\link{splitBy}}}
\examples{
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{}% __ONLY ONE__ keyword per line
