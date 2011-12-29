\name{subsetBy}
\alias{subsetBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finds subsets of a dataframe which is split by variables in a formula.}
\description{
  A data frame is split by a formula into groups. Then subsets are found
  within each group, and the result is collected into a data frame.
  
}
\usage{
subsetBy(formula, subset, data = parent.frame(), select, drop=FALSE,
join=TRUE, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula to split by}
  \item{subset}{logical expression indicating elements or rows to keep:
    missing values are taken as false.} 
  \item{data}{A data frame}
  \item{select}{expression, indicating columns to select from a data frame.}
  \item{drop}{passed on to [ indexing operator.}
  \item{join}{If FALSE the result is a list of data frames (as defined by
    'formula'); if TRUE one data frame is returned.  }
  \item{...}{further arguments to be passed to or from other methods.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A data frame.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{See Also \code{\link{splitBy}}}
\examples{
data(dietox)
subsetBy(~Evit, Weight < mean(Weight), data=dietox)
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{}% __ONLY ONE__ keyword per line
