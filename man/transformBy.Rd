\name{transformBy}
\alias{transformBy}

\title{ Function to make groupwise transformations}
\description{
  Function to make groupwise transformations of data by applying the
  transform function to subsets of data.
}
\usage{
transformBy(formula, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula with only a right hand side, see examples below}
  \item{data}{A data frame}
  \item{\dots}{Further arguments of the form tag=value}
}
\details{

  The ... arguments  are tagged vector
  expressions, which are evaluated in the data frame data. The tags are
  matched against names(data), and for those that match, the value
  replace the corresponding variable in data, and the others are appended
  to data.
}
\value{
  The modified value of the dataframe data.
}
%%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{
  \code{\link{orderBy}},
  \code{\link{summaryBy}},
  \code{\link{splitBy}},
  \code{\link{doby.xtabs}},
}



\examples{

data(dietox)
transformBy(~Pig, data=dietox, minW=min(Weight), maxW=max(Weight), 
    gain=sum(range(Weight)*c(-1,1)))

}
\keyword{univar}
