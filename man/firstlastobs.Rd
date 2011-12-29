\name{firstlastobs}
\alias{firstobs}
\alias{lastobs}
\alias{firstobs.default}
\alias{lastobs.default}
\alias{firstobs.formula}
\alias{lastobs.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Locate the index of the first/last unique value}
\description{
  Locate the index of the first/last unique value in i) a vector or
  of a variable in a data frame. 
}
\usage{
\method{firstobs}{formula}(formula, data=parent.frame(), ...)
\method{lastobs}{formula}(formula, data=parent.frame(), ...)
firstobs(x, ...)
lastobs(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector}
  \item{formula}{A formula (only the first term is used, see 'details').}
  \item{data}{A data frame}
  \item{\dots}{Currently not used}
}
\details{
  If writing ~a+b+c as formula, then only a is considered.
}
\value{
  A vector.
}
%%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

x <- c(rep(1,5),rep(2,3),rep(3,7),rep(1,4))

firstobs(x)
lastobs(x)

data(dietox)

firstobs(~Pig, data=dietox)
lastobs(~Pig, data=dietox)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

