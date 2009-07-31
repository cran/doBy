\name{which.maxn}
\Rdversion{1.1}
\alias{which.maxn}
\alias{which.minn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Where are the n largest or n smallest elements in a numeric vector ?
}
\description{
    Determines the locations, i.e., indices of the n largest or n smallest
    elements of a numeric vector.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
which.maxn(x, n = 1)
which.minn(x, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    numeric vector
}
  \item{n}{
    integer >= 1
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector of length at most n with the indices of the n largest /
  smaller elements. NAs are discared and that can cause the vector to be
  smaller than n.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard, sorenh@agrsci.dk
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{which.max}},  \code{\link{which.min}}
}
\examples{
x <- c(1:4,0:5,11,NA,NA)
ii <- which.minn(x,5)

x <- c(1,rep(NA,10),2)
ii <- which.minn(x,5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
