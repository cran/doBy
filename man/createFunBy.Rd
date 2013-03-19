\name{createFunBy}
\alias{createFunBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A template function for creating groupwise functions 
}
\description{
  A template function for creating groupwise functions
}
\usage{
createFunBy(formula, data, FUN, class = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula containing a vertical bar (which is used for
    defining groups)
%%     ~~Describe \code{formula} here~~
}
  \item{data}{A data frame
%%     ~~Describe \code{data} here~~
}
  \item{FUN}{The function to be applied
%%     ~~Describe \code{FUN} here~~
}
  \item{class}{The class to give the result of the returned value of the
    created function.
%%     ~~Describe \code{class} here~~
}
  \item{\dots}{Further arguments passed on to \code{FUN}
%%     ~~Describe \code{\dots} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A function
}
%\references{
%% ~put references to the literature/web site here ~
%}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

\note{
  This function is a recent addition and has not been thoroughly
  tested. Please report bugs.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{parseGroupFormula}}
}
\examples{

## Example: Create a function for creating groupwise t-tests

mydata <- data.frame(y=rnorm(32), x=rnorm(32),
g1=factor(rep(c(1,2),each=16)), g2=factor(rep(c(1,2), each=8)),
g3=factor(rep(c(1,2),each=4))) 


t.testBy <- function(formula, data, ...){
  createFunBy(formula, data, FUN=t.test, class="t.testBy", ...)
}

t.testBy(y~g1|g2+g3, data=mydata)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

