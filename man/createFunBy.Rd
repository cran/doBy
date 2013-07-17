\name{createFunBy}
%\alias{createFunBy}
\alias{formulaFunBy}
\alias{xyFunBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A template function for creating groupwise functions 
}
\description{
  A template function for creating groupwise functions
}
\usage{
%createFunBy(formula, data, FUN, class = NULL, ...)
formulaFunBy(formula, group, data, FUN, class = NULL, ...)
xyFunBy(xy, group, data, FUN, class = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula of the form \code{y ~ x}  (which must be
    variable names in \code{data}).
  }
  \item{xy}{A character vector with one or two elements (which must be
    variable names in \code{data}).
  }
  \item{group}{A right hand sided formula or a character vector defining
  the grouping of data}
\item{data}{A data frame
  }
  \item{FUN}{The function to be applied
}
  \item{class}{The class to give the result of the returned value of the
    created function.
}
  \item{\dots}{Further arguments passed on to \code{FUN}
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


t.testBy1 <- function(formula, group, data, ...){
  formulaFunBy(formula, group, data, FUN=t.test, class="t.testBy1", ...)
}

t.testBy2 <- function(formula, group, data, ...){
  xyFunBy(formula, group, data, FUN=t.test, class="t.testBy1", ...)
}


t.testBy1(y~g1, ~g2+g3, data=mydata)
t.testBy2(y~x, ~g2+g3, data=mydata)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

