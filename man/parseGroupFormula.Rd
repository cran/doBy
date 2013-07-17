\name{parseGroupFormula}
\alias{parseGroupFormula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract components from a formula with "conditioning bar"
}
\description{
  Extract components from a formula with the form \code{y~x1+...xn|g1+...gm}
}
\usage{
parseGroupFormula(form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{A formula of the form \code{y~x1+...xn|g1+...gm}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  If the formula is \code{y~x1+x2|g1+g2} the result is
  \item{model}{\code{y~x1+x2}}
  \item{groups}{\code{g1+g2}}
  \item{groupFormula}{\code{~g1+g2}}
}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

\note{
  This function is a recent addition and has not been thoroughly
  tested. Please report bugs.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% \code{\link{createFunBy}}
% }
\examples{
gf<-parseGroupFormula(y~x1+x2|g1+g2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentatn directory.
\keyword{utilities}
