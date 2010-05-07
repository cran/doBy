\name{recodevar}
\alias{recodevar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{recode levels of variable}
\description{
 Recodes a variable with levels, say '1','2' to a variable with levels,
 say 'a', 'b'
}
\usage{
recodevar(var, src, tgt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{The variable to be recoded}
  \item{src}{The source levels: the present levels of var}
  \item{tgt}{The target levels: the new levels of var}
}
\value{
  A new variable
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
% \note{ ~~further notes~~ }

%  ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
 x <- c("dec","jan","feb","mar","apr","may")
 src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
 tgt1 <- list("winter","spring")
 recodevar(x,src=src1,tgt=tgt1)
}
\keyword{utilities}% at least one, from doc/KEYWORDS

