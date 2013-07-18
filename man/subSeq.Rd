\name{subSeq}
\alias{subSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Find sub-sequences of identical elements in a vector.
}
\description{
  Find sub-sequences of identical elements in a vector.
}
\usage{
subSeq(x, item = NULL)
}
\arguments{
  \item{x}{
    An atomic vector.
  }
  \item{item}{
    Optionally a specific value to look for in 'x'.
}
}
%\details{
%  
%}
\value{
  A dataframe.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{
%  Comparisons (used when specifying 'item') are based on character
%  comparisons. 
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{rle}}
}
\examples{
x <- c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3)
(ans <- subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item=1)
subSeq(x,item="1")

x <- as.character(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item="1")
subSeq(x,item=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
