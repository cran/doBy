\name{recodeVar}
\alias{recodeVar}
\alias{recodevar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recode values of a vector}
\description{
 Recodes a vector with values, say 1,2 to a variable with values,
 say 'a', 'b'
}
\usage{
recodeVar(x, src, tgt, default=NULL, keep.na=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector; the variable to be recoded}
  \item{src}{The source values: a subset of the present values of x}
  \item{tgt}{The target values: the corresponding new values of x}
  \item{default}{Default target value for those values of x not listed
    in 'src'. When default=NULL, values of x which are not given in
    'src' will be kept in the output.}
  \item{keep.na}{If TRUE then NA's in x will be retained in the output}
}
\value{
  A vector
}
% \references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, <sorenh at agrsci dot dk>}
% \note{ ~~further notes~~ }

\section{Warning }{Care should be taken if x is a factor. A safe
  approach may be to convert x to a character vector using as.character.} 
%  ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\seealso{
  \code{\link[base]{cut}},
  \code{\link[base]{factor}},
  \code{\link[car]{recode}}
}
\examples{
x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)
#[1] "winter" "winter" "winter" "spring" "spring" "spring"

x <- c(rep(1:3,3))
#[1] 1 2 3 1 2 3 1 2 3

## Simple usage:
recodeVar(x, src=c(1,2), tgt=c("A","B"))
#[1] "A" "B" NA  "A" "B" NA  "A" "B" NA 

## Here we need to use lists
recodeVar(x, src=list(c(1,2)), tgt=list("A"))
#[1] "A" "A" NA  "A" "A" NA  "A" "A" NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L")
#[1] "A" "A" "L" "A" "A" "L" "A" "A" "L"
recodeVar(x, src=list(c(1,2),3), tgt=list("A","B"), default="L")
#[1] "A" "A" "B" "A" "A" "B" "A" "A" "B"

## Dealing with NA's in x
x<-c(NA,rep(1:3,3),NA)
#[1] NA  1  2  3  1  2  3  1  2  3 NA
recodeVar(x, src=list(c(1,2)), tgt=list("A"))
#[1] NA  "A" "A" NA  "A" "A" NA  "A" "A" NA  NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L")
#[1] NA  "A" "A" "L" "A" "A" "L" "A" "A" "L" NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L", keep.na=FALSE)
#[1] "L" "A" "A" "L" "A" "A" "L" "A" "A" "L" "L"


}
\keyword{utilities}% at least one, from doc/KEYWORDS

