% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sub_seq.R
\name{sub_seq}
\alias{sub_seq}
\alias{subSeq}
\alias{is_grouped}
\alias{rle2}
\title{Find sub-sequences of identical elements in a vector.}
\usage{
subSeq(x, item = NULL)

sub_seq(x, item = NULL)

is_grouped(x)

rle2(x)
}
\arguments{
\item{x}{An atomic vector or a factor.}

\item{item}{Optionally a specific value to look for in \code{x}.}
}
\value{
A dataframe.
}
\description{
Find sub-sequences of identical elements in a vector.
}
\details{
\itemize{
\item \code{sub_seq} is synonymous with \code{subSeq}
\item \code{rle2} is identical to \code{rle} (from base) but \code{rle2} works on
factors as input (a factor is coerced to character).
\item \code{is_grouped} checks if the values in \code{x} are clustered into the
smallest number of clusters.
}
}
\examples{

x <- c(1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 3)
(ans <- subSeq(x))
ans$value
# Notice: Same results below
subSeq(x, item=1)
subSeq(x, item="1")

xc <- as.character(x)
(ans<-subSeq(xc))
ans$value
# Notice: Same results below
subSeq(xc, item="1")
subSeq(xc, item=1)

is_grouped(x)
is_grouped(sort(x))
is_grouped(xc)
is_grouped(sort(xc))
}
\seealso{
\code{\link{rle}}
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{utilities}
