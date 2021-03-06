% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by-lapplyBy.R
\name{by-lapply}
\alias{by-lapply}
\alias{lapply_by}
\alias{lapplyBy}
\alias{sapply_by}
\alias{sapplyBy}
\title{Formula based version of lapply and sapply}
\usage{
lapply_by(data, formula, FUN, ...)

lapplyBy(formula, data = parent.frame(), FUN, ...)

sapply_by(data, formula, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)

sapplyBy(
  formula,
  data = parent.frame(),
  FUN,
  ...,
  simplify = TRUE,
  USE.NAMES = TRUE
)
}
\arguments{
\item{data}{A dataframe.}

\item{formula}{A formula describing how data should be split.}

\item{FUN}{A function to be applied to each element in the splitted
list, see 'Examples' below.}

\item{...}{optional arguments to FUN.}

\item{simplify}{Same as for `sapply`}

\item{USE.NAMES}{Same as for `sapply`}
}
\value{
A list.
}
\description{
This function is a wrapper for calling lapply on the
    list resulting from first calling splitBy.
}
\examples{
fun <- function(x) range(x$uptake)
lapplyBy(~Treatment + Type, data=CO2, FUN=fun)
sapplyBy(~Treatment + Type, data=CO2, FUN=fun)

# Same as
lapply(splitBy(~Treatment + Type, data=CO2), FUN=fun)
}
\seealso{
\code{\link{splitBy}}, \code{\link{split_by}}
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{utilities}
