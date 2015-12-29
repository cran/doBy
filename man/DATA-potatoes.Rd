\name{potatoes}
\alias{potatoes}
\docType{data}
\title{
	Weight and size of 20 potatoes
}
\description{
	Weight and size of 20 potatoes. Weight in grams; size in
	milimeter. There are two sizes: \code{length} is the longest length
	and \code{width} is the shortest length across a potato.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("potatoes")}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{weight}}{a numeric vector}
    \item{\code{length}}{a numeric vector}
    \item{\code{width}}{a numeric vector}
  }
}
%% \details{
%% %%  ~~ If necessary, more details than the __description__ above ~~
%% }
\source{
	My own garden; autumn 2015.
}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

%% \references{
%% %%  ~~ possibly secondary sources and usages ~~
%% }
\examples{
data(potatoes)
## maybe str(potatoes) ; plot(potatoes) ...
}
\keyword{datasets}
