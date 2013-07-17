\name{milkman}
\alias{milkman}
\docType{data}
\title{
  Milk yield data for manually milked cows.
}
\description{
  Milk yield data for cows milked manually twice a day (morning and evening).
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(milkman)}
\format{
  A data frame with 161836 observations on the following 12 variables.
  \describe{
    \item{\code{cowno}}{a numeric vector; cow identification}
    \item{\code{lactno}}{a numeric vector; lactation number}
    \item{\code{ampm}}{a numeric vector; milking time: 1: morning; 2: evening}
    \item{\code{dfc}}{a numeric vector; days from calving}
    \item{\code{my}}{a numeric vector; milk yield (kg)}
    \item{\code{fatpct}}{a numeric vector; fat percentage}
    \item{\code{protpct}}{a numeric vector; protein percentage}
    \item{\code{lactpct}}{a numeric vector; lactose percentage}
    \item{\code{scc}}{a numeric vector; somatic cell counts}
    \item{\code{race}}{a factor with levels \code{RDM} \code{Holstein} \code{Jersey}}
    \item{\code{ecmy}}{a numeric vector; energy corrected milk}
    \item{\code{cowlact}}{Combination of cowno and lactno; necessary
      because the same cow may appear more than once in the dataset (in
      different lactations)}
  }
}
\details{
 There are data for 222 cows. Some cows appear more than once in the
 dataset (in different lactations) and there are 288 different
 lactations. 
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(milkman)
## maybe str(milkman) ; plot(milkman) ...
}
\keyword{datasets}
