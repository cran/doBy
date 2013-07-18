\name{descStat}
\alias{descStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Computing simple descriptive statistics of a numeric vector. 
}
\description{
    Computing simple descriptive statistics of a numeric vector -- not
    unlike what proc means of SAS does
}
\usage{
descStat(x, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric vector
}
  \item{na.rm}{
    Should missing values be removed
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector with named elements.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Gregor Gorjanc; gregor.gorjanc <at> bf.uni-lj.si
  %%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{summaryBy}}
}
\examples{
x <- c(1,2,3,4,NA,NaN)
descStat(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
