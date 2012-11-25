\name{lmBy}
\alias{lmBy}
\alias{coef.lmBy}
\alias{fitted.lmBy}
\alias{residuals.lmBy}
\alias{print.lmBy}
\alias{getBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  List of lm objects with a common model
}
\description{
  The data is split into strata according to the levels of the grouping
  factors and individual lm fits are obtained for each stratum.
}
\usage{
lmBy(formula, data, id = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
    A linear model
    formula object of the form y ~ x1+...+xn | g1+...gm.
    In the formula object, y represents the response, x1,...,xn the
    covariates, and the grouping factors specifying the partitioning of
    the data according to which different lm fits should be performed. 
}
  \item{data}{
    A dataframe
    %%     ~~Describe \code{data} here~~
}
\item{id}{
  A formula describing variables from data which are to be available
  also in the output.
}
\item{\dots}{
  Additional arguments passed on to \code{lm()}.

}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list of lm fits.
  %%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

\note{
  This is a recent addition to the package; please report bugs.
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## To be written...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

