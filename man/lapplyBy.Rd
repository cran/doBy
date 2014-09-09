\name{lapplyBy}
\alias{lapplyBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formula based version of lapply}
\description{
  This function is a wrapper for calling lapply on the list resulting
  from first calling splitBy.
}
\usage{
lapplyBy(formula, data = parent.frame(), FUN, keep.groupid = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula describing how data should be split}
  \item{data}{A dataframe}
  \item{FUN}{A function to be applied to each element in the splitted
    list, see 'Examples' below.}
  \item{keep.groupid}{If TRUE, the grouping information is stored as a
    dataframe in an attribute called groupid. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list.
}
%\references{ ~put references to the literature/web site here ~ }

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%% \note{
%%   The \code{keep.groupid} argument is a recent addition, and i
%% }

\seealso{
  \code{\link{orderBy}},
  \code{\link{summaryBy}},
  \code{\link{transformBy}},
  \code{\link{splitBy}},
  %\code{\link{lapplyBy}},
}


\examples{
data(dietox)

## Calculate weekwise feed efficiency = weight gain / feed intake
dietox <- orderBy(~Pig+Time, data=dietox)
v<-lapplyBy(~Pig, data=dietox, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)

## Technically this is the same as
dietox <- orderBy(~Pig+Time, data=dietox)
wdata <- splitBy(~Pig, data=dietox)
v <- lapply(wdata, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

