\name{timeSinceEvent}
\alias{timeSinceEvent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate "time since event" in a vector.
}
\description{
  Events are coded as 1 in numeric vector (and non-events are coded
  with values  different from 1). \code{timeSinceEvent} will give the time since
  event (with and without sign). In a logical vector, events are coded
  as TRUE and all non-events as FALSE.
}
\usage{
timeSinceEvent(yvar, tvar = seq_along(yvar))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yvar}{
    A numerical or logical vector specifying the events
}
  \item{tvar}{
    An optional vector specifying time
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A dataframe with columns 'yvar', 'tvar', 'abs.tse' (absolute time
  since nearest event), 'sign.tse' (signed time since nearest event) and
  'run' (indicator of the time window around each event).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard <sorenh at agrsci dot dk>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{subSeq}},   \code{\link{rle}}
}
\examples{
## Events:
yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0)

## Plot results:
tse<- timeSinceEvent(yvar)
plot(sign.tse~tvar, data=tse, type="b")
grid()
rug(tse$tvar[tse$yvar==1], col=4,lwd=4)
points(scale(tse$run), col=tse$run,lwd=2)
lines(abs.tse+.2~tvar, data=tse, type="b",col=3)

## Find times for which time since an event is at most 1:
tse$tvar[tse$abs<=1]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

