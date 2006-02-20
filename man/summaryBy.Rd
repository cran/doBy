\name{summaryBy}
\alias{summaryBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Function to calculate groupwise summary statistics}
\description{
Function to calculate groupwise summary statistics, much like
  the summary procedure of SAS
}
\usage{
summaryBy(formula, data = parent.frame(), id = NULL, FUN = mean,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{formula}{A formula object, see examples below}
  \item{data}{A data frame}
  %\item{subset}{A specification of a subset of data to be used}
  \item{id}{A formula specifying variables which data are not grouped by
  but which should appear in the output. See examples below.}
  \item{FUN}{A list of functions to be applied, see examples below.}
  \item{...}{Additional arguments to FUN.}
}
\details{
  NAs are removed before calculating summary statistics
}

\value{
A data frame
}
\author{Søren Højsgaard, sorenh@agrsci.dk}

\seealso{
  \code{\link{orderBy}},
  %\code{\link{qqnormBy}},
  %\code{\link{summaryBy}},
  \code{\link{splitBy}},
  \code{\link{doby.xtabs}},
  }
}

\examples{
data(dietox)
dietox12    <- subset(dietox,dietox$Time==12)

%myvar <- function(x) var(x,na.rm=TRUE)
summaryBy(Weight+Feed~Evit+Cu,      data=dietox12, FUN=c(mean,var), na.rm=TRUE)  

summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
FUN=c(mean,var), na.rm=TRUE)  

summaryBy(Weight+Feed~Evit+Cu,      data=dietox12, FUN=length)  

summaryBy(log(Weight)+Feed~Evit+Cu,      data=dietox12)  
summaryBy(.~Evit+Cu,      data=dietox12, id=~Litter, FUN=mean)

data(warpbreaks)
summaryBy(breaks ~ wool+tension, warpbreaks)
summaryBy(breaks ~., warpbreaks)
summaryBy(.~ wool+tension, warpbreaks)

summaryBy(Ozone~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE)


}
\keyword{univar}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

