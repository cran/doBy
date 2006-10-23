\name{summaryBy}
\alias{summaryBy}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Function to calculate groupwise summary statistics}
\description{
Function to calculate groupwise summary statistics, much like
  the summary procedure of SAS
}
\usage{
summaryBy(formula, data = parent.frame(), id = NULL, FUN = mean,
keep.names=FALSE, postfix=NULL, p2d=FALSE, order=TRUE, ...)

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{formula}{A formula object, see examples below}
  \item{data}{A data frame}
  %\item{subset}{A specification of a subset of data to be used}
  \item{id}{A formula specifying variables which data are not grouped by
  but which should appear in the output. See examples below.}
\item{FUN}{A list of functions to be applied, see examples below.}
\item{keep.names}{If TRUE and if there is only ONE function in FUN, then
the variables in the output will have the same name as the variables in
the input, see 'examples'.}
\item{postfix}{An optional vector of postfixes for the names of the output
  variables, see 'examples'.}
\item{p2d}{Should parentheses in output variable names be replaced by dots?}
\item{order}{Should the resulting dataframe be ordered according to the
  variables on the right hand side of the formula? (using \link{orderBy}}
\item{...}{Additional arguments to FUN. This could for example be NA actions.}
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
  %\code{\link{summaryBy}},
  \code{\link{transformBy}},
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

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
  keep.names=TRUE)

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean,var),na.rm=TRUE,
  postfix=c("m","v"))

summaryBy(Ozone+Wind~Month, data=airquality,
  FUN=c(function(x,...){c(mean(x,...),var(x,...))},function(x,...){median(x,...)}),
  postfix=list(c("m","v"),"med"), na.rm=TRUE)


summaryBy(conc ~ Plant, CO2)

do.call("summaryBy", list(.~ Treatment, CO2[3:5], FUN = function(x) mean(range(x)), postfix = "midrange"))


}
\keyword{univar}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

