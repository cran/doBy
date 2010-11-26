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
          keep.names=FALSE, p2d=FALSE, order=TRUE, full.dimension=FALSE, ...)
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
%\item{postfix}{An optional vector of postfixes for the names of the output
%  variables, see 'examples'.}
\item{p2d}{Should parentheses in output variable names be replaced by dots?}
\item{order}{Should the resulting dataframe be ordered according to the
  variables on the right hand side of the formula? (using \link{orderBy}}
\item{full.dimension}{If TRUE then rows of summary statistics are
  repeated such that the result will have the same number
  of rows as the input dataset.}
\item{...}{Additional arguments to FUN. This could for example be NA actions.}
}
\details{
  Extra arguments ('...') are passed onto the functions in FUN. Hence
  care must be taken that all functions in FUN accept these arguments -
  OR one can explicitly write a functions which get around this.  This
  can particularly be an issue in connection with handling NAs. See
  examples below.

  Some code for this function has been suggested by Jim Robison-Cox. 
}

\value{
A data frame
}
\author{Søren Højsgaard, sorenh@agrsci.dk}

\seealso{
  \code{\link{orderBy}},
  \code{\link{ave}},
  \code{\link{transformBy}},
  \code{\link{splitBy}},
  \code{\link{lapplyBy}},
}


\examples{

data(dietox)
dietox12    <- subset(dietox,Time==12)

summaryBy(Weight+Feed~Evit+Cu,      data=dietox12,
   FUN=c(mean,var,length))  

summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
   FUN=c(mean,var,length))  

## Calculations on transformed data:

summaryBy(log(Weight)+Feed~Evit+Cu, data=dietox12)  

## Calculations on all numerical variables (not mentioned elsewhere): 

summaryBy(.~Evit+Cu,                data=dietox12,
   id=~Litter, FUN=mean)

## There are missing values in the 'airquality' data, so we remove these
## before calculating mean and variance with 'na.rm=TRUE'. However the
## length function does not accept any such argument. Hence we get
## around this by defining our own summary function in which length is
## not supplied with this argument while mean and var are:

sumfun <- function(x, ...){
  c(m=mean(x, ...), v=var(x, ...), l=length(x))
}
summaryBy(Ozone+Solar.R~Month, data=airquality, FUN=sumfun, na.rm=TRUE)

## Using '.' on the right hand side of a formula means to stratify by
## all variables not used elsewhere:

data(warpbreaks)
summaryBy(breaks ~ wool+tension, warpbreaks)
summaryBy(breaks ~., warpbreaks)
summaryBy(.~ wool+tension, warpbreaks)

## Keep the names of the variables (works only if FUN only returns one
## value):

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
  keep.names=TRUE)

## Using full.dimension=TRUE

## Consider:
summaryBy(breaks~wool, data=warpbreaks)
## Rows of result are replicated below
summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE)
## Notice: Previous result is effectively the same as
with(warpbreaks, ave(breaks, wool))
## A possible application of full.dimension=TRUE is if we want to 
## standardize (center and scale) data within groups:
ss <- summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE, FUN=c(mean,sd))
(warpbreaks$breaks-ss$breaks.mean)/ss$breaks.sd

}
\keyword{univar}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
