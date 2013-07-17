\name{scaleBy}
\alias{scaleBy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Groupwise scaling and centering of numeric columns in a dataframe
}
\description{
  Groupwise scaling and centering of numeric columns in a
  dataframe. Obtained by first splitting a dataframe and then calling
  scale on each stratum. 
}
\usage{
scaleBy(formula, data, center = TRUE, scale = TRUE, details=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
    Either a two-sided formula or a list. A dot (.) is allowed on both left and right hand side of
    formula. See 'details' for the meaning of this. 
  }
  \item{data}{
    A dataframe
  }
  \item{center}{
    If TRUE then data is centered to have mean zero
  }
  \item{scale}{
    If TRUE then data is scaled to have variance one
  }
  \item{details}{
    If larger than zero then information about grouping etc. is printed.
  }
}
\details{
  A typical formula is \code{y1+y2~f1+f2} where y1 and y2 are numeric
  variables and f1 and f2 can be of any type. For each cross-combination
  of the values of f1 and f2, the variables (y1,y2) are centered/scaled.
  
  It is valid to write \code{.~f1+f2}. In this case the variables to be
  centered/scaled are taken to be all numeric variables in the dataframe
  except those that a listed on the right hand side of the formula.

  It is valid to write \code{y1+y2~.}. In this case the stratification
  is taken to be by all non-numeric variables. If there are no
  non-numeric variables, then no stratification is made and a 'global'
  centering/scaling is made.

  It is valid to write \code{.~.}. In this case the variables to be
  centered/scaled are taken to be all numeric variables in the
  dataframe. The stratification is made by all non-numeric variables. If
  there are no non-numeric variables, then no stratification is made and a 'global'
  centering/scaling is made.
  
}
\value{
  A dataframe with the same columns as the input dataframe, but the
  scaled / centered values are put into the relevant columns. 
}
%%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

% \note{
%   The workhorse is the \code{funBy} function which, alas, is not yet
%   documented. 
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %\code{\link{ave}},
  \code{\link{lapplyBy}},
  \code{\link{orderBy}},
  %\code{\link{scaleBy}},
  \code{\link{splitBy}},
  \code{\link{summaryBy}},
  \code{\link{transformBy}},
}
\examples{

## The following forms are equivalent:
scaleBy(conc+rate ~ state, data=Puromycin)
scaleBy(list(c("conc","rate"), "state"), data=Puromycin)
scaleBy(list(c("."), "."), data=Puromycin)
scaleBy(.~., data=Puromycin)

## The same results can be obtained from
lapply(splitBy(~state, data=Puromycin),
	function(.dd) scale(.dd[,sapply(Puromycin,class)=="numeric"]))


## The pig growth data 'dietox'
data(dietox)

# "Remove the effect of time" by centering data within each time point.
dietox2 <- scaleBy(Weight~Time, data=dietox, scale=FALSE)

\dontrun{
library(lattice)
xyplot(Weight~Time|Evit+Cu, groups=Pig, data=dietox)
xyplot(Weight~Time|Evit+Cu, groups=Pig, data=dietox2)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

