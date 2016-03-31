\name{null_basis}
\alias{null_basis}

\title{
	Finds the basis of the (right) null space.
}
\description{
	Finds the basis of the (right) null space of a matrix, a vector (a
	1-column matrix) or a model object for which a model matrix can be
	extracted. 
}
\usage{
null_basis(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A matrix, a vector (a
	1-column matrix) or a model object for which a model matrix can be
	extracted (using \code{model.matrix}).
}
}
\details{
	Finds basis for the (right) null space {x : Mx = 0}.
}
\value{
	A matrix (possibly with zero columns if the null space consists only
	of the zero vector).
}
%% \references{
%% ~put references to the literature/web site here ~
%% }
\author{
  Søren Højsgaard, \email{sorenh@math.aau.dk}
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link[MASS]{Null}}
}
\examples{
M <- matrix(c(1,1,1,1,1,1,0,0,0,0,1,1), nrow=4)
null_basis(M)
MASS::Null(t(M))

M <- c(1,1,1,1)
null_basis(M)
MASS::Null(t(M))

m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
null_basis(m0)
MASS::Null(t(model.matrix(m0)))

## Make balanced dataset
dat.bal   <- expand.grid(list(A=factor(1:2), B=factor(1:3), C=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset: 'B' is nested within 'C' so B=1 is only
## found when C=1 and B=2,3 are found in each C=2,3,4
dat.nst <- dat.bal
dat.nst$C <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
xtabs(y ~ C+B+A , data=dat.nst)

mod.bal  <- lm(y ~ A + B*C, data=dat.bal)
mod.nst  <- lm(y ~ A + B*C, data=dat.nst)

null_basis( mod.bal )
null_basis( mod.nst )

null_basis( model.matrix(mod.bal) )
null_basis( model.matrix(mod.nst) )

MASS::Null( t(model.matrix(mod.bal)) )
MASS::Null( t(model.matrix(mod.nst)) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
