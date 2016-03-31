\name{is_estimable}
\alias{is_estimable}

\title{
	Determines if contrasts are estimable.
}
\description{
	Determines if contrasts are estimable, that is, if the contrasts can
	be written as a linear function of the data.
}
\usage{
is_estimable(K, null.basis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{
		A matrix.
}
\item{null.basis}{
	A basis for a null space (can be found with \code{null_basis()}).
%%     ~~Describe \code{null.basis} here~~
}
}
\details{
 Consider the setting \eqn{E(Y)=Xb}. A linear function of \eqn{b}, say
 \eqn{l'b} is	estimable if and only if there exists an \eqn{r} such
 that \eqn{r'X=l'} or equivalently \eqn{l=X'r}. Hence \eqn{l} must be
 in the column space of \eqn{X'}, i.e. in the orthogonal complement of
 the null space of \eqn{X}. Hence, with a basis \eqn{B} for the null
	space, \code{is_estimable()} checks if each row \eqn{l} of the matrix
	\eqn{K} is perpendicular to each column basis vector in \eqn{B}. 
 %%  ~~ If necessary, more details than the description above ~~
}
\value{
	A logical vector.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  \url{http://web.mit.edu/18.06/www/Essays/newpaper_ver3.pdf}
}
\author{
 Søren Højsgaard, \email{sorenh@math.aau.dk}
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{null_basis}}
}
\examples{
## TO BE WRITTEN
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

