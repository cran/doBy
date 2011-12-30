\name{renameCol}
\alias{renameCol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Rename columns in a matrix or a dataframe. 
}
\description{
  Rename columns in a matrix or a dataframe.
}
\usage{
renameCol(indata, src, tgt)
}
\arguments{
  \item{indata}{
    A dataframe or a matrix
}
  \item{src}{
    Source: Vector of names of columns in 'indata' to be renamed. Can
    also be a vector of column numbers.
  }
  \item{tgt}{
    Target: Vector with corresponding new names in the output.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A dataframe if 'indata' is a dataframe; a matrix in 'indata' is a matrix.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard, <sorenh at mail dot dk>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[gdata]{rename.vars}}  
}
\examples{

renameCol(CO2, 1:2, c("kk","ll"))
renameCol(CO2, c("Plant","Type"), c("kk","ll"))

# These fail - as they should:
# renameCol(CO2, c("Plant","Type","conc"), c("kk","ll"))
# renameCol(CO2, c("Plant","Type","Plant"), c("kk","ll"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utitlities}
