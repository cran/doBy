\name{Rscript2HTML}
\alias{Rscript2HTML}
\alias{HTMLreport}
\alias{RweaveHTMLreport}
\alias{RweaveHTMLreportSetup}
\alias{RweaveHTMLreportWritedoc}
\alias{RweaveHTMLreportFinish}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Automatic Generation of Reports (as HTML documents)
%%  ~~function to do ... ~~
}
\description{
  HTMLreport provides a simple framework for mixing text and R code for
  automatic report generation.
  The basic idea is to replace the R code with its output, such that the
  final document only contains the text and the output of the
  statistical analysis.
}
\usage{
Rscript2HTML(srcfile, driver = RweaveHTMLreport(), destdir=".",
postfix="REPORT", cssfile=NULL, cleanup=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srcfile}{
    A text file with R code and descriptive text. The file name
    extension can be anything except .html as this is the name of the
    output file
  }
  \item{driver}{
    The actual workhorse
  }
  \item{destdir}{
    Specification of where to put the report.
  }
  \item{postfix}{
    A string to append to the filename for the HTML file.
  }
  \item{cssfile}{
    An optional cssfile; must be located in destdir to take effect.
  }
  \item{cleanup}{
    If set to \code{FALSE} then the temporary files are not removed.
  }
  \item{\dots}{
    Further arguments, currently not used.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  None
  %%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Søren Højsgaard <sorenh at agrsci dot dk>
}
\note{
  HTMLreport is a recent addition to the doBy package and it has not
  been thoroughly tested.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{Sweave}}
}
\examples{
tf <- system.file("HTMLreport", "PuromycinAnalysis-report.R",
   package = "doBy")

## Create report in working directory
Rscript2HTML(tf)
## Creates report in specified directory (which must exist).
##Rscript2HTML(tf, path=".REPORT/")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
