\name{HTMLreport}
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
HTMLreport(srcfile, driver = RweaveHTMLreport(), destdir=".",
postfix="REPORT", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srcfile}{
    A text file with R code and descriptive text. The file name
    extension can be anything except .html as this is the name of the
    output file
%%     ~~Describe \code{srcfile} here~~
}
  \item{driver}{
    The actual workhorse
    %%     ~~Describe \code{driver} here~~
  }
  
  \item{destdir}{
    Specification of where to put the report.
    %%     ~~Describe \code{driver} here~~
  }
  
  \item{postfix}{
    A string to append to the filename for the HTML file.
    %%     ~~Describe \code{driver} here~~
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
HTMLreport(tf)
## Creates report in specified directory (which must exist).
##HTMLreport(tf, path=".REPORT/") 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
