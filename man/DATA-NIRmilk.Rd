\name{NIRmilk}
\alias{NIRmilk}
\docType{data}
\non_function{}

\title{NIRmilk}
\usage{data(NIRmilk)}
\description{
  Near infra red light  (NIR) measurments are made at 152 wavelengths on 17 milk samples. 
  While milk runs through a glass tube, infra red light is sent through
  the tube and the amount of light passing though the tube is measured
  at different wavelengths.
  Each milk sample was additionally analysed for fat, lactose, protein
  and drymatter.
}
\format{
  This data frame contains 18 rows and 158 columns.
  The first column is the sample number.
  The columns Xwww contains the infra red light amount at wavelength www.
  The response variables are fat, protein, lactose and dm (drymatter).
}
\details{
  PCA regression
}
% \source{
%   SHD
% }

% \references{
% }

\examples{	
data(NIRmilk)
}
\keyword{datasets}
