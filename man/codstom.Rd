\name{codstom}
\alias{codstom}
\docType{data}
\title{Diet of Atlantic cod in the Gulf of St. Lawrence (Canada)}
\description{
  Stomach content data for Atlantic cod (Gadus morhua) in the Gulf of
  St.Lawrence, Eastern Canada.
  Note: many prey items were of no interest for this analysis and were regrouped into the "Other" category.
}
\usage{data(codstom)}
\format{
  A data frame with 10000 observations on the following 10 variables.
  \describe{
    \item{\code{region}}{a factor with levels \code{SGSL} \code{NGSL}}
    \item{\code{ship.type}}{a factor with levels \code{2} \code{3} \code{31} \code{34} \code{90} \code{99}}
    \item{\code{ship.id}}{a factor with levels \code{11558} \code{11712}
      \code{136148} \code{136885}
      \code{136902} \code{137325} \code{151225} \code{151935} \code{99433}}
    \item{\code{trip}}{a factor with levels \code{10} \code{11}
      \code{12} \code{179} \code{1999}
      \code{2} \code{2001} \code{20020808} \code{3} \code{4} \code{5}
      \code{6} \code{7} \code{8}
      \code{88} \code{9} \code{95}}     
    \item{\code{set}}{a numeric vector}
    \item{\code{fish.id}}{a numeric vector}
    \item{\code{fish.length}}{a numeric vector}
    \item{\code{prey.mass}}{a numeric vector}
    \item{\code{prey.type}}{a factor with levels \code{Ammodytes_sp}
      \code{Argis_dent}
      \code{Chion_opil} \code{Detritus} \code{Empty} \code{Eualus_fab}
      \code{Eualus_mac} \code{Gadus_mor} \code{Hyas_aran}
      \code{Hyas_coar}
      \code{Lebbeus_gro} \code{Lebbeus_pol} \code{Leptocl_mac}
      \code{Mallot_vil}
      \code{Megan_norv} \code{Ophiuroidea} \code{Other} \code{Paguridae}
      \code{Pandal_bor} \code{Pandal_mon} \code{Pasiph_mult}
      \code{Sabin_sept}
      \code{Sebastes_sp} \code{Them_abys} \code{Them_comp} \code{Them_lib}}    
  }
}
\details{
  For ship.type of 90 and 99, ship.id is also required to obtain a
  unique ship identification.
  Origin, ship.type, ship.id, trip, set and fish.id are all required to discriminate individual stomachs. 
}
\source{
  Small subset from a larger dataset collected by D. Chabot and
  M. Hanson,
  Fisheries & Oceans Canada (chabotd@dfo-mpo.gc.ca).
}
\references{
}
\examples{
data(codstom)
str(codstom)
summaryBy(prey.mass ~ region+ship.type+ship.id+trip+set+fish.id,
data = codstom, id = ~fish.length, FUN = sum)  # removes multiple occurences of prey.type in stomachs
summaryBy(prey.mass ~ region+ship.type+ship.id+trip,
data = codstom, FUN = mean)  # mean prey mass per stomach for each trip
}
\keyword{datasets}
