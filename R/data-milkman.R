#' Milk yield data for manually milked cows.
#' 
#' Milk yield data for cows milked manually twice a day (morning and evening).
#' 
#' There are data for 222 cows. Some cows appear more than once in the dataset
#' (in different lactations) and there are 288 different lactations.
#' 
#' @name data-milkman
#' @docType data
#'
#' @format
#' A data frame with 161836 observations on the following 12 variables.
#' \describe{
#'   \item{\code{cowno}}{a numeric vector; cow identification}
#'   \item{\code{lactno}}{a numeric vector; lactation number}
#'   \item{\code{ampm}}{a numeric vector; milking time: 1: morning; 2: evening}
#'   \item{\code{dfc}}{a numeric vector; days from calving}
#'   \item{\code{my}}{a numeric vector; milk yield (kg)}
#'   \item{\code{fatpct}}{a numeric vector; fat percentage}
#'   \item{\code{protpct}}{a numeric vector; protein percentage}
#'   \item{\code{lactpct}}{a numeric vector; lactose percentage}
#'   \item{\code{scc}}{a numeric vector; somatic cell counts}
#'   \item{\code{race}}{a factor with levels \code{RDM} \code{Holstein} \code{Jersey}}
#'   \item{\code{ecmy}}{a numeric vector; energy corrected milk}
#'   \item{\code{cowlact}}{Combination of cowno and lactno; necessary
#'     because the same cow may appear more than once in the dataset (in
#'     different lactations)}
#' }
#'
#' @keywords datasets
#'
#' @references Friggens, N. C.; Ridder, C. and Løvendahl, P. (2007). 
#' On the Use of Milk Composition Measures to Predict the Energy Balance of Dairy Cows.
#' J. Dairy Sci. 90:5453–5467 doi:10.3168/jds.2006-821.
#'
#' This study was part of the Biosens project used data from the
#' “Mælkekoens energibalance og mobilisering” project; both were
#' funded by the Danish Ministry of Food, Agriculture and Fisheries
#' and the Danish Cattle Association.
#' 
#' @examples
#' 
#' data(milkman)
#' 
"milkman"

