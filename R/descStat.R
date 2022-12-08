###############################################################################
#' @title Computing simple descriptive statistics of a numeric vector.
#' @description Computing simple descriptive statistics of a numeric
#'     vector - not unlike what proc means of SAS does
###############################################################################
#' @param x A numeric vector
#' @param na.rm Should missing values be removed
#' @return A vector with named elements.
#' @author Gregor Gorjanc; \email{gregor.gorjanc@@bf.uni-lj.si}
#' @seealso \code{\link{summaryBy}}, \code{\link{summary_by}}
#' @keywords utilities
#' @examples
#' 
#' x <- c(1, 2, 3, 4, NA, NaN)
#' descStat(x)
#' 
#' @export descStat
descStat <- function (x, na.rm = TRUE)
{
  if(!is.numeric(x))
    stop("'x' must be numeric")
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  c(n = length(x),
    obs = sum(!is.na(x)),
    mean = m,
    median = median(x, na.rm = na.rm),
    sd = s,
    cv = s/m,
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm))
}

