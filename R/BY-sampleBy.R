#' @title Sampling from a data frame
#'
#' @description   A data frame is split according to some variables in a formula, and a
#'  sample of a certain fraction of each is drawn.
#'
#' @name by-sample
#'
#' @details If systematic=FALSE (default) then frac gives the fraction
#'     of data sampled. If systematic=TRUE and frac=.2 then every 1/.2
#'     i.e. every 5th observation is taken out.
#'
#' @param formula A formula defining the grouping of the data frame.
#' @param frac The part of data to be sampled.
#' @param replace Is the sampling with replacement.
#' @param data A data frame.
#' @param systematic Should sampling be systematic.
#'
#' @return A dataframe.
#' @seealso \code{\link{summaryBy}}, \code{\link{orderBy}},
#'     \code{\link{splitBy}}, \code{\link{transformBy}}
#'
#' @keywords utilities
#' 
#' @examples
#' data(dietox)
#' sampleBy(formula = ~Evit+Cu, frac=.1, data = dietox)
#' 


sampleBy <- function(formula, frac=0.1, replace=FALSE, data=parent.frame(),
                     systematic=FALSE
                     ){
  ddd<-splitBy(formula, data=data)

  ddd<- lapply(ddd, function(dat){
    if (systematic==TRUE){
      idx <- seq(1,nrow(dat),1/frac)
    } else {  
      idx <- sort(sample(1:nrow(dat), size=round(frac*nrow(dat)), replace=replace))
    }
    dat[idx,]
  }) 
  ddd <-do.call("rbind",ddd)
  return(ddd)
}

