###############################################################################
#' @title Locate the index of the first/last unique value
#' @description Locate the index of the first/last unique value in i) a vector
#'     or of a variable in a data frame.
#' @name firstlastobs
###############################################################################
#' @details If writing ~a + b + c as formula, then only a is considered.
#' 
#' @aliases firstobs lastobs firstobs.default lastobs.default firstobs.formula
#'     lastobs.formula
#' @param x A vector
#' @param formula A formula (only the first term is used, see 'details').
#' @param data A data frame
#' @param ... Currently not used
#' @return A vector.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' x <- c(rep(1, 5), rep(2, 3), rep(3, 7), rep(1, 4))
#' firstobs(x)
#' lastobs(x)
#' data(dietox)
#' firstobs(~Pig, data=dietox)
#' lastobs(~Pig, data=dietox)
#' 

#' @export
#' @rdname firstlastobs
lastobs <- function(x, ...)  UseMethod("lastobs")

#' @export
#' @rdname firstlastobs
firstobs <- function(x, ...) UseMethod("firstobs")

#' @export
#' @rdname firstlastobs
lastobs.default <- function(x, ...){
  ux <- unique(x)
  m <-match(ux, x)
  sort(sapply(ux, function(i) {max(which(i==x))}))
}

#' @export
#' @rdname firstlastobs
firstobs.default <- function(x, ...){
  ux <- unique(x)
  m <- match(ux,x)
  sort(sapply(ux, function(i) {min(which(i==x))}))
}

#' @export
#' @rdname firstlastobs
lastobs.formula <- function(formula, data=parent.frame(), ...){
  rhs <- gsub(" +","",strsplit(paste(formula[2]),"\\+")[[1]][1])
  lastobs(data[,rhs])
}

#' @export
#' @rdname firstlastobs
firstobs.formula <- function(formula, data=parent.frame(), ...){
   rhs <- gsub(" +","",strsplit(paste(formula[2]),"\\+")[[1]][1])
   firstobs(data[,rhs])
}
