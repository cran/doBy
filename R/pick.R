#' @title Extract (pick) elements without using brackets
#'
#' @description Extract (pick) elements without using brackets so that elements can be picked out as part of a pipe workflow.
#'
#' @details
#' 
#' These two helper functions extract elements from lists, data frames, or vectors.
#' They are simple wrappers for the standard bracket operators in R:
#'
#' - `pick1()` uses single brackets (`[`) and returns a **subset**.
#' - `pick2()` uses double brackets (`[[`) and returns the **element itself**.
#'
#' These are safer and more flexible than `$`, especially when used with the base R pipe (`|>`)
#' or in functional programming.
#'
#' @param x A list, data frame, or vector.
#' @param which The index or name of the element(s) to extract.
#'
#' @return
#' - `pick1()` returns a subset of `x`.
#' - `pick2()` returns a single element from `x`.
#'
#' @examples
#' lst <- list(a = 1:3, b = 4:6)
#'
#' # Without pipe
#' pick1(lst, "a")      # List with one element
#' pick2(lst, "a")      # Just the vector 1:3
#'
#' # With base R pipe
#' lst |> pick1("a")
#' lst |> pick2("a")
#'
#' df <- data.frame(x = 1:5, y = letters[1:5])
#'
#' df |> pick1("y")     # Returns a data frame with column 'y'
#' df |> pick2("y")     # Returns column 'y' as a character vector
#'
#' @name pick_elements
#' @aliases pick1 pick2
#' @export
pick1 <- function(x, which) {
  x[which]
}

#' @rdname pick_elements
#' @export
pick2 <- function(x, which) {
  x[[which]]
}
