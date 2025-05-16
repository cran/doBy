#' Bind list of data frames and add list names as a column
#'
#' @description
#' Binds a named list of data frames (or tibbles) into a single data frame.
#' Adds the list name as a new column (first column).
#'
#' @param lst A named list of data frames or tibbles.
#' @param name A character scalar: name of the column to hold the list names (default "name").
#'
#' @return A data frame or tibble, depending on the class of the input.
#'
#' @examples
#' lst <- list(a = data.frame(x = 1:2), b = data.frame(x = 3:4))
#' rbind_list(lst)
#'
#' lst <- split(iris, iris$Species)
#' rbind_list(lst)
#' 
#'
#' @export
rbind_list <- function(lst, name = "name") {
  stopifnot(
    is.list(lst),
    !is.null(names(lst)),
    all(nzchar(names(lst))),
    is.character(name),
    length(name) == 1
  )

  use_tibble <- requireNamespace("tibble", quietly = TRUE) &&
                any(vapply(lst, inherits, logical(1), "tbl_df"))

  out <- Map(function(df, nm) {
    df <- as.data.frame(df)  # ensures even tibbles behave nicely
    df[[name]] <- nm
    df[, c(name, setdiff(names(df), name)), drop = FALSE]
  }, lst, names(lst))

  result <- do.call(rbind, out)
  row.names(result) <- NULL

  if (use_tibble) {
    result <- tibble::as_tibble(result)
  }

  result
}

## Navn | Fordel | Ulempe
## rbind_list | Beskrivende, enkel, matcher base R-logik | En smule generisk
## stack_list | Mere "verbalt" og beskriver handlingen | Kan forveksles med stack()
## combine_list | Letforståeligt, signalerer sammensmeltning | Ikke specifikt om rbind
## bind_rows_list | Tydeligt og i stil med dplyr::bind_rows() | Længere navn
## join_list | Knap så præcist – lyder som en slags merge | Forvirrende i statistisk kontekst
## collapse_list | Tydeligt at det handler om at lave én ud af mange | Kan tolkes som vektor-collapse
