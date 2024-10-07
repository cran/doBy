
#' Plot the response variable against the predictor variables.
#'
#' @param formula. A formula of the form y ~ x1 + x2 + ... + xn, where
#'     y is the response variable and x1, x2, ..., xn are the
#'     predictor variables. A dot as right hand side is allowed.
#' 
#' @param data. A data frame containing the variables in the formula.
#' @param geoms A list of ggplot2 geoms to be added to the plot.
#' @param global_aes A list of global aesthetics to be added to the plot.
#' @param plot A logical value indicating whether the plot should be displayed.
#' @param nrow,ncol Number of rows / columns in plot. 
#'
#' @return A list of ggplot2 plots.
#' @export
#'
#' @examples
#' library(ggplot2)
#' response_plot(iris, Sepal.Width ~ ., geoms=geom_point())
#' response_plot(iris, Sepal.Width ~ ., geoms=geom_point(), global_aes=list(color="Species"))
#' personality |> response_plot(easygon~., geoms=geom_point(), global_aes=NULL)
#' 
response_plot <- function(data., formula., geoms=NULL, global_aes=NULL, plot=TRUE, nrow=NULL, ncol=NULL) {  
    trms <- terms(formula., data=data.)
    ## trms
    yy <- as.character(formula.[[2]])
    xx <- setdiff(attr(trms, "term.labels"), yy)
    ##    list(x=xx, y=yy) |> str()

    aes_template <- lapply(xx, function(x_){
        c(list(x=x_, y=yy), global_aes) 
    })
    ## aes_template

    aes_list <- aes_template |> lapply(ggpubr_create_aes)
    ## aes_list
    
    plot_basic <- lapply(aes_list, function(z_){
        data.  |> ggplot(mapping = z_)    
    })
    ## plot_basic
    
    plot_list <-lapply(plot_basic, function(pl_) {
        pl_ + geoms
    } )
    if (plot){
        s <- cowplot::plot_grid(plotlist = plot_list, nrow=nrow, ncol=ncol)        
        print(s)
    }
    
    return(invisible(plot_list))
}


### Taken from ggpubr::create_aes to avoid circular dependencies

ggpubr_create_aes <- function (.list, parse = TRUE) 
{
  # if (missing(parse)) {
  #   parse <- base::getOption("ggpubr.parse_aes", default = TRUE)
  # }
#  if (parse) {
    return(create_aes.parse(.list))
#  }
#  else {
#    return(create_aes.name(.list))
#  }
}

create_aes.parse <- function (.list) 
{
#  .list <- .list %>% purrr::map(function(x) parse_expression(x))
  .list <- .list |> lapply(function(x) parse_expression(x))
  do.call(ggplot2::aes, .list)
}

parse_expression <- function (x) 
{
  if (is_parsable_aes(x)) {
    if (contains_space(x)) {
      if (!is_math_string(x)) 
        return(as.name(x))
    }
    x <- parse(text = x)[[1]]
  }
  x
}

is_numeric_char <- function (x) 
{
  if (is.character(x)) 
    res <- grepl("^[[:digit:]]+$", x)
  else res <- FALSE
  res
}

is_parsable_aes <- function (x) 
{
  is.character(x) & (!is_numeric_char(x)) & (length(x) == 1)
}

contains_space <- function (x) 
{
  grepl("\\s", x)
}

is_math_string <- function (x) 
{
  operators <- c("+", "-", "*", "^", "%%", "%/%", "/", "==", 
                 ">", "<", "!=", "<=", ">=")
  contains_math_operators <- unlist(lapply(operators, grepl, 
                                           x, fixed = TRUE))
  contains_parentheses <- grepl(pattern = "\\(.*\\)", x)
  any(c(contains_math_operators, contains_parentheses))
}




# parse_as_expression <- function (text) 
# {
#   stopifnot(is.character(text))
#   out <- vector("expression", length(text))
#   for (i in seq_along(text)) {
#     expr <- parse(text = text[[i]])
#     out[[i]] <- if (length(expr) == 0) 
#       NA
#     else expr[[1]]
#   }
#   out
# }






