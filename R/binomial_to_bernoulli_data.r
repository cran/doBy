
#' @title Convert binomial data to bernoulli data
#'
#' @description Convert binomial data to bernoulli data by expanding dataset.
#'
#' @param data. A dataframe
#' @param y     Column with 'successes' in binomial distribution `y~bin(size, p)`
#' @param size  Column with 'failures', i.e. size-y or 'total', i.e. size. 
#' @param type  Whether `size` is rest (i.e. 'failures') or 'total'
#' @param response_name Name of response variable in output dataset.
#' @param rest_name Name of 'failures' in column `response_name`. 
#'
#' @examples
#'
#' dat <- budworm
#' dat <- dat[dat$dose %in% c(1,2), ]
#' dat$ntotal <- 5
#' dat
#' dat.a <- dat |>
#'   binomial_to_bernoulli_data(ndead, ntotal, type="total")
#' dat.b <- dat |>
#'   dplyr::mutate(nalive=ntotal-ndead) |> dplyr::select(-ntotal) |>
#'   binomial_to_bernoulli_data(ndead, nalive, type="rest")
#'
#' m0 <- glm(cbind(ndead, ntotal-ndead) ~ dose + sex, data=dat, family=binomial())
#' m1 <- glm(ndead / ntotal ~ dose + sex, data=dat, weight=ntotal, family=binomial())
#' ma <- glm(response ~ dose + sex, data=dat.a, family=binomial())
#' mb <- glm(response ~ dose + sex, data=dat.b, family=binomial())
#'
#' dat.a$response
#' dat.b$response ## Not same and therefore the following do not match
#'
#' all.equal(coef(m0), coef(ma))
#' all.equal(coef(m0), coef(mb))
#' all.equal(coef(m1), coef(ma))
#' all.equal(coef(m1), coef(mb))
#' @export

binomial_to_bernoulli_data <- function(data., y, size, type=c("rest", "total"), 
                                  response_name="response", rest_name=NULL){
  
    type <- match.arg(type)
    
    yy <- rlang::enquo(y) |> rlang::as_name() ## dead
    xx <- rlang::enquo(size) |> rlang::as_name() ## total (or rest)
    if (is.null(rest_name)){
        rest_name <- paste0("not_", yy)
    }
    `:=` = NULL
    if (identical(type, "total")){        
        data. <- data. |> 
            mutate(!!sym(rest_name) := !!sym(xx) - !!sym(yy)) |>
            select(-!!sym(xx))
        tidy_data <- data. |>
            tidyr::pivot_longer(c(!!sym(yy), !!sym(rest_name)),
                                names_to  = response_name,
                                values_to = "n")
    } else {
        tidy_data <- data. |>
            tidyr::pivot_longer(c(!!sym(yy), !!sym(xx)),
                                names_to  = response_name,
                                values_to = "n")
    }
    out <- tidy_data |> tidyr::uncount(n)
    resp <- out[,response_name] |> pull()
    lev <- rev(c(yy, setdiff(unique(resp), yy)))
    
    
    out[,response_name] <- out[,response_name] |> pull() |> factor(levels=lev)
    return(out)
}


