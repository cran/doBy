#' @title Model stability for glm objects
#' 
#' @param data. A data frame
#' @param model A glm object
#' @param n.searches Number of searches
#' @param method Method for generating data
#' @param ... Additional arguments to be passed to \code{\link{step}}
#' 
#' @export
model_stability_glm <- function(data., model, n.searches=10, method=c("subgroups", "resample"), ...){
  method <- match.arg(method)
    data_list <- generate_data_list(data., K=n.searches, method=method)

    lhs <- as.character(model$formula[[2]])
    
    
    fit_list <- data_list |>
        lapply(function(dat..){
            model <- update(model, data=dat..)
            step(model, ...)
        })
    
    fit_list2 <- lapply(
        fit_list, function(x){
            update(x, data=data.)
        }
    )
##    fit_list2 <<- fit_list2
    rhs_nms  <- get_rhs_nms(model)    
    rhs_list <- get_predictor_list(fit_list2)
    rhs_matrix <- glist2matrix(rhs_list, rhs_nms, aggregate=TRUE)
    rhs_raw    <- glist2matrix(rhs_list, rhs_nms, aggregate=FALSE)    
    rhs_matrix <- as.data.frame(rhs_matrix)
    freq <- rhs_matrix$Freq__
    rhs_matrix$Freq__ <- NULL

    rhs_matrix = as(t(rhs_matrix), "dgCMatrix")
    rhs_raw = as(t(rhs_raw), "dgCMatrix")

    loc <- get_location(rhs_raw, rhs_matrix)    
    
    out <- list(
        call = match.call(),
        model = model,
        data. = data.,
        data_list = data_list,
        formula = formula(model),
        n.searches = n.searches,
        fit = fit_list2,
        rhs_matrix = rhs_matrix,
        rhs_raw = rhs_raw,
        n.searches = n.searches,
        loc = loc,
        lhs = lhs,
        freq = freq)
    
    class(out) <- "model_stability_glm_class"
    out
}


#' @export
print.model_stability_glm_class <- function(x, ...){
  x$data. <- NULL
  print.default(x[c("rhs_matrix", "freq")])    
}

#' @export
formula.model_stability_glm_class <- function(x, fit=FALSE, unique=TRUE, text=FALSE, ...){

    if (fit){
        text <- FALSE
    }
    
    f <- get_formulas(x, unique=unique, text=text)
    if (!fit){
        f
    } else {
        cl <- x$model$call
        lapply(f, function(z) {
            cl$formula <- z
            out <- update(eval(cl), data=x$data.)
            out
        }
        )
    }
}


#' @export
summary.model_stability_glm_class <- function(object, ...){

    freq_pred <- object$rhs_raw |> rowSums()
    num_pred <- object$rhs_matrix |> colSums()

    out <- list(number_of_models=ncol(object$rhs_raw),
                number_of_unique_models=ncol(object$rhs_matrix),                
                number_of_predictors=num_pred, 
                frequency_of_predictors=freq_pred)
    
    class(out) <- "model_stability_glm_summary_class"
    out
}



#' @export
print.model_stability_glm_summary_class <- function(x, ...){
    print.default(unclass(x))
}

#' @title Get formulas from model_stability_glm_class object
#' @param object A model_stability_glm_class object
#' @param unique If TRUE, return unique models
#' @param text If TRUE, return text (rather than formula).  
#' @export
get_formulas <- function(object, unique=TRUE, text=FALSE){

    if (unique){
        mmm <- object$rhs_matrix        
    } else {
        mmm <- object$rhs_raw        
    }

    
    rhs2formula <- function(object, i, text=FALSE) {
        out <- paste0(object$lhs, "~", paste0(names(which(mmm[,i]==1)), collapse="+"))
        if (!text)
            as.formula(out)
        else
            out
    }

    out <- lapply(1:ncol(mmm),
                  function(i){
                      rhs2formula(object, i, text=text)
                  })
    return(out)
}



#' @title Cross-validation for list of glm objects
#' 
#' @param data. A data frame
#' @param fit_list A list of glm objects
#' @param K Number of folds
#' 
#' @importFrom boot cv.glm
#' @export
cv_glm_fitlist <- function(data., fit_list, K=5){
    #  sapply(obj, inherits, "glm")
      fit_list <- lapply(fit_list, function(x){
          if(!inherits(x, "glm"))  stop("x must be an glm object")
          update(x, data=data.)
      })

      cv.error <- sapply(fit_list, function(x){

        boot::cv.glm(data., x, K=K)$delta[1] 
    })
    return(cv.error)    
}











get_rhs_nms <- function(model){
  nms <- names(eval(model$call$data))
  lhs <- as.character(formula(model)[[2]])
  rhs_nms <- setdiff(nms, lhs)
  rhs_nms      
}

get_location <- function(rhs_raw, rhs_matrix){
    u <- apply(rhs_raw, 1, paste0, collapse='')
    z <- apply(rhs_matrix, 1, paste0, collapse='')

    loc <- sapply(u, function(i){
        (i == z)  |> which()
    })
    names(loc) <- NULL
    loc
}


get_predictor_list <- function(x){
  rhs_list <-
    lapply(x,
           function(s){
             s |> terms() |> attr("term.labels")  
           })
  rhs_list
}


get_rhs_matrix <- function(x, aggregate=FALSE){
  if (!inherits(x, "model_stability_glm_class"))
    stop("'x' is not model_stability_glm_class\n")
  rhs_list <- get_predictor_list(x$fit)
  rhs_raw <- glist2matrix(rhs_list, aggregate=aggregate)
  t(rhs_raw)
}

aggregate_matrix <- function(mat){
  
    if (ncol(mat) > 0){
        mat <- mat |> as.data.frame()  |> table() |> as.data.frame.table()
        mat <- dplyr::filter(mat, .data$Freq>0)
        mat <- mat[order(mat$Freq, decreasing=TRUE),]
        mat <- sapply(mat, 
                      function(o){
                        as.numeric(as.character(o))
                        })
        
        if (is.null(dim(mat)))
            mat <- as.data.frame(t(mat))
        else
          mat
    } else { 
        as.data.frame(cbind(Freq=nrow(mat)))        
    }
}

glist2matrix <- function(rhs_list, nms, aggregate=TRUE){
  
  
  # Get unique terms across all lists
  unique_terms <- unique(unlist(rhs_list))
  
  # Create an empty matrix M with 3 rows and columns equal to the number of unique terms
  M <- matrix(0, nrow = length(rhs_list), ncol = length(unique_terms), 
              dimnames = list(NULL, unique_terms))
  
  # Iterate over each list and mark the presence of terms in the matrix
  for (i in seq_along(rhs_list)) {
        ##print(rhs_list[[i]])
        M[i, rhs_list[[i]]] <- 1
  }

  if (aggregate) {
    M2 <- aggregate_matrix(M)
    colnames(M2) <- c(colnames(M), "Freq__")
    M2
  }
  else
    M
  
  
    #   zz <- matrix(0, length(rhs_list), length(nms))
    #     colnames(zz) <- nms
    #         ii <- lapply(rhs_list, match, nms)
    # 
    # for (i in 1:length(rhs_list)){
    #     zz[i, ii[[i]]] <- 1
    # }
    # 
    # if (aggregate) {
    #    zz2 <- aggregate_matrix(zz)
    #     colnames(zz2) <- c(nms, "Freq__")
    #     zz2
    #     }
    # else
    #     zz
}


#' @title Generate data list
#' @param data. A data frame
#' @param K Number of folds
#' @param method Method for generating data
#' 
#' @export
generate_data_list <- function(data., K, method=c("subgroups", "resample")){
  
  method <- match.arg(method)
  
  sample0 <- function (x, ...) {
    x[sample.int(length(x), ...)]
  }
  
  out <- as.list(rep(NA, K))
  
  switch (method,
          "subgroups" = {
            n <- nrow(data.)
            f <- ceiling(n/K)
            s <- sample0(rep(1L:K, f), n)
            
            ms <- max(s)
            for (i in seq_len(ms)) {
              j.in <- seq_len(n)[(s != i)]
              di <- data.[j.in, , drop = FALSE]
              out[[i]] <- di
            }
          },
          "resample" = {
            for (i in 1:K){
              j.in <- sample(nrow(data.), replace=TRUE) |> sort()
              di <- data.[j.in, , drop = FALSE]
              out[[i]] <- di              
            }
          }
  )
  out
}



# predictor.list <-
#     lapply(object$fit,
#            function(s){
#                s |> terms() |> attr("term.labels")  
#            })
# rhs.matrix <- glist2matrix(predictor.list, aggregate=FALSE)



# stability_glm_old <- function(data., model, n.searches=5, k=2, ...){    
#   
#   formula. <- formula(model)        
#   data_list <- modelr::crossv_kfold(data., k = n.searches, id = ".id")
#   
#   fit_list <- data_list[["train"]] |>
#     lapply(function(dat..){
#       model <- update(model, data=dat..)
#       step(model, ..., trace=0)
#     })
#   
#   fit_list2 <- lapply(
#     fit_list, function(x){
#       update(x, data=data.)
#     }
#   )
#   
#   out <- list(call=match.call(),
#               model= model,
#               data.=data.,
#               data_list=data_list,
#               formula=formula.,
#               k=k,                
#               n.searches=n.searches,
#               fit=fit_list2
#   )
#   
#   class(out) <- "stability_glm_class"
#   out
# }
# 

## stability_glm <- function(data., formula., k=2, ...){
##     cat("NOTE: This is an experimental functionality\n")
##     fit_list <- data.[["train"]] |>
##         map(function(dat..){
##             step(lm(formula., data=dat..), k=k, ..., trace=0)    
##         })

##     out <- list(call=match.call(),
##                 data=data.,
##                 formula=formula.,
##                 k=k,                
##                 n.fold=dim(data.)[1],
##                 fit=fit_list
##                 )
##     class(out) <- "stability_glm_class"
##     out
## }




## #' @export
## model_stability_glm <- function(data., model, n.searches=5, method=c("resample", "subgroups"), ...){
##     data_list <- generate_data_list(data., K=n.searches, method=method)
##     formula. <- formula(model)
    
##     sapply(data_list, nrow)
##     fit_list <- data_list |>
##         lapply(function(dat..){
##             model <- update(model, data=dat..)
##             step(model, ...)
##         })
    
##     fit_list2 <- lapply(
##         fit_list, function(x){
##             update(x, data=data.)
##         }
##     )
    
##     rhs_nms <- get_rhs_nms(model)    
##     predictor.list <- get_predictor_list(fit_list2)
##     rhs <- glist2matrix(predictor.list, rhs_nms, aggregate=TRUE)
##     rhs <- as.data.frame(rhs)
##     freq <- rhs$Freq__
##     rhs$Freq__ <- NULL
    
    
##     out <- list(
##         call = match.call(),
##         model = model,
##         data. = data.,
##         data_list = data_list,
##         formula = formula.,
##         n.searches = n.searches,
##         fit = fit_list2,
##         rhs_matrix = as(t(rhs), "dgCMatrix"),
##         freq = freq)
    
##     class(out) <- "model_stability_glm_class"
##     out
## }


## #' @export
## summary.model_stability_glm_class <- function(object, ...){
##     lhs <- as.character(object$formula[[2]])

##     rhs_nms <- get_rhs_nms(object$model)    
##     predictor.list <- get_predictor_list(object$fit)
    
##     rhs_raw <- glist2matrix(predictor.list, rhs_nms, aggregate=FALSE)
##     rhs_matrix <- glist2matrix(predictor.list, rhs_nms, aggregate=TRUE)
##     rownames(rhs_matrix) <- 1:nrow(rhs_matrix)
    
##     rhs_matrix <-
##         apply(rhs_matrix, 2, as.numeric, simplify=FALSE) |> 
##         as.data.frame()
    
##     freq <- rhs_matrix$Freq__
##     rhs_matrix$Freq__ <- NULL

##     u <- apply(rhs_raw, 1, paste0, collapse='')
##     z <- apply(rhs_matrix, 1, paste0, collapse='')

##     loc <- sapply(u, function(i){
##         (i == z)  |> which()
##     })
##     names(loc) <- NULL
##     loc
    
##     n.searches <- length(object$fit)

##     rhs_raw <- t(rhs_raw)
##     rhs_matrix=t(rhs_matrix)
    
##     out <- list(
##         model = object$model,
##         n.searches = n.searches  ,
##         data. = object$data.,
##         lhs = lhs,
##         rhs_matrix = object$rhs_matrix, #as(t(rhs_matrix), "dgCMatrix"),
##         freq = object$freq,
##         rhs_raw = rhs_raw,
##         rhs_raw = as(t(rhs_raw), "dgCMatrix"),
##         loc=loc
##     )
##     class(out) <- "model_stability_glm_summary_class"
##     out
## }




## #' @export
## formula.model_stability_glm_class <- function(x, fit=FALSE, ...){
##   if (!fit){
##     return(lapply(x$fit, formula))
##   } else {
##     x$fit
##   }
## }

## #' @export
## formula.model_stability_glm_summary_class <- function(x, fit=FALSE, ...){
##   if (!fit){
##     get_formulas(x)
##   } else {
##     f <- get_formulas(x)
##     cl <- x$model$call
##     lapply(f, function(z) {
##       cl$formula <- z
##       out <- update(eval(cl), data=x$data.)
##       out
##     }
##     )
##   }
## }



