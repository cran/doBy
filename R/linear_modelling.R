
##' @title Add predicted values of different types to dataframe
##' 
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of predicted value
##' @param transformation A possible transformation of predicted variable, e.g. reciprocal(), log() etc
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars
##' 
##' @export
add_pred <- function (data, model, var = "pred", type = NULL, transformation=NULL) 
{
    pred2 <- function (model, data, type = NULL) 
    {
        if (is.null(type)) {
            stats::predict(model, data)
        }
        else {
            stats::predict(model, data, type = type)
        }
    }

    pp <- pred2(model, data, type = type)
    if (!is.null(transformation)){
        pp <- transformation(pp)
    }
    data[[var]] <- pp
    data
}

##' @title Reciprocal function
##' @description  A simple function returning the reciprocal of its argument
##' @param x An R object for whih 1/x makes sense
##' @author Søren Højsgaard
##' @export
reciprocal <- function(x){
  1/x    
}

##' @title Add residuals of different types to dataframe
##' 
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of residual value 
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars 
##'
##' @export
add_resid <- function (data, model, var = "resid", type) {

    resid2 <- function(model, type){
        UseMethod("resid2")
    }
    resid2.lm <- function(model,
                          type=c("working", "response", "deviance", 
                                 "pearson", "partial", "rstandard", "rstudent")){
        type <- match.arg(type)

        if (identical(type, "rstandard")){
            return(stats::rstandard(model))                        
        }
        
        if (identical(type, "rstudent")){
            return(stats::rstudent(model))                       
        }

        return(stats::residuals(model, type=type))
    }

    resid2.merMod <- function(model,
                              type=c("deviance", "response")){
        return(residuals(model, type=type))
    }
    
    
    if (missing(type))
        type="working"


    data[[var]] <- resid2(model, type)
    data
}


##' @title Get response variable from model
##' @param object lm or glm object 
## ' @param data dataframe or tibble
## ' @param model model object
## ' @param var name of new variable in dataframe / tibble
## ' @param type type of residual value
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars 
##' @export
response <- function(object){

    is_lm <- function(object) {
        identical(class(object), "lm")
    }
    
    is_glm <- function(object) {
        cls <- class(object)
        (all(c("lm", "glm") %in% cls)) && (length(cls) == 2)
    }
    
    obs <- function(object){
        UseMethod("obs")
    }
    
    obs.lm <- function(object) {
        obs <- model.response(model.frame(object))        
        
        if (is_glm(object)){
            wgt <- unname(model.weights(model.frame(object)))
            if (!is.null(wgt)) {
                obs <- obs * wgt
            }
        }
        return(obs)
    }
    return(obs(object))
}



##' @title Add interaction columns to data frame
##' @param .data dataframe
##' @param .formula right hand sided formula
##' @return dataframe
##' @author Søren Højsgaard
##' @export
add_int <- function(.data, .formula) {

    ff <- rhsf2list(.formula)
    lapply(ff, function(g){
        if (length(g)>1){
            var <- paste(g, collapse="_")
            ia <- apply(.data[,g], 1, function(r) paste(r, collapse="_"))
            .data[[var]] <<- ia
        }})
    .data
}


rhsf2list <- function (.formula) {
    if (is.character(.formula)) 
        return(list(.formula))
    if (is.numeric(.formula)) 
        return(lapply(list(.formula), "as.character"))
    if (is.list(.formula)) 
        return(lapply(.formula, "as.character"))
    .formula0 <- .formula[[length(.formula)]]
    .formula1 <- unlist(strsplit(paste(deparse(.formula0), collapse = ""), 
                             " *\\+ *"))
    .formula2 <- unlist(lapply(.formula1, strsplit, " *\\* *| *: *| *\\| *"), 
                    recursive = FALSE)
    .formula2
}
