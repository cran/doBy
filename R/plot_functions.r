#' @title Plot linear model object
#'
#' @param lm_fit An object of class 'lm'
#' @param format The format of the plot (or a list of plots if format is "list")
#'
#' @examples
#'
#' m1 <- lm(speed ~ dist, data=cars)
#' plot_lm(m1)
#' plot_lm(m1, "2x2")
#' plot_lm(m1, "1x4")
#' plot_lm(m1, "4x1")
#' plot_lm(m1, "list")
#' 
#' @export
plot_lm <- function(lm_fit, format="2x2") {

    if(!inherits(lm_fit, "lm"))
        stop("'lm_fit' must inherit form 'lm'\n")

    format <- match.arg(format, c("2x2", "1x4", "4x1", "list"))
    
    dd <- data.frame(fitted_values          = predict(lm_fit),
                   ## residuals           = resid(lm_fit),
                   observed_values        = predict(lm_fit)+resid(lm_fit),
                   standardized_residuals = rstandard(lm_fit))
    
    pl1 <- ggplot(dd, aes(x = .data$fitted_values, y = .data$standardized_residuals)) +
        geom_point() + geom_hline(yintercept=c(0, -1.96, 1.95))
    
    pl2 <- ggplot(dd, aes(x=.data$fitted_values, y=.data$observed_values)) +
        geom_point() +  geom_smooth(method="lm", formula=y ~ x, se=FALSE)
    
    pl3 <- ggplot(dd, aes(x=.data$fitted_values, y=sqrt(abs(.data$standardized_residuals)))) + geom_point()

    pl4 <- ggplot(dd, aes(sample = .data$standardized_residuals)) + stat_qq() + stat_qq_line() +
        labs(y="standardized_residuals", x = "theoretical quantiles")

    switch(format,
           "2x2"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=2) },
           "1x4"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=1) },
           "4x1"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=4) },
           "list"={ list(pl1=pl1, pl2=pl2, pl3=pl3, pl4=pl4) }
           )

}






#' @title Two-way interaction plot 
#'
#' @description Plots the mean of the response for
#'     two-way combinations of factors, thereby illustrating possible
#'     interactions.
#'
#' @name interaction-plot
#'
#' @note This is a recent addition to the package and is subject to change.
#'
#' @param .data A data frame
#' @param .formula A formula of the form `y ~ x1 + x2`
#' @param interval Either `conf.int`, `boxplot` or `none`
#'
#' @examples
#'
#' ToothGrowth |> interaction_plot(len ~ dose + supp)
#' ToothGrowth |> interaction_plot(len ~ dose + supp, interval="conf.int")
#' ToothGrowth |> interaction_plot(len ~ dose + supp, interval="boxplot")
#' ToothGrowth |> interaction_plot(len ~ dose + supp, interval="none")

#' @import dplyr
#' @import ggplot2

#' @export
interaction_plot <- function(.data, .formula, interval="conf.int"){

    interval = match.arg(tolower(interval), c("conf.int", "boxplot", "none"))

    if (!inherits(.formula, "formula")) stop("'.formula' is not formula")
    if (length(.formula) != 3)          stop("'.formula' is not two sided")
    
    lhs <- all.vars(.formula[[2]])
    rhs <- all.vars(.formula[[3]])
    if (length(rhs) < 2) stop("rhs must have at least two elements")

    rr <- sym(lhs)
    s1 <- sym(rhs[1])
    s2 <- sym(rhs[2])
    
    ## If lhs is transformed
    resp <- eval(.formula[[2]], .data)
    .data$RESP <- resp  ## KLUDGY
    rr2 <- sym("RESP")  ## KLUDGY

    dd1 <- .data |> group_by(!!sym(s1), !!sym(s2)) 
    tmp <- dd1 |> summarise(val = mean({{ rr2 }}),
                             sd  = sd({{ rr2 }}) / sqrt(n()),
                             lwr = .data$val - 1.96 * sd, upr=.data$val + 1.96 * sd,
                             .groups="keep")
    ##print(.data); print(dd1); print(tmp)

    switch(interval,
           "boxplot" = {
               ## BOXPLOT
               pp <- ggplot(.data, aes(x = factor(!!sym(s1)), y = !!sym(rr2),
                                       colour = !!sym(s2))) 
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_boxplot() + ## FIXME want conf. interval bars
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) + 
                   theme_bw()
           },
           "conf.int" = {
               ## mean +/- 2 sd
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = .data$val,
                                     colour = !!sym(s2)))
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) + 
                   geom_errorbar(aes(ymin=.data$lwr, ymax=.data$upr), data=tmp,
                                 width=.4, position=position_dodge(0.1))
           },
           "none" = {
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = .data$val,
                                     colour = !!sym(s2)))               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) + 
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) 
           })
}
