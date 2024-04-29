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






