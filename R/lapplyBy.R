lapplyBy <- function (formula, data = parent.frame(), FUN) 
{
    ddd <- splitBy(formula, data = data)
    ddd <- lapply(ddd, FUN)
    return(ddd)
}
