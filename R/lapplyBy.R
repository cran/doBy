lapplyBy <- function (formula, data = parent.frame(), FUN) 
{
    ddd <- splitBy(formula, data = data)

    gr  <- unique(attr(ddd,"grps"))
    ##print(gr)    
    ddd <- lapply(ddd, FUN)
    ddd <- ddd[gr]

    return(ddd)
}
