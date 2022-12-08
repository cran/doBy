## ----echo=FALSE-------------------------------------------------------------------------
require( doBy )
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date())

## ----include=FALSE,echo=FALSE-----------------------------------------------------------
library(knitr)

## ----r setup, echo=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(prompt=TRUE) 
library(doBy)
if (!dir.exists("figures")) dir.create("figures")
opts_chunk$set(
               tidy=FALSE,fig.path='figures/doBy'
           )

oopt <- options()
options("digits"=4, "width"=90, "prompt"="> ", "continue"="  ")
options(useFancyQuotes="UTF-8")

## ----echo=F-----------------------------------------------------------------------------
library(doBy)

## ---------------------------------------------------------------------------------------
f  <- function(a, b, c=4, d=9){
    a + b + c + d
}
fr_ <- section_fun(f, list(b=7, d=10))
fr_
f(a=10, b=7, c=5, d=10)
fr_(a=10, c=5)

## ---------------------------------------------------------------------------------------
fe_ <- section_fun(f, list(b=7, d=10), method = "env")
fe_
f(a=10, b=7, c=5, d=10)
fe_(a=10, c=5)

## ---------------------------------------------------------------------------------------
get_section(fe_) 
## attr(fe_, "arg_env")$args ## Same result
get_fun(fe_) 
## environment(fe_)$fun ## Same result

## ---------------------------------------------------------------------------------------
n <- 4
toeplitz(1:n)

## ---------------------------------------------------------------------------------------
inv_toeplitz <- function(n) {
    solve(toeplitz(1:n))
}
inv_toeplitz(4)

## ---------------------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
    inv_toeplitz(4), inv_toeplitz(8), inv_toeplitz(16),
    inv_toeplitz(32), inv_toeplitz(64),
    times=5
)

## ---------------------------------------------------------------------------------------
n.vec  <- c(4, 8, 16, 32, 64)
fun_list <- lapply(n.vec,
                  function(ni){
                      section_fun(inv_toeplitz, list(n=ni))}
                  )

## ---------------------------------------------------------------------------------------
fun_list[[1]]
fun_list[[1]]()

## ---------------------------------------------------------------------------------------
bquote_list <- function(fnlist){
    lapply(fnlist, function(g) {
        bquote(.(g)())
    }
    )
}

## ---------------------------------------------------------------------------------------
bq_fun_list <- bquote_list(fun_list)
bq_fun_list[[1]]
## Evaluate one:
eval(bq_fun_list[[1]])
## Evaluate all:
## sapply(bq_fun_list, eval)

## ---------------------------------------------------------------------------------------
names(bq_fun_list) <- n.vec
microbenchmark(
  list  = bq_fun_list,
  times = 5
)

