## ----echo=FALSE-------------------------------------------------------------------------
require( doBy )
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date())

## ----include=FALSE----------------------------------------------------------------------
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
f1  <- function(a, b, c=4, d=9){
    a + b + c + d
}
f1_ <- restrict_fun(f1, list(b=7, d=10))
class(f1_)

## ---------------------------------------------------------------------------------------
f1_
f1_(100)

## ---------------------------------------------------------------------------------------
get_restrictions(f1_) 
## attr(f1_, "arg_env")$args ## Same result
get_fun(f1_) 
## environment(f1_)$fun ## Same result

## ---------------------------------------------------------------------------------------
rnorm5 <- restrict_fun(rnorm, list(n=5))
rnorm5()

## ---------------------------------------------------------------------------------------
f1s_ <- restrict_fun_sub(f1, list(b=7, d=10))
f1s_
f1s_(100)

## ---------------------------------------------------------------------------------------
f2  <- function(a) {
    a <- a + 1
    a
}
## Notice that the following is absurd
f2s_ <- restrict_fun_sub(f2, list(a = 10))
f2s_
# do not run: f2s_()
try(f2s_())

## Using the environment approch, the result makes sense
f2_ <- restrict_fun(f2, list(a = 10))
f2_
f2_()

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
scaf.list <- lapply(n.vec,
                  function(ni){
                      restrict_fun(inv_toeplitz, list(n=ni))}
                  )

## ---------------------------------------------------------------------------------------
scaf.list[[1]]
scaf.list[[1]]()

## ---------------------------------------------------------------------------------------
bquote_list <- function(fnlist){
    lapply(fnlist, function(g) {
        bquote(.(g)())
    }
    )
}

## ---------------------------------------------------------------------------------------
bq.list <- bquote_list(scaf.list)
bq.list[[1]]
## Evaluate one:
eval(bq.list[[1]])
## Evaluate all:
## sapply(bq.list, eval)

## ---------------------------------------------------------------------------------------
names(bq.list) <- n.vec
microbenchmark(
  list  = bq.list,
  times = 5
)

## ---------------------------------------------------------------------------------------
n.vec <- seq(50, 700, by=50)
scaf.list <- lapply(n.vec,
                  function(ni){
                      restrict_fun(inv_toeplitz, list(n=ni))}
                  )
bq.list <- bquote_list(scaf.list)
names(bq.list) <- n.vec
mb <- microbenchmark(
  list  = bq.list,
  times = 5
)
doBy::mb_summary(mb)  %>% head(4)

## ----fig.height=3-----------------------------------------------------------------------
par(mfrow=c(1,2))
y <- mb_summary(mb)$mean
plot(n.vec, y)
plot(log(n.vec), log(y))
mm <- lm(log(y) ~ log(n.vec))
broom::tidy(mm)
abline(mm)

