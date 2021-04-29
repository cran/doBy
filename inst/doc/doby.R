## ----echo=FALSE---------------------------------------------------------------
#require( doBy )
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date())

## ----include=FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(
tidy=FALSE,fig.path='figures/doBy'
)

## ----echo=FALSE---------------------------------------------------------------
library(doBy)
if (!dir.exists("figures")) dir.create("figures")
oopt <- options()
options("digits"=4, "width"=80, "prompt"=" ", "continue"="  ")
options(useFancyQuotes="UTF-8")

## ----echo=F-------------------------------------------------------------------
library(doBy)

## -----------------------------------------------------------------------------
data(CO2)
CO2 <- transform(CO2, Treat=Treatment, Treatment=NULL)
levels(CO2$Treat) <- c("nchil","chil")
levels(CO2$Type)  <- c("Que","Mis")
CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))

## -----------------------------------------------------------------------------
airquality <- subset(airquality, Month %in% c(5,6))

## -----------------------------------------------------------------------------
myfun1 <- function(x){c(m=mean(x), s=sd(x))}
summaryBy(cbind(conc, uptake, lu=log(uptake)) ~ Plant, 
          data=CO2, FUN=myfun1)

## ----results='hide'-----------------------------------------------------------
summaryBy(conc ~ Plant, data=CO2, FUN=mean)

## ----results='hide'-----------------------------------------------------------
## Will fail because of log(uptake)
## summaryBy(list(c("conc", "uptake", "log(uptake)"), "Plant"), 
##          data=CO2, FUN=myfun1)
## Works
summaryBy(list(c("conc", "uptake"), "Plant"), 
          data=CO2, FUN=myfun1)

## -----------------------------------------------------------------------------
x1 <- orderBy(~ Temp + Month, data=airquality)
head(x1)

## -----------------------------------------------------------------------------
x2 <- orderBy(~ - Temp + Month, data=airquality)

## -----------------------------------------------------------------------------
x3 <- orderBy(c("Temp", "Month"), data=airquality)
x4 <- orderBy(c("-Temp", "Month"), data=airquality)

## -----------------------------------------------------------------------------
x <- splitBy(~ Month, data=airquality)
lapply(x, head, 4)
attributes(x)

## ----results='hide'-----------------------------------------------------------
splitBy("Month", data=airquality)

## -----------------------------------------------------------------------------
x <- subsetBy(~Month, subset=Wind > mean(Wind), data=airquality)
head(x)

## -----------------------------------------------------------------------------
x <- transformBy(~Month, data=airquality, 
                 minW=min(Wind), maxW=max(Wind),
                 chg = diff(range(Wind)))
head(x)

## -----------------------------------------------------------------------------
x <- transformBy("Month", data=airquality, 
                 minW=min(Wind), maxW=max(Wind),
                 chg = diff(range(Wind)))

## -----------------------------------------------------------------------------
f1  <- function(a, b=2, c=4){a + b + c}
f1_ <- restrict(f1, list(a=1, b=7))
class(f1_)
f1_
f1_()

## -----------------------------------------------------------------------------
restrictions(f1_) 
## attr(f1_, "arg_env")$args ## Same result

## -----------------------------------------------------------------------------
original_fun(f1_) 
## environment(f1_)$fun ## Same result

## -----------------------------------------------------------------------------
rnorm5 <- restrict(rnorm, list(n=5))
rnorm5()

## -----------------------------------------------------------------------------
f1s_ <- restrict_sub(f1, list(a=1, b=7))
f1s_
f1s_()

## -----------------------------------------------------------------------------
f2  <- function(a) {a <- a + 1; a}
## Notice that the following is absurd
f2s_ <- restrict_sub(f2, list(a = 10))
f2s_
# do not run: f2s_()
try(f2s_())
## Using the environment approch, the result makes sense
f2_ <- restrict(f2, list(a = 10))
f2_
f2_()

## -----------------------------------------------------------------------------
sum2n <- function(n) {
  s <- 0
  for (i in 1:n) s <- s + i
  s
}
sum2n(10)

## -----------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  sum2n(10), sum2n(100), sum2n(1000), sum2n(10000),
  times=5
)

## -----------------------------------------------------------------------------
n.vec  <- c(10, 100, 1000, 10000)
fn.list <- lapply(n.vec, function(a.) restrict(sum2n, list(n=a.)))
fn.list %>% length

## -----------------------------------------------------------------------------
fn.list[[1]]
sapply(fn.list, function(f) do.call(f, list()))

## ----eval=FALSE---------------------------------------------------------------
#  microbenchmark(
#    fn.list[[1]](), fn.list[[2]](), fn.list[[3]](), fn.list[[4]](),
#    times=5
#  )

## -----------------------------------------------------------------------------
dobq <- function(fnlist){
   lapply(fnlist, function(g) bquote(.(g)()))
}
cl.list <- dobq(fn.list)
cl.list[[1]]

## -----------------------------------------------------------------------------
sapply(cl.list, eval)

## -----------------------------------------------------------------------------
names(cl.list) <- n.vec
microbenchmark(
  list=cl.list,
  times=5
)

## -----------------------------------------------------------------------------
x <- c(1,1,1,2,2,2,1,1,1,3)
firstobs(x)
lastobs(x)

## -----------------------------------------------------------------------------
firstobs(~Plant, data=CO2)
lastobs(~Plant, data=CO2)

## -----------------------------------------------------------------------------
x <- c(1:4, 0:5, 11, NA, NA)
which.maxn(x,3)
which.minn(x,5)

## -----------------------------------------------------------------------------
x <- c(1, 1, 2, 2, 2, 1, 1, 3, 3, 3, 3, 1, 1, 1)
subSeq(x)
subSeq(x, item=1)
subSeq(letters[x])
subSeq(letters[x], item="a")

## -----------------------------------------------------------------------------
x <- c("dec", "jan", "feb", "mar", "apr", "may")
src1 <- list(c("dec", "jan", "feb"), c("mar", "apr", "may"))
tgt1 <- list("winter", "spring")
recodeVar(x, src=src1, tgt=tgt1)

## -----------------------------------------------------------------------------
head(renameCol(CO2, 1:2, c("plant_", "type_")))
head(renameCol(CO2, c("Plant", "Type"), c("plant_", "type_")))

## -----------------------------------------------------------------------------
yvar <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0)

## -----------------------------------------------------------------------------
tvar <- seq_along(yvar) + c(0.1, 0.2)

## -----------------------------------------------------------------------------
tse<- timeSinceEvent(yvar, tvar)

## -----------------------------------------------------------------------------
plot(sign.tse ~ tvar, data=tse, type="b")
grid()
rug(tse$tvar[tse$yvar == 1], col="blue",lwd=4)
points(scale(tse$run), col=tse$run, lwd=2)
lines(abs.tse + .2 ~ tvar, data=tse, type="b",col=3)

## -----------------------------------------------------------------------------
plot(tae ~ tvar, data=tse, ylim=c(-6,6), type="b")
grid()
lines(tbe ~ tvar, data=tse, type="b", col="red")
rug(tse$tvar[tse$yvar==1], col="blue", lwd=4)
lines(run ~ tvar, data=tse, col="cyan", lwd=2)

## -----------------------------------------------------------------------------
plot(ewin ~ tvar, data=tse, ylim=c(1, 4))
rug(tse$tvar[tse$yvar==1], col="blue", lwd=4)
grid()
lines(run ~ tvar, data=tse, col="red")

## -----------------------------------------------------------------------------
tse$tvar[tse$abs <= 1]

## -----------------------------------------------------------------------------
lynx <- as.numeric(lynx)
tvar <- 1821:1934
plot(tvar, lynx, type="l")

## -----------------------------------------------------------------------------
yyy <- lynx > mean(lynx)
head(yyy)
sss <- subSeq(yyy, TRUE)
sss

## -----------------------------------------------------------------------------
plot(tvar, lynx, type="l")
rug(tvar[sss$midpoint], col="blue", lwd=4)

## -----------------------------------------------------------------------------
yvar <- rep(0, length(lynx))
yvar[sss$midpoint] <- 1
str(yvar)

## -----------------------------------------------------------------------------
tse <- timeSinceEvent(yvar,tvar)
head(tse, 20)

## -----------------------------------------------------------------------------
len1 <- tapply(tse$ewin, tse$ewin, length)
len2 <- tapply(tse$run, tse$run, length)
c(median(len1), median(len2), mean(len1), mean(len2))

## -----------------------------------------------------------------------------
tse$lynx <- lynx
tse2 <- na.omit(tse)
plot(lynx ~ tae, data=tse2)

## -----------------------------------------------------------------------------
plot(tvar, lynx, type="l", lty=2)
mm <- lm(lynx ~ tae + I(tae^2) + I(tae^3), data=tse2)
lines(fitted(mm) ~ tvar, data=tse2, col="red")

## ----echo=F-------------------------------------------------------------------
options(oopt)

## -----------------------------------------------------------------------------
CO2

## -----------------------------------------------------------------------------
head(airquality, n=20)

