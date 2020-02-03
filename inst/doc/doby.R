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
dir.create("figures")
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
ff  <- function(a, b=2, c=4){a + b + c}
ff1 <- specialize(ff, arglist=list(a=1, b=7, yy=123))
ff1
gg  <- rnorm
gg1 <- specialize(gg, list(n=10))
gg1

## -----------------------------------------------------------------------------
f  <- function(a) {a <- a + 1; a}
f1 <- specialize(f, list(a = 10))
f1

## -----------------------------------------------------------------------------
x <- c(1,1,1,2,2,2,1,1,1,3)
firstobs(x)
lastobs(x)

## -----------------------------------------------------------------------------
firstobs(~Plant, data=CO2)
lastobs(~Plant, data=CO2)

## -----------------------------------------------------------------------------
x <- c(1:4,0:5,11,NA,NA)
which.maxn(x,3)
which.minn(x,5)

## -----------------------------------------------------------------------------
x <- c(1,1,2,2,2,1,1,3,3,3,3,1,1,1)
subSeq(x)
subSeq(x, item=1)
subSeq(letters[x])
subSeq(letters[x],item="a")

## -----------------------------------------------------------------------------
x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)

## -----------------------------------------------------------------------------
head(renameCol(CO2, 1:2, c("kk","ll")))
head(renameCol(CO2, c("Plant","Type"), c("kk","ll")))

## -----------------------------------------------------------------------------
yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0)

## -----------------------------------------------------------------------------
tvar <- seq_along(yvar) + c(0.1,0.2)

## -----------------------------------------------------------------------------
tse<- timeSinceEvent(yvar,tvar)

## -----------------------------------------------------------------------------
plot(sign.tse~tvar, data=tse, type="b")
grid()
rug(tse$tvar[tse$yvar==1], col="blue",lwd=4)
points(scale(tse$run), col=tse$run, lwd=2)
lines(abs.tse+.2~tvar, data=tse, type="b",col=3)

## -----------------------------------------------------------------------------
plot(tae~tvar, data=tse, ylim=c(-6,6),type="b")
grid()
lines(tbe~tvar, data=tse, type="b", col="red")
rug(tse$tvar[tse$yvar==1], col="blue",lwd=4)
lines(run~tvar, data=tse, col="cyan",lwd=2)

## -----------------------------------------------------------------------------
plot(ewin~tvar, data=tse,ylim=c(1,4))
rug(tse$tvar[tse$yvar==1], col="blue",lwd=4)
grid()
lines(run~tvar, data=tse,col="red")

## -----------------------------------------------------------------------------
tse$tvar[tse$abs<=1]

## -----------------------------------------------------------------------------
lynx <- as.numeric(lynx)
tvar <- 1821:1934
plot(tvar,lynx,type="l")

## -----------------------------------------------------------------------------
yyy <- lynx>mean(lynx)
head(yyy)
sss <- subSeq(yyy,TRUE)
sss

## -----------------------------------------------------------------------------
plot(tvar,lynx,type="l")
rug(tvar[sss$midpoint],col="blue",lwd=4)

## -----------------------------------------------------------------------------
yvar <- rep(0,length(lynx))
yvar[sss$midpoint] <- 1
str(yvar)

## -----------------------------------------------------------------------------
tse <- timeSinceEvent(yvar,tvar)
head(tse,20)

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

