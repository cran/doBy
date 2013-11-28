### R code from vignette source 'LSmeans.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: LSmeans.Rnw:54-57
###################################################
require( doBy )
prettyVersion <- packageDescription("doBy")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: LSmeans.Rnw:95-96
###################################################
options("width"=90, "digits"=3)


###################################################
### code chunk number 3: LSmeans.Rnw:107-111
###################################################
dir.create("figures")
oopt <- options()
options("digits"=4, "width"=80, "prompt"="> ", "continue"="  ")
##options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 4: LSmeans.Rnw:151-152 (eval = FALSE)
###################################################
## lm( y ~ treat + block + year)


###################################################
### code chunk number 5: LSmeans.Rnw:171-173 (eval = FALSE)
###################################################
## library(lme4)
## lmer( y ~ treat + (1|block) + (1|year))


###################################################
### code chunk number 6: LSmeans.Rnw:198-201
###################################################
summary( warpbreaks )
head( warpbreaks, 4 )
ftable(xtabs( ~ wool + tension, data=warpbreaks))


###################################################
### code chunk number 7: LSmeans.Rnw:205-212
###################################################
 opar <- par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
     plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
          varwidth = TRUE, subset = wool == "A", main = "Wool A")
     plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
          varwidth = TRUE, subset = wool == "B", main = "Wool B")
     mtext("warpbreaks data", side = 3, outer = TRUE)
     par(opar)


###################################################
### code chunk number 8: LSmeans.Rnw:220-221
###################################################
(warp.lm <- lm(breaks ~ wool + tension, data=warpbreaks))


###################################################
### code chunk number 9: LSmeans.Rnw:226-228
###################################################
uni <- unique(warpbreaks[,2:3])
prd <- cbind(breaks=predict(warp.lm, newdata=uni), uni); prd


###################################################
### code chunk number 10: LSmeans.Rnw:243-244
###################################################
LSmeans(warp.lm, effect="tension")


###################################################
### code chunk number 11: LSmeans.Rnw:250-251
###################################################
doBy::summaryBy(breaks ~ tension, data=warpbreaks)


###################################################
### code chunk number 12: LSmeans.Rnw:257-258
###################################################
K <- LSmatrix(warp.lm, effect="tension"); K


###################################################
### code chunk number 13: LSmeans.Rnw:262-263
###################################################
linest( warp.lm, K=K )


###################################################
### code chunk number 14: LSmeans.Rnw:272-273
###################################################
(warp.lm2 <- update(warp.lm, .~.+wool:tension))


###################################################
### code chunk number 15: LSmeans.Rnw:278-280
###################################################
K2 <- LSmatrix(warp.lm2, effect="tension"); K2
linest(warp.lm2, K=K2)


###################################################
### code chunk number 16: LSmeans.Rnw:293-296
###################################################
warp.poi <- glm(breaks ~ wool + tension, family=poisson, data=warpbreaks)
LSmeans(warp.poi, effect="tension", type="link")
LSmeans(warp.poi, effect="tension", type="response")


###################################################
### code chunk number 17: LSmeans.Rnw:307-310
###################################################
warp.qpoi <- glm(breaks ~ wool + tension, family=quasipoisson, data=warpbreaks)
LSmeans(warp.qpoi, effect="tension", type="link")
LSmeans(warp.qpoi, effect="tension", type="response")


###################################################
### code chunk number 18: LSmeans.Rnw:316-319
###################################################
warp.poi2 <- glm(breaks ~ wool + tension, family=poisson(link=identity),
                 data=warpbreaks)
LSmeans(warp.poi2, effect="tension", type="link")


###################################################
### code chunk number 19: LSmeans.Rnw:333-336
###################################################
warp.gam <- glm(breaks ~ wool + tension, family=Gamma(link=identity),
                 data=warpbreaks)
LSmeans(warp.gam, effect="tension", type="link")


###################################################
### code chunk number 20: LSmeans.Rnw:356-358
###################################################
warp.poi3 <- glm(breaks ~ wool + tension, family=quasipoisson(link=identity), data=warpbreaks)
LSmeans(warp.poi3, effect="tension")


###################################################
### code chunk number 21: LSmeans.Rnw:363-366
###################################################
library(lme4)
warp.mm <- lmer(breaks ~ tension + (1|wool), data=warpbreaks)
LSmeans(warp.mm, effect="tension")


###################################################
### code chunk number 22: LSmeans.Rnw:374-375
###################################################
VarCorr(warp.mm)


###################################################
### code chunk number 23: LSmeans.Rnw:382-383
###################################################
LSmeans(warp.mm, effect="tension", adjust.df=FALSE)


###################################################
### code chunk number 24: LSmeans.Rnw:388-392
###################################################
library(geepack)
warp.gee <- geeglm(breaks ~ tension, id=wool, family=poisson, data=warpbreaks)
LSmeans(warp.gee, effect="tension")
LSmeans(warp.gee, effect="tension", type="response")


###################################################
### code chunk number 25: LSmeans.Rnw:401-405
###################################################
library(ggplot2)
ChickWeight$Diet <- factor(ChickWeight$Diet)
qplot(Time, weight, data=ChickWeight, colour=Chick, facets=~Diet,
      geom=c("point","line"))


###################################################
### code chunk number 26: LSmeans.Rnw:410-412
###################################################
rr <- lmer(weight~Time*Diet + (0+Time|Chick), data=ChickWeight)
coef(summary(rr))


###################################################
### code chunk number 27: LSmeans.Rnw:418-419
###################################################
LSmatrix(rr, effect="Diet")


###################################################
### code chunk number 28: LSmeans.Rnw:426-427
###################################################
K1 <- LSmatrix(rr, effect="Diet", at=list(Time=1)); K1


###################################################
### code chunk number 29: LSmeans.Rnw:433-436
###################################################
K0 <- LSmatrix(rr, effect="Diet", at=list(Time=0))
K1-K0
LSmeans(rr, K=K1-K0)


###################################################
### code chunk number 30: LSmeans.Rnw:441-448
###################################################
LSmeans_trend <- function(object, effect, trend){

    K<-LSmatrix(object, effect=effect, at=as.list(setNames(1, trend))) -
        LSmatrix(object, effect=effect, at=as.list(setNames(0, trend)))
    LSmeans(object, K=K)
}
LSmeans_trend(rr, effect="Diet", trend="Time")


###################################################
### code chunk number 31: LSmeans.Rnw:456-462
###################################################
data(CO2)
CO2 <- transform(CO2, Treat=Treatment, Treatment=NULL)
levels(CO2$Treat) <- c("nchil","chil")
levels(CO2$Type) <- c("Que","Mis")
ftable(xtabs( ~ Plant + Type + Treat, data=CO2), col.vars=2:3)
##CO2 <- subset(CO2, Plant %in% c("Qn1", "Qc1", "Mn1", "Mc1"))


###################################################
### code chunk number 32: LSmeans.Rnw:466-467
###################################################
qplot(x=log(conc), y=uptake, data=CO2, color=Treat, facets=~Type)


###################################################
### code chunk number 33: LSmeans.Rnw:472-474
###################################################
co2.lm1 <- lm(uptake ~ conc + Type + Treat, data=CO2)
LSmeans(co2.lm1, effect="Treat")


###################################################
### code chunk number 34: LSmeans.Rnw:480-482 (eval = FALSE)
###################################################
## co2.lm <- lm(uptake ~ log(conc) + Type + Treat, data=CO2)
## LSmeans(co2.lm, effect="Treat")


###################################################
### code chunk number 35: LSmeans.Rnw:487-490
###################################################
co2.lm2 <- lm(uptake ~ log.conc + Type + Treat,
             data=transform(CO2, log.conc=log(conc)))
LSmeans(co2.lm2, effect="Treat")


###################################################
### code chunk number 36: LSmeans.Rnw:497-499
###################################################
co2.lm3 <- lm(uptake ~ conc + I(conc^2) + Type + Treat, data=CO2)
LSmeans(co2.lm3, effect="Treat")


###################################################
### code chunk number 37: LSmeans.Rnw:507-510
###################################################
co2.lm4 <- lm(uptake ~ conc + conc2 + Type + Treat, data=
              transform(CO2, conc2=conc^2))
LSmeans(co2.lm4, effect="Treat")


###################################################
### code chunk number 38: LSmeans.Rnw:515-516
###################################################
LSmeans(co2.lm4, effect="Treat", at=list(conc=10, conc2=100))


###################################################
### code chunk number 39: LSmeans.Rnw:527-535
###################################################
## Make balanced dataset
dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat.bal$y <- rnorm(nrow(dat.bal))

## Make unbalanced dataset:  'BB' is nested within 'CC' so BB=1
## is only found when CC=1 and BB=2,3 are found in each CC=2,3,4
dat.nst <- dat.bal
dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))


###################################################
### code chunk number 40: LSmeans.Rnw:541-543
###################################################
head(dat.nst)
ftable(xtabs( ~ AA + BB + CC, data=dat.nst))


###################################################
### code chunk number 41: LSmeans.Rnw:548-550
###################################################
mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)
coef(mod.nst)


###################################################
### code chunk number 42: LSmeans.Rnw:557-558
###################################################
LSmeans(mod.nst, effect=c("BB", "CC"))


###################################################
### code chunk number 43: LSmeans.Rnw:571-576
###################################################
simdat<-structure(list(treat = structure(c(1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L
), .Label = c("t1", "t2"), class = "factor"), year = structure(c(1L,
1L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"),
    y = c(0.9, 1, 1.1, 3, 3, 4.9, 5, 5.1)), .Names = c("treat", "year",
"y"), row.names = c(NA, -8L), class = "data.frame")


###################################################
### code chunk number 44: LSmeans.Rnw:581-582
###################################################
simdat


###################################################
### code chunk number 45: LSmeans.Rnw:586-587
###################################################
qplot(treat, y, data=simdat, color=year)


###################################################
### code chunk number 46: LSmeans.Rnw:592-593
###################################################
LSmeans( lm(y~treat+year, data=simdat), effect="treat")


###################################################
### code chunk number 47: LSmeans.Rnw:597-598
###################################################
summaryBy(y~treat, data=simdat)


###################################################
### code chunk number 48: LSmeans.Rnw:616-619
###################################################
library("multcomp")
g1 <- glht(warp.lm, mcp(tension="Tukey"))
summary( g1 )


###################################################
### code chunk number 49: LSmeans.Rnw:624-625
###################################################
K1 <- g1$linfct; K1


###################################################
### code chunk number 50: LSmeans.Rnw:637-638
###################################################
X <- model.matrix( mod.nst ); as(X,"Matrix")


###################################################
### code chunk number 51: LSmeans.Rnw:652-654
###################################################
K <- LSmatrix(mod.nst, effect="BB", at=list(CC=2));K
LSmeans(mod.nst, K=K)


###################################################
### code chunk number 52: LSmeans.Rnw:665-668
###################################################
XtXinv <- MASS::ginv(t(X)%*%X)
bhat <- as.numeric(XtXinv %*% t(X) %*% dat.nst$y)
zapsmall(bhat)


###################################################
### code chunk number 53: LSmeans.Rnw:674-675
###################################################
K %*% bhat


###################################################
### code chunk number 54: LSmeans.Rnw:702-704
###################################################
S<-svd(X)
names(S)


###################################################
### code chunk number 55: LSmeans.Rnw:708-710
###################################################
B<-S$v[, S$d<1e-10, drop=FALSE ]; zapsmall(B) ## Basis for N(X)
zapsmall( rowSums(K%*%B) )


