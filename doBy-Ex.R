pkgname <- "doBy"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('doBy')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("DATA-beets")
### * DATA-beets

flush(stderr()); flush(stdout())

### Name: beets
### Title: Yield and sugar percentage in sugar beets from a split plot
###   experiment.
### Aliases: beets
### Keywords: datasets

### ** Examples

data(beets)
## maybe str(beets) ; plot(beets) ...

beets$bh <- with(beets, interaction(block, harvest))
summary(aov(yield~block+sow+harvest+Error(bh), beets))
summary(aov(sugpct~block+sow+harvest+Error(bh), beets))



cleanEx()
nameEx("DATA-budworm")
### * DATA-budworm

flush(stderr()); flush(stdout())

### Name: budworm
### Title: Effect of Insecticide on survivial of tobacco budworms
### Aliases: budworm
### Keywords: datasets

### ** Examples

data(budworm)
## function to caclulate the empirical logits
empirical.logit<- function(nevent,ntotal) {
  y<-log ((nevent+0.5)/(ntotal-nevent+0.5))
  y
}

## plot the empirical logits against log-dose

log.dose  <- log(budworm$dose)
emp.logit <- empirical.logit(budworm$ndead,budworm$ntotal)
plot(log.dose,emp.logit,type='n',xlab='log-dose',ylab='emprirical logit')
title('budworm: emprirical logits of probability to die ')

male   <- budworm$sex=='male'
female <- budworm$sex=='female'
lines(log.dose[male],emp.logit[male],type='b',lty=1,col=1)
lines(log.dose[female],emp.logit[female],type='b',lty=2,col=2)
legend(0.5,2,legend=c('male','female'),lty=c(1,2),col=c(1,2))



cleanEx()
nameEx("DATA-codstom")
### * DATA-codstom

flush(stderr()); flush(stdout())

### Name: codstom
### Title: Diet of Atlantic cod in the Gulf of St. Lawrence (Canada)
### Aliases: codstom
### Keywords: datasets

### ** Examples

data(codstom)
str(codstom)
# removes multiple occurences of same prey.type in stomachs
codstom1 <- summaryBy(prey.mass ~ 
                      region+ship.type+ship.id+trip+set+fish.id+prey.type,
                      data = codstom, id = ~fish.length, 
                      keep.names=TRUE, FUN = sum) 

# keeps a single line per stomach with the total mass of stomach content
codstom2 <- summaryBy(prey.mass ~ region+ship.type+ship.id+trip+set+fish.id,
                      data = codstom, id = ~fish.length, 
                      keep.names=TRUE, FUN = sum) 

# mean prey mass per stomach for each trip
codstom3 <- summaryBy(prey.mass ~ region+ship.type+ship.id+trip,
                      data = codstom2, keep.names=TRUE, FUN = mean) 

## Not run: 
##D           
##D # wide version, one line per stomach, one column per prey type
##D library(reshape)
##D codstom4 <- melt(codstom, id = c(1:7, 9))
##D codstom5 <- cast(codstom4, 
##D                  region+ship.type+ship.id+trip+set+fish.id+fish.length ~ 
##D                  prey.type, sum)
##D k <- length(names(codstom5))
##D prey_col <- 8:k
##D out <- codstom5[,prey_col]
##D out[is.na(out)] <- 0
##D codstom5[,prey_col] <- out
##D codstom5$total.content <- rowSums(codstom5[, prey_col])
## End(Not run)



cleanEx()
nameEx("DATA-dietox")
### * DATA-dietox

flush(stderr()); flush(stdout())

### Name: dietox
### Title: Growth curves of pigs in a 3x3 factorial experiment
### Aliases: dietox
### Keywords: datasets

### ** Examples

data(dietox)
str(dietox) ;
plot(dietox)




cleanEx()
nameEx("DATA-milkman")
### * DATA-milkman

flush(stderr()); flush(stdout())

### Name: milkman
### Title: Milk yield data for manually milked cows.
### Aliases: milkman
### Keywords: datasets

### ** Examples

data(milkman)
## maybe str(milkman) ; plot(milkman) ...



cleanEx()
nameEx("Rmarkup")
### * Rmarkup

flush(stderr()); flush(stdout())

### Name: Rmarkup
### Title: Automatic Generation of Reports (as HTML documents)
### Aliases: Rmarkup
### Keywords: utilities

### ** Examples

tf <- system.file("HTMLreport", "PuromycinAnalysis-report.R",
   package = "doBy")

## Create report in working directory
Rmarkup(tf)
## Creates report in specified directory (which must exist).
## Rmarkup(tf, path=".REPORT/")




cleanEx()
nameEx("descStat")
### * descStat

flush(stderr()); flush(stdout())

### Name: descStat
### Title: Computing simple descriptive statistics of a numeric vector.
### Aliases: descStat
### Keywords: utilities

### ** Examples

x <- c(1,2,3,4,NA,NaN)
descStat(x)



cleanEx()
nameEx("doBy")
### * doBy

flush(stderr()); flush(stdout())

### Name: doBy
### Title: Various utilities which includes functions for creating
###   groupwise calculations etc.
### Aliases: doBy
### Keywords: utilities

### ** Examples


data(dietox)

summaryBy(Weight+Feed~Evit+Cu+Time,      data=dietox, FUN=c(mean,var),
na.rm=TRUE, use="pair")  

orderBy(~Time+Evit, data=dietox)

splitBy(formula = ~Evit+Cu, data = dietox)

sampleBy(formula = ~Evit+Cu, frac=.1, data = dietox)




cleanEx()
nameEx("dose.LD50")
### * dose.LD50

flush(stderr()); flush(stdout())

### Name: dose.LD50
### Title: Calculate LD50
### Aliases: dose.LD50
### Keywords: models

### ** Examples

data(budworm)
m1 <- glm(ndead/20 ~ sex + log(dose), data=budworm, weight=ntotal, family=binomial)
coef(m1)

dose.LD50(m1,c(1,1,NA))
dose.LD50(m1,c(1,0,NA))



cleanEx()
nameEx("esticon")
### * esticon

flush(stderr()); flush(stdout())

### Name: esticon
### Title: Contrasts for lm, glm, lme, and geeglm objects
### Aliases: esticon esticon.geeglm esticon.glm esticon.gls esticon.lm
###   esticon.lme esticon.mer esticon.coxph
### Keywords: utilities

### ** Examples

data(iris)
lm1  <- lm(Sepal.Length~Sepal.Width+Species+Sepal.Width:Species, data=iris)
## Note that the setosa parameters are set to zero
coef(lm1)

## Estimate the intercept for versicolor
lambda1 <- c(1,0,1,0,0,0)
esticon(lm1,lambda1)

## Estimate the difference between versicolor and virgica intercept
## and test if the difference is 1
lambda2 <- c(0,1,-1,0,0,0)
esticon(lm1,lambda2,beta0=1)

## Do both estimates at one time
esticon(lm1,rbind(lambda1,lambda2),beta0=c(0,1))

## Make a combined test for that the difference between versicolor and virgica intercept
## and difference between versicolor and virginica slope is zero:
lambda3 <- c(0,0,0,0,1,-1)
esticon(lm1,rbind(lambda2,lambda3),joint.test=TRUE)

# Example using esticon on coxph objects (thanks to Alessandro A. Leidi).
# Using dataset 'veteran' in the survival package  
# from the Veterans' Administration Lung Cancer study 

library(survival);
data(veteran)
sapply(veteran,class)
levels(veteran$celltype)
attach(veteran)
veteran.s<-Surv(time,status)
coxmod<-coxph(veteran.s~age+celltype+trt,method='breslow')
summary(coxmod)

# compare a subject 50 years old with celltype 1 
# to a subject 70 years old with celltype 2 
# both subjects on the same treatment 
AvB<-c(-20,-1,0,0,0)

# compare a subject 40 years old with celltype 2 on treat=0
# to a subject 35 years old with celltype 3 on treat=1
CvB<-c(5,1,-1,0,-1)

esti<-esticon(coxmod,rbind(AvB,CvB))
esti
exp(esti[,c(2,7,8)])







cleanEx()
nameEx("firstlastobs")
### * firstlastobs

flush(stderr()); flush(stdout())

### Name: firstlastobs
### Title: Locate the index of the first/last unique value
### Aliases: firstobs lastobs firstobs.default lastobs.default
###   firstobs.formula lastobs.formula
### Keywords: utilities

### ** Examples


x <- c(rep(1,5),rep(2,3),rep(3,7),rep(1,4))

firstobs(x)
lastobs(x)

data(dietox)

firstobs(~Pig, data=dietox)
lastobs(~Pig, data=dietox)





cleanEx()
nameEx("lapplyBy")
### * lapplyBy

flush(stderr()); flush(stdout())

### Name: lapplyBy
### Title: Formula based version of lapply
### Aliases: lapplyBy
### Keywords: utilities

### ** Examples

data(dietox)

## Calculate weekwise feed efficiency = weight gain / feed intake
dietox <- orderBy(~Pig+Time, data=dietox)
v<-lapplyBy(~Pig, data=dietox, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)

## Technically this is the same as 
dietox <- orderBy(~Pig+Time, data=dietox)
wdata <- splitBy(~Pig, data=dietox)
v <- lapply(wdata, function(d) c(NA, diff(d$Weight)/diff(d$Feed)))
dietox$FE <- unlist(v)



cleanEx()
nameEx("lmBy")
### * lmBy

flush(stderr()); flush(stdout())

### Name: lmBy
### Title: List of lm objects with a common model
### Aliases: lmBy coef.lmBy fitted.lmBy residuals.lmBy print.lmBy getBy
### Keywords: models

### ** Examples

## To be written...



cleanEx()
nameEx("orderBy")
### * orderBy

flush(stderr()); flush(stdout())

### Name: orderBy
### Title: Ordering (sorting) rows of a data frame
### Aliases: orderBy
### Keywords: utilities

### ** Examples

data(dietox)
orderBy(~Time+Evit, data=dietox)
## Sort decreasingly by Time
orderBy(~-Time+Evit, data=dietox)



cleanEx()
nameEx("popMeans")
### * popMeans

flush(stderr()); flush(stdout())

### Name: popMeans
### Title: Calculate population means (LSMEANS in SAS jargon)
### Aliases: popMeans popMeans.default popMeans.lme summary.conMeans
### Keywords: models utilites

### ** Examples

dat <- expand.grid(list(AA=factor(1:2), BB=factor(1:3), CC=factor(1:3)))
dat$y <- rnorm(nrow(dat))
dat$x <- rnorm(nrow(dat))
dat$x2 <- dat$x^2

## Examples

## 1) LSMEANS with factors only.
mod1 <- lm(y ~ AA + BB*CC + x, data=dat)
## Average over AA for each combination of (BB,CC); evaluate at x=mean(x)
popMeans(mod1, c("BB","CC"))
## Average over (AA,BB) for each value of CC; evaluate at x=mean(x)
popMeans(mod1, c("CC"))

## 2) The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##  lsmeans CC BB*CC / stderr;
## run;
##
mod2 <- lm(y ~ AA + BB*CC + x + x2, data=dat)
popMeans(mod2, "CC")

## Notice the difference to:
mod3 <- lm(y ~ AA + BB*CC + x + I(x^2), data=dat)
popMeans(mod3, "CC")

## The difference arises because in the former case, x2 is evaluated at mean(x2) whereas
## in the latter case x is evaluated at mean(x)^2

## 3) Plug in particular values of covariates
## The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x2;
##  lsmeans CC BB*CC / at x=2 stderr;
## run;
popMeans(mod2, c("CC"), at=list(x=2))
## Above, x=2 is used while x2 is set to mean(x2)

## The call to popMeans() below is equivalent to the following SAS code
## proc glm data=dat;
##	class AA BB CC;
##	model y = AA BB|CC x x*x;
##    lsmeans CC BB*CC / at x=2 stderr;
## run;
##
popMeans(mod3, c("CC"), at=list(x=2))
## Above, x=2 is used while I(x^2) is set to mean(x^2)=4. Notice that setting
## popMeans(mod3, c("CC"), at=list(x=2,"I(x^2)"=123))
## has no effect: I(x^2) is still 4 because x=2. Hence the following two results
## are identical

popMeans(mod2, c("CC"), at=list(x=2, x2=4))
popMeans(mod3, c("CC"), at=list(x=2))

## END 



cleanEx()
nameEx("recodeVar")
### * recodeVar

flush(stderr()); flush(stdout())

### Name: recodeVar
### Title: Recode values of a vector
### Aliases: recodeVar
### Keywords: utilities

### ** Examples

x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)
#[1] "winter" "winter" "winter" "spring" "spring" "spring"

x <- c(rep(1:3,3))
#[1] 1 2 3 1 2 3 1 2 3

## Simple usage:
recodeVar(x, src=c(1,2), tgt=c("A","B"))
#[1] "A" "B" NA  "A" "B" NA  "A" "B" NA 

## Here we need to use lists
recodeVar(x, src=list(c(1,2)), tgt=list("A"))
#[1] "A" "A" NA  "A" "A" NA  "A" "A" NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L")
#[1] "A" "A" "L" "A" "A" "L" "A" "A" "L"
recodeVar(x, src=list(c(1,2),3), tgt=list("A","B"), default="L")
#[1] "A" "A" "B" "A" "A" "B" "A" "A" "B"

## Dealing with NA's in x
x<-c(NA,rep(1:3,3),NA)
#[1] NA  1  2  3  1  2  3  1  2  3 NA
recodeVar(x, src=list(c(1,2)), tgt=list("A"))
#[1] NA  "A" "A" NA  "A" "A" NA  "A" "A" NA  NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L")
#[1] NA  "A" "A" "L" "A" "A" "L" "A" "A" "L" NA 
recodeVar(x, src=list(c(1,2)), tgt=list("A"), default="L", keep.na=FALSE)
#[1] "L" "A" "A" "L" "A" "A" "L" "A" "A" "L" "L"





cleanEx()
nameEx("renameCol")
### * renameCol

flush(stderr()); flush(stdout())

### Name: renameCol
### Title: Rename columns in a matrix or a dataframe.
### Aliases: renameCol
### Keywords: utitlities

### ** Examples


renameCol(CO2, 1:2, c("kk","ll"))
renameCol(CO2, c("Plant","Type"), c("kk","ll"))

# These fail - as they should:
# renameCol(CO2, c("Plant","Type","conc"), c("kk","ll"))
# renameCol(CO2, c("Plant","Type","Plant"), c("kk","ll"))



cleanEx()
nameEx("sampleBy")
### * sampleBy

flush(stderr()); flush(stdout())

### Name: sampleBy
### Title: Sampling from a data frame
### Aliases: sampleBy
### Keywords: utilities

### ** Examples

data(dietox)
sampleBy(formula = ~Evit+Cu, frac=.1, data = dietox)



cleanEx()
nameEx("scaleBy")
### * scaleBy

flush(stderr()); flush(stdout())

### Name: scaleBy
### Title: Groupwise scaling and centering of numeric columns in a
###   dataframe
### Aliases: scaleBy
### Keywords: utilities

### ** Examples


data(dietox)

# "Remove" the effect of time by centering data within each time point.
dietox2 <- scaleBy(Weight~Time, data=dietox, scale=FALSE)

## Not run: 
##D library(lattice)
##D xyplot(Weight~Time|Evit+Cu, groups=Pig, data=dietox)
##D xyplot(Weight~Time|Evit+Cu, groups=Pig, data=dietox2)
## End(Not run)




cleanEx()
nameEx("splitBy")
### * splitBy

flush(stderr()); flush(stdout())

### Name: splitBy
### Title: Split a data frame
### Aliases: splitBy print.splitByData
### Keywords: utilities

### ** Examples

data(dietox)
splitBy(formula = ~Evit+Cu, data = dietox)



cleanEx()
nameEx("subSeq")
### * subSeq

flush(stderr()); flush(stdout())

### Name: subSeq
### Title: Find sub-sequences of identical elements in a vector.
### Aliases: subSeq
### Keywords: utilities

### ** Examples

x <- c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3)
(ans <- subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item=1)
subSeq(x,item="1")

x <- as.character(c(1,1,1,0,0,1,1,1,2,2,2,1,2,2,2,3))
(ans<-subSeq(x))
ans$value
# Notice: Same results below
subSeq(x,item="1")
subSeq(x,item=1)



cleanEx()
nameEx("subsetBy")
### * subsetBy

flush(stderr()); flush(stdout())

### Name: subsetBy
### Title: Finds subsets of a dataframe which is split by variables in a
###   formula.
### Aliases: subsetBy
### Keywords: utilities

### ** Examples

data(dietox)
subsetBy(~Evit, Weight < mean(Weight), data=dietox)



cleanEx()
nameEx("summaryBy")
### * summaryBy

flush(stderr()); flush(stdout())

### Name: summaryBy
### Title: Function to calculate groupwise summary statistics
### Aliases: summaryBy
### Keywords: univar

### ** Examples


data(dietox)
dietox12    <- subset(dietox,Time==12)

summaryBy(Weight+Feed~Evit+Cu,      data=dietox12,
   FUN=c(mean,var,length))  

summaryBy(Weight+Feed~Evit+Cu+Time, data=subset(dietox,Time>1),
   FUN=c(mean,var,length))  

## Calculations on transformed data:

summaryBy(log(Weight)+Feed~Evit+Cu, data=dietox12)  

## Calculations on all numerical variables (not mentioned elsewhere): 

summaryBy(.~Evit+Cu,                data=dietox12,
   id=~Litter, FUN=mean)

## There are missing values in the 'airquality' data, so we remove these
## before calculating mean and variance with 'na.rm=TRUE'. However the
## length function does not accept any such argument. Hence we get
## around this by defining our own summary function in which length is
## not supplied with this argument while mean and var are:

sumfun <- function(x, ...){
  c(m=mean(x, ...), v=var(x, ...), l=length(x))
}
summaryBy(Ozone+Solar.R~Month, data=airquality, FUN=sumfun, na.rm=TRUE)

## Using '.' on the right hand side of a formula means to stratify by
## all variables not used elsewhere:

data(warpbreaks)
summaryBy(breaks ~ wool+tension, warpbreaks)
summaryBy(breaks ~., warpbreaks)
summaryBy(.~ wool+tension, warpbreaks)

## Keep the names of the variables (works only if FUN only returns one
## value):

summaryBy(Ozone+Wind~Month, data=airquality,FUN=c(mean),na.rm=TRUE,
  keep.names=TRUE)

## Using full.dimension=TRUE

## Consider:
summaryBy(breaks~wool, data=warpbreaks)
## Rows of result are replicated below
summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE)
## Notice: Previous result is effectively the same as
with(warpbreaks, ave(breaks, wool))
## A possible application of full.dimension=TRUE is if we want to 
## standardize (center and scale) data within groups:
ss <- summaryBy(breaks~wool, data=warpbreaks, full.dimension=TRUE, FUN=c(mean,sd))
(warpbreaks$breaks-ss$breaks.mean)/ss$breaks.sd




cleanEx()
nameEx("timeSinceEvent")
### * timeSinceEvent

flush(stderr()); flush(stdout())

### Name: timeSinceEvent
### Title: Calculate "time since event" in a vector.
### Aliases: timeSinceEvent
### Keywords: utilities

### ** Examples

## Events:
yvar <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0)

## Plot results:
tse<- timeSinceEvent(yvar)
plot(sign.tse~tvar, data=tse, type="b")
grid()
rug(tse$tvar[tse$yvar==1], col=4,lwd=4)
points(scale(tse$run), col=tse$run,lwd=2)
lines(abs.tse+.2~tvar, data=tse, type="b",col=3)

## Find times for which time since an event is at most 1:
tse$tvar[tse$abs<=1]

yvar <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)
tvar <- c(207, 208, 208, 208, 209, 209, 209, 209, 210, 210, 211, 211, 
211, 212, 213, 213, 214, 214, 215, 216, 216, 216, 216, 217, 217, 
217, 218, 218, 219, 219, 219, 219, 220, 220, 221, 221, 221, 221, 
222, 222, 222)

timeSinceEvent(yvar, tvar)





cleanEx()
nameEx("transformBy")
### * transformBy

flush(stderr()); flush(stdout())

### Name: transformBy
### Title: Function to make groupwise transformations
### Aliases: transformBy
### Keywords: univar

### ** Examples


data(dietox)
transformBy(~Pig, data=dietox, minW=min(Weight), maxW=max(Weight), 
    gain=sum(range(Weight)*c(-1,1)))




cleanEx()
nameEx("which.maxn")
### * which.maxn

flush(stderr()); flush(stdout())

### Name: which.maxn
### Title: Where are the n largest or n smallest elements in a numeric
###   vector ?
### Aliases: which.maxn which.minn
### Keywords: utilities

### ** Examples

x <- c(1:4,0:5,11,NA,NA)
ii <- which.minn(x,5)

x <- c(1,rep(NA,10),2)
ii <- which.minn(x,5)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
