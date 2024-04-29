## ----include = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options("digits"=3)
library(doBy)
library(boot)
#devtools::load_all()

## ---------------------------------------------------------------------------------------
dat0 <- mtcars[1:6,]
dat0

## ---------------------------------------------------------------------------------------
generate_data_list(dat0, K=3, method="subgroups")
generate_data_list(dat0, K=3, method="resample")


## ---------------------------------------------------------------------------------------
set.seed(1411)
dat <- personality
train <- sample(1:nrow(dat), 0.6*nrow(dat))
dat.training <- dat[train, ]
dat.testing <- dat[-train, ]

## ---------------------------------------------------------------------------------------
mod1 <- glm(agreebl ~ ., data = dat.training)

## ---------------------------------------------------------------------------------------
set.seed(1411)
n.searches <- 12
mod_stab <- model_stability_glm(dat.training, mod1,
                           n.searches=n.searches, method="subgroups", trace=0)
mod_stab
summary(mod_stab)

## ---------------------------------------------------------------------------------------
formula(mod_stab, fit=FALSE) [1:2]

## ---------------------------------------------------------------------------------------
fit_list <- formula(mod_stab, fit=TRUE)

set.seed(1411)
cv.error <- cv_glm_fitlist(dat.training, fit_list, K=4)
cv.error


cv3 <- sapply(fit_list, 
       function(fit) {
         x <- update(fit, data=dat.training)
         modelr::rmse(x, dat.testing)
       }
)
cv3

plot(cv.error, cv3)

## ---------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(sqrt(cv.error), main="Subgroups")
plot(cv3, main="Subgroups")

