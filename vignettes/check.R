n.vec  <- c(4, 8, 16, 32, 64)
fn.list <- lapply(n.vec,
                  function(ni){
                      restrict_fun(inv_toeplitz, list(n=ni))}
                  )


bquote_list <- function(fnlist, names=NULL){
    out <- lapply(fnlist, function(g) bquote(.(g)()))
    if (!is.null(names))
        names(out) <- names
    out
}

fn.list[[1]]
fn.list[[1]]()

fn.list[[1]]  %>% get_fun
fn.list[[1]]  %>% get_restrictions

make_scaffold_list ()


fn.list <- list(function(x){x+1}, function(x){x+2})

sapply(fn.list, eval)

zz <- dobq(fn.list)

sapply(zz, function(z) eval(z, list(x=10)))




library(magrittr)
library(broom)
library(ggplot2)
library(doBy)
library(patchwork)

data(CO2)

ggplot(CO2, aes(x=(conc), y=(uptake), color=Treatment, group=Plant)) +
    geom_line() +
    geom_point() +
    facet_grid(~Type)

CO2 <- subset(CO2, conc < 300) ## OK
names(CO2)[1:3] <- c("plant", "type", "treat")
CO2.bal <- transform(CO2,
                     type = abbreviate(CO2$type, 3),
                     treat = abbreviate(CO2$treat, 4)
                     )
rownames(CO2.bal) <- NULL
CO2.ubal <- CO2.bal[-c(1, 5, 6, 9, 12, 16, 19, 20, 23, 28, 34),]


p1 <- ggplot(CO2.bal, aes(x=conc, y=uptake, color=treat, group=plant)) +
    geom_line() + geom_point() + facet_grid(~type)

p2 <- ggplot(CO2.ubal, aes(x=conc, y=uptake, color=treat, group=plant)) +
    geom_line() + geom_point() + facet_grid(~type)

p1 / p2


form.add <- uptake ~ conc + treat + type
fm.bal  <- lm(form.add, data=CO2.bal)
fm.ubal <- lm(form.add, data=CO2.ubal)

LSmeans(fm.bal, effect=~1)
LSmeans(fm.ubal, effect=~1)

CO2.bal$uptake %>% mean
CO2.ubal$uptake %>% mean

LE_matrix(fm.bal, effect=NULL)
LE_matrix(fm.bal, effect=~1)
LE_matrix(fm.bal, effect=character(0))
LE_matrix(fm.bal, effect="")


LE_matrix(fm.bal, effect=~type)
LSmeans(fm.bal, effect=~type)

LE_matrix(fm.bal, effect=~type+treat)
LSmeans(fm.bal, effect=~type+treat)




LE_matrix(fm.bal, effect="type")
LSmeans(fm.bal, effect="type")


LE_matrix(fm.bal2, effect=c("type", "treat"))
LSmeans(fm.bal, effect=c("type", "treat"))

f2 <- uptake ~ conc + treat + type + conc:type
fm.bal2 <- lm(f2, data=CO2)













rhs <- ~(conc + treat + type)^2
rhs2 <- reformulate(labels(terms(rhs)))

f <- as.formula(paste("uptake", paste0(rhs2, collapse=" ")))



lm(f, data=CO2)  %>% tidy

lm(f, data=CO2)  %>% anova

ff <- lm(f, data=CO2)

gg <- ff %>% MASS::stepAIC(direction = "both", k=6)

f <- y~(A+B+C)^2


ggplot(CO2, aes(x=1/(conc), y=log(uptake), color=treat, group=Plant)) +
    geom_line() +
    geom_point() +
    facet_grid(~type)

ggplot(CO2, aes(x=1/(conc), y=log(uptake), color=type, group=Plant)) +
    geom_line() +
    geom_point() +
    facet_grid(~treat)
