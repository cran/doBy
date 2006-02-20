prompt(plot.by)
prompt(hist.by)
prompt(qqnorm.by)
prompt(summary.by)
prompt(doby.xtabs)

#levels(dietox$Cu)     <- c("low", "medium", "high")
#levels(dietox$Evit)   <- c("low", "medium", "high")
#write.table(dietox                         ,file="data\\dietox.txt")    

path <- "D:\\Stat\\RDEVEL\\dobyDevel\\doby\\"
src <- function(){ source(paste(path,"R\\doby.R",sep=''))}
src()
plot.by(thickness, strength, subject=crop, data=st, col=1:4, fun="subject.f",lwd=2, silent=F, lty=1:400)






source(paste(path,"demo\\doby.R",sep=''))
    
dietox        <- read.table("data\\DietOx.txt", header=T, na.strings="*")
dietox$Cu     <- as.factor(dietox$Cu)
dietox$Evit   <- as.factor(dietox$Evit)

source("D:\\Stat\\RDEVEL\\dobyDevel\\doby\\R\\doby.R")

install.packages("D:/Stat/RDEVEL/dobyDEVEL/doby_1.000.zip", .libPaths()[1], CRAN = NULL)
library(doby)


remove.packages("doby", .libPaths()[1])
demo(plot.by)

remove.packages("", .libPaths()[1])

source("doby\\R\\doby.R")
plot.by(Time,log(Weight),subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", data=dietox,group.pch=1:10, group.col=1:10)


##
##  Grouped of weight against time
##

data(dietox)
par(mfrow=c(3,3),omi=c(0,0,.5,0))
source("doby\\R\\doby.R")
plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=", data=dietox,group.pch=1:10, group.col=1:10)

mtext("Weight against Time",outer=TRUE)

source("R\\doby.R")
par(mfrow=c(3,3),omi=c(0,0,.5,0))
plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=T,group.col=1:30,pch=1:30, data=dietox)


par(mfrow=c(3,3),omi=c(0,0,.5,0))
source("R\\doby.R")

plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=T,group.col=1:30,pch=1:30, 
    data=dietox, lwd=1, xlab="KKKKK", ylab="llll", lty =4)


plot.by(Time,Weight, group=c(Evit,Cu),title="Evit*Cu=",lines=T,group.col=1:30,pch=1:30,data=dietox)

source("R\\doby.R")
plot.by(Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu=",lines=T,col=3, data=dietox)

par(mfrow=c(1,1))
plot.by(Weight,subject=Pig,title="Plot of all pigs",lines=T,col=1:300, data=dietox)


subject.f   <- function(){
    r<-spline(subject.x,subject.y);
    lines(r$x,r$y,col=cur.col,lty="dashed")
}
group.f        <- function(){
    s<-lm(group.y~group.x+I(group.x^2)); 
    print(s$coef)
    print(group.id)
    abline(s,lwd=3,col=(1:10)[group.id]);
    print(mean(group.y))
}

source("doby\\R\\doby.R")
par(mfrow=c(3,3),omi=c(0,0,0.5,0))
plot.by(Time,Weight,subject=Pig,group=c(Evit,Cu),title="Evit*Cu =", 
col=1:30,lines=F, lwd=2, group.fun="group.f",data=dietox ,silent=F)


fun="subject.f"
mtext("Weight against Time with mean curve",outer=TRUE)


##
## Grouped QQ-plots
##
dietox12    <- subset(dietox,dietox$Time==12)
par(mfrow=c(3,3),omi=c(0,0,.5,0))
qqnorm.by(Weight, group=c(Evit,Cu), data=dietox12)
mtext("QQ-plots for Weight in week 12",outer=T)

##
## Grouped histograms
##

dietox12    <- subset(dietox,dietox$Time==12)

par(mfrow=c(3,3),omi=c(0,0,.5,0))
hist.by(Weight, group=c(Evit,Cu),group.col=rep(2:4,3), data=dietox12)
mtext("Histograms for Weight in week 12",outer=T)

hist.by(Weight, group=c(Evit,Cu),group.col=rep(2:4,3), data=dietox12, smooth=15)
mtext("Smoothed histograms for Weight in week 12",outer=T)


##
## Grouped summary statistics
##

s2 <- summary.by(cbind(Weight,Feed)~Evit+Cu, data=dietox12, FUN=list(mean,var))  
s2 <- summary.by(cbind(Weight,Feed)~Evit+Cu+Time, data=dietox, FUN=list(mean,var))  

plot(log(s2$mean.Weight),log(s2$var.Weight))
abline(lm(log(s2$var.Weight) ~ log(s2$mean.Weight)))

s1 <- summary.by(Weight~Evit+Cu,data=dietox12,FUN=mean)
plot.by(Evit,mean.Weight, subject=Cu,   lines=T, pch=1:3, data=s1)
plot.by(Cu,  mean.Weight, subject=Evit, lines=T, pch=1:3, data=s1)


src()
s2 <- power.by(cbind(Weight,Feed)~Evit+Cu, data=dietox12)  



source("doby\\R\\doby.R")




q
