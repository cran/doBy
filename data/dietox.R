dietox <- read.table("dietox.txt",header=TRUE)
dietox$Evit <- factor(dietox$Evit);
dietox$Cu <- factor(dietox$Cu);
dietox$Litter <- factor(dietox$Litter)
dietox$Pig <- factor(dietox$Pig)
levels(dietox$Evit) <- c("Evit000","Evit100","Evit200")
levels(dietox$Cu) <- c("Cu000","Cu035","Cu175")
