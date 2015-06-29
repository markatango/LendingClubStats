library(foreach)
library(iterators)
library(reshape2)
library(ggplot2)
library(plyr)
library(MASS)
library(caret)


# read in the data
sphd.dd <- read.csv("sphd.csv")

# select only observations that did not ever go non-current or worse.
# These are candidates for trade on the secondary market
sphd.dd$label <- as.character(sphd.dd$label)
sphd.dd <- sphd.dd[which(sphd.dd$label != "noNCS"),]
sphd.dd$label <- as.factor(sphd.dd$label)
sphd.dd <- sphd.dd[,-which(names(sphd.dd) == "chargedOff")]
sphd.dd <- sphd.dd[,-which(names(sphd.dd) == "LOAN_ID")]

# partition in training and test sets
set.seed(1340)
u <- createDataPartition(sphd.dd$label,list=FALSE)
train.raw <- sphd.dd[u,]
test.raw <- sphd.dd[-u,]
rm(u)

# pre process train set predictors (no labels)
numCols <- sapply(train.raw,is.numeric)
train.n <- train.raw[,numCols]
train.f <- train.raw[,!numCols]
train.f <- train.f[, -which(names(train.f) =="label")]
train.lab <- train.raw$label

# construct pre processor for numeric data and re-map data to independent axes
pp1 <- preProcess(train.n, method=c("pca", "center", "scale"), threshold=0.95)
train.n.p <- predict(pp1, train.n)

# add back the factors and labels
train.NOpca <- cbind(train.f, train.n, label=train.raw$label)
train.pca <- cbind(train.f, train.n.p, label=train.raw$label)
#rm(train.raw, train.n, train.f, train.n.p)

#=====================================
# ignore the test data set for now.
#=====================================

pairs(train.n.p[,1:10], col=train.lab)
pairs(train.n[,1:10], col=train.lab)

# 1) study relation between number of non-current state n
g <- ggplot(train.NOpca, aes(x=label, y=noNCS))
g <- g + geom_point(aes(color=train.lab), position="jitter")
g

# 1a) show probability distribution of charge off based on number of non-current reports
# group by minFico score

cp.noNCS <- ddply(train.NOpca, .(noNCS), summarize,
                  numLoans = length(noNCS),
                  numCOs = sum(label=="co")*1
)

cp.noNCS <- cp.noNCS[order(cp.noNCS$noNCS),]
cp.noNCS$cp <- cp.noNCS$numCOs / cp.noNCS$numLoans

g <- ggplot(cp.noNCS, aes(x=noNCS, y=cp))
g <- g + geom_point(aes(size=numLoans))
g

cp.1.m <- melt(train.NOpca, 
               id.vars=c("label", "homeOwnership", "state", "grade", "earlyCreditLine"), 
               measure.vars=c("noNCS",grep("cum",names(train.NOpca), value=TRUE))
)

cp.1.c <- dcast(cp.1.m,homeOwnership+label~variable,)
# 2) why keep a loan with x non-currents
cp.2 <- melt(train.NOpca, 
             id.vars=c("label", "homeOwnership", "state", "grade", "earlyCreditLine"), 
             measure.vars=grep("cum",names(train.NOpca), value=TRUE)
)
cp.2.c <- dcast( cp.2, homeOwnership+label ~ variable, mean)
cp.2.c.m <- melt(cp.2.c, 
                 id.vars=c("label", "homeOwnership")
)
                 
g <- ggplot(cp.2.c.m, aes(x=variable, y=value)) + labs(title="Number of consecutive 'CURRENT' months vs. change in Fico Score")
g <- g + geom_point(aes(color=label))
g <- g + geom_smooth(method='loess',aes(group=label))
g <- g + scale_x_discrete(name="No. of contiguous months of 'CURRENT' status",
                          labels=1:23)
g <- g + scale_y_continuous(name="Cumulative change in Fico score")
g

g <- ggplot(cp.2.c.m,aes(group=variable))
g <- g + geom_density(aes(x=value, color=label))
g

# 3) density at 6 months
cp.3.m <- melt(train.NOpca,
               id.vars=c("label", "homeOwnership", "state", "grade", "earlyCreditLine"), 
               measure.vars=c("cumFico10")
)
g <- ggplot(cp.3.m, aes(x=value, group = state))
g <- g + geom_density(aes(color=label), width=3)
g <- g + scale_x_discrete(name="No. of contiguous months of 'CURRENT' status",
                          labels=cut)
g <- g + scale_y_continuous(name="Probability density")
g 

#sphd.pca <- as.data.frame(prcomp(sphd.n,scale=TRUE)$x)
pairs(sphd.pca[sample(1:5000,100),],col=sphd.dd$label)

sphd.m1 <- melt(sphd.pca, id.vars="label")

g <- ggplot(sphd.pca,aes(x=PC1,y=PC16))
g <- g + geom_point(aes(color=label))
g <- g + geom_boxplot()
g
                

# sphd.m1$value <- sapply(sphd.m1$value, function(x){
#   ifelse(x>125, 0, x)
# })


g <- ggplot(sphd.m1,aes(x=label, y=value))
g <- g + geom_boxplot()
g

sphd.m1 <- melt(sphd.dd, id.vars="label", 
                measure.vars="cumSlope12")

            
g1 <- ggplot(sphd.m1,aes(x=value,group=label))
g1 <- g1 + geom_density(aes(color=label))
g1

sphd.m1 <- melt(sphd.dd, id.vars="label", 
                measure.vars="minFicoScore")

g1 <- ggplot(sphd.m1,aes(x=label, y=value))
g1 <- g1 + geom_boxplot() + geom_point(aes(color=label), position = "jitter")
g1

g1 <- ggplot(sphd.m1,aes(x=value,group=label))
g1 <- g1 + geom_density(aes(color=label))
g1
