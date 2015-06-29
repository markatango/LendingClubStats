library(caret)
library(randomForest)

# read in the data
sphd.dd <- read.csv("sphd.csv")

# select only observations that did not ever go non-current or worse.
# These are candidates for trade on the secondary market
sphd.dd$label <- as.character(sphd.dd$label)
sphd.dd <- sphd.dd[which(sphd.dd$label != "noNCS"),]
sphd.dd$label <- as.factor(sphd.dd$label)
sphd.dd <- sphd.dd[,-which(names(sphd.dd) == "chargedOff")]

# partition in training and test sets
u <- createDataPartition(sphd.dd$label,list=FALSE)
train.raw <- sphd.dd[u,]
test.raw <- sphd.dd[-u,]
rm(u)

# pre process train set predictors (no labels)
numCols <- sapply(train.raw,is.numeric)
train.n <- train.raw[,numCols]
train.f <- train.raw[,!numCols]
train.f <- train.f[, -which(names(train.f) =="label")]

# construct pre processor for numeric data and re-map data to independent axes
pp1 <- preProcess(train.n, method=c("pca", "center", "scale"), threshold=0.95)
train.n.p <- predict(pp1, train.n)

# add back the factors and labels
train <- cbind(train.f, train.n.p, label=train.raw$label)
#rm(train.raw, train.n, train.f, train.n.p)

# apply preProcessors to test data
test.n <- test.raw[,numCols]
test.f <- test.raw[,!numCols]
test.f <- test.f[, -which(names(test.f) == "label")]

test.n.p <- predict(pp1, test.n)
test <- cbind(test.f, test.n.p, label=test.raw$label)
rm(test.raw, test.n, test.f, test.n.p)


# k nearest neighbors


a <- c()
ind <- 1:pp1$numComp
for(i in ind) {
  fit <- knn3(label~. ,train,k=i)
  yhat <- predict(fit,test,type="class")
  cm <- confusionMatrix(yhat,test$label)
  a <- c(a,cm$overall["Accuracy"])
}

plot(ind,a)

fit <- knn3(label~.,train,k=which(a==max(a)))
yhat <- predict(fit,test,type="class")
cm <- confusionMatrix(yhat,test$label)
cm$table
cm$overall["Accuracy"]

a <- c()

for(x in 1:pp1$numComp){
  theFormula <- as.formula(paste("label~",paste("PC",1:x,sep="",collapse="+")))
  fit <- qda(theFormula, train)
  yhat <- predict(fit,test,type="class")
  cm <- confusionMatrix(yhat$class,test$label)
  #cm$table
  cm$overall["Accuracy"]
  a <- c(a, cm$overall["Accuracy"])
}

plot(a)   

theFormula <- as.formula(paste("label~",paste("PC",1:which(a==max(a))[1],sep="",collapse="+")))
fit <- qda(theFormula, train)
yhat <- predict(fit,test,type="class")
cm <- confusionMatrix(yhat$class,test$label)
cm$table
cm$overall["Accuracy"]

a <- c()

for(x in 1:pp1$numComp){
  theFormula <- as.formula(paste("label~",paste("PC",1:x,sep="",collapse="+")))
  fit <- lda(theFormula, train)
  yhat <- predict(fit,test,type="class")
  cm <- confusionMatrix(yhat$class,test$label)
  #cm$table
  cm$overall["Accuracy"]
  a <- c(a, cm$overall["Accuracy"])
}

plot(a)   

theFormula <- as.formula(paste("label~",paste("PC",1:which(a==max(a)),sep="",collapse="+")))
fit <- lda(theFormula, train)
yhat <- predict(fit,test,type="class")
cm <- confusionMatrix(yhat$class,test$label)
cm$table
cm$overall["Accuracy"]

fit <- train(label~., data=train)
yhat <- predict(fit,test)
cm <- confusionMatrix(yhat,test$label)
cm$table
cm$overall["Accuracy"]


uTrState <- unique(train$state)
uTeState <- unique(test$state)
uState <- setdiff(union(uTrState,uTeState),uTeState)

uTr <- which(train$state %in% uState)
train <- train[-uTr,]
uTe <- which(test$state %in% uState)
test <- test[-uTe,]



fit <- train(label~., data=train)
yhat <- predict(fit,test)
cm <- confusionMatrix(yhat,test$label)
cm$table
cm$overall["Accuracy"]


