set.seed(12)

######################################################
# Building and Pruning a Tree
######################################################

install.packages("tree")   # install tree package
library(tree)   # load tree package
?tree

# Classification Trees
Heart=read.csv("Heart.csv")   # load dataset
Heart=na.omit(Heart)   # removes observations with NAs
Heart$X=NULL   # remove redundant variable

classtree=tree(AHD~.,data=Heart)   # build tree
summary(classtree)   # summary of the tree 
classtree   # more detailed summary

plot(classtree)   # display tree
text(classtree,pretty=0)   # display split labels

preds=predict(classtree,newdata=Heart,type="class")   # predictions
table(observed=Heart$AHD,predicted=preds)   # misclassification rate

?cv.tree
cv_classtree=cv.tree(classtree,K=10,FUN=prune.misclass)   # perform CV
cv_classtree   # summary

plot(cv_classtree$size,cv_classtree$dev,type="b",
     xlab="Tree size",ylab="CV error")   # plot CV errors

?prune.tree
pruned_classtree=prune.tree(classtree,best=8,method="misclass")   # prune original tree
summary(pruned_classtree)   # summary of pruned tree
pruned_classtree   # more detailed summary of pruned tree

plot(pruned_classtree)   # display tree
text(pruned_classtree,pretty=0)   # display split labels

preds=predict(pruned_classtree,newdata=Heart,type="class")   # predictions from pruned tree
table(observed=Heart$AHD,predicted=preds)   # misclassification rate for pruned tree
(24+15)/297


# Regression Trees
library(MASS)
data("Boston")   # load dataset
?Boston

regtree=tree(medv~.,data=Boston)   # build tree
summary(regtree)   # summary of the tree
regtree   # more detailed summary

plot(regtree)   # display tree
text(regtree,pretty=0)   # display split labels

preds=predict(regtree,newdata=Boston)   # predictions
sum((Boston$medv-preds)^2)/(nrow(Boston)-9)   # residual mean deviance

?cv.tree
cv_regtree=cv.tree(regtree,K=10)   # perform CV
cv_regtree   # summary

plot(cv_regtree$size,cv_regtree$dev,type="b",
     xlab="Tree size",ylab="CV error")   # plot CV errors

?prune.tree
pruned_regtree=prune.tree(regtree,best=9,method="deviance")   # prune original tree
summary(pruned_regtree)   # summary of pruned tree
pruned_regtree   # more detailed summary of pruned tree

plot(pruned_regtree)   # display tree
text(pruned_regtree,pretty=0)   # display split labels


######################################################
# Bagging and Random Forests
######################################################

install.packages("randomForest")   # install randomForest package
library(randomForest)   # load randomForest package
?randomForest

# Bagging
bag=randomForest(medv~.,data=Boston,mtry=13,importance=TRUE)
bag   # summary
mean((Boston$medv-bag$predicted)^2)   # OOB MSE
bag$oob.times   # number of times each data point is OOB

# finding optimal m based on OOB error
oob_err=double(13)
oob_err

for(m in 1:13)
{
  fit=randomForest(medv~.,data=Boston,mtry=m,ntree=500)   # fit for each m
  oob_err[m]=fit$mse[500]   # OOB error for each m
  cat(m," ")
}

plot(1:13,oob_err,pch=19,col="red",type="b",ylab="MSE",xlab="m")

# Random Forest 
rf=randomForest(medv~.,data=Boston,mtry=4,importance=TRUE)
rf   # summary
mean((Boston$medv-rf$predicted)^2)   # OOB MSE

# Variable Improtance Measures
?importance
importance(rf)   # measures of importance of each variable
varImpPlot(rf)   # plot importance measures

######################################################
# Boosting
######################################################

install.packages("gbm")   # install gbm package
library(gbm)   # load gbm package
?gbm

boost=gbm(medv~.,data=Boston,distribution="gaussian",
          n.trees=5000,shrinkage=0.001,interaction.depth=2)
summary(boost)

preds=predict(boost,newdata=Boston,n.trees=5000,type="response")   # predictions
mean((Boston$medv-preds)^2)

