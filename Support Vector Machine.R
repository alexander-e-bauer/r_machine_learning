######################################################
# Support Vector Classifier
######################################################

set.seed(1)

# generate the dataset
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

dat=data.frame(x=x,y=as.factor(y))
summary(dat)

plot(x,col=(3-y))   # plot the dataset

install.packages("e1071")   # install package e1071
library(e1071)   # load package e1071
?svm

svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)   # fit svc

summary(svmfit)   # summary
svmfit$index   # index of the support vectors
plot(svmfit,dat)   # plot decision boundary

svmfit$fitted   # predictions on training data
preds=predict(svmfit,newdata=dat)   # predictions on training data
table(true=dat$y,predicted=svmfit$fitted)   # confusion matrix

svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)   # fit svc
plot(svmfit,dat)   # plot decision boundary
svmfit$index   # index of the support vectors

set.seed(1)
?tune
tunefit=tune(svm,y~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))   # perform 10-fold CV
summary(tunefit)   # summary

tunefit$best.parameters   # choice of best parameters
bestfit=tunefit$best.model   # model with best parameters

summary(bestfit)   # summary of best model
table(true=dat$y,predicted=bestfit$fitted)   # confusion matrix

######################################################
# Support Vector Machine (SVM) for Binary Classification
######################################################

set.seed(1)

# generate the dataset
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))

dat=data.frame(x=x,y=as.factor(y))
summary(dat)
plot(x,col=y)

svmfit=svm(y~.,data=dat,kernel="radial",gamma=1,cost=1)   # fit svm with radial kernel
summary(svmfit)   # summary
plot(svmfit,dat)   # plot decision boundary
table(true=dat$y,predicted=svmfit$fitted)   # confusion matrix

set.seed(1)
tunefit=tune(svm,y~.,data=dat,kernel="radial", 
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))   # perform 10-fold CV

tunefit$best.parameters   # choice of best parameters
bestfit=tunefit$best.model   # model with best parameters

summary(bestfit)   # summary of best model
table(true=dat$y,predicted=bestfit$fitted)   # confusion matrix

######################################################
# Support Vector Machine (SVM) for More than Two Classes
######################################################

library(ISLR)
data("Khan")   # load Khan dataset
names(Khan)

dim(Khan$xtrain)   # matrix of training features
dim(Khan$xtest)   # matrix of test features
length(Khan$ytrain)   # vector of training responses
length(Khan$ytest)   # vector of test responses
table(Khan$ytrain)   # frequency of classes among training responses
table(Khan$ytest)   # frequency of classes among test responses

train=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))   # training data 
fit=svm(y~.,data=train,kernel="linear",cost=10)   # fit svm on training data
summary(fit)   # summary
table(true=train$y,predicted=fit$fitted)   # confusion matrix for training data

test=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))   # test data
preds=predict(fit,newdata=test)   # predictions on test data
table(true=test$y,predicted=preds)   # confusion matrix for test data
