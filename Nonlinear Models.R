# load Wage dataset
library(ISLR)
data("Wage")
Wage=Wage[,c(1,2,5,11)]
rownames(Wage)=1:nrow(Wage)
attach(Wage)

######################################################
# Polynomial Regression
######################################################

polyfit1=lm(wage~poly(age,4,raw=TRUE),data=Wage)   # fit polynomial of degree 4
summary(polyfit1)   # summary of the polynomial fit
polyfit1=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)   # alternative way

polyfit2=lm(wage~poly(age,4),data=Wage)   # fit orthogonal polynomials
summary(polyfit2)   # summary

# both fits generate same predictions
plot(polyfit1$fitted.values,polyfit2$fitted.values)

agelims=range(age)   # lowest and highest value of age
agelims
age.grid=seq(from=agelims[1],to=agelims[2])   # sequence of age values for prediction
age.grid
preds=predict(polyfit2,newdata=data.frame(age=age.grid),se=TRUE)   # predictions
se_bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)   # standard error bands
# left panel of Figure 7.1 (slide 243)
plot(age,wage,col="darkgrey")   # plot data
lines(age.grid,preds$fit,lwd=2,col="blue")   # add fit
matlines(age.grid,se_bands,col="blue",lty=2)   # add standard error bands

# polynomial logistic regression
polyfit3=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
summary(polyfit3)   # summary of the fit
preds_logit=predict(polyfit3,newdata=data.frame(age=age.grid),se=TRUE)   # logit predictions
se_bands_logit=cbind(preds_logit$fit+2*preds_logit$se.fit,
               preds_logit$fit-2*preds_logit$se.fit)   # standard error bands for logits
preds_prob=exp(preds_logit$fit)/(1+exp(preds_logit$fit))   # probability predictions
se_bands_prob=exp(se_bands_logit)/(1+exp(se_bands_logit))   # standard error bands for probabilities

plot(age,I(wage>250),type="n")
points(jitter(age),I(wage>250),col="darkgrey",pch="l",cex=0.5)   # plot data
lines(age.grid,preds_prob,lwd=2,col="blue")   # add fit
matlines(age.grid,se_bands_prob,col="blue",lty=2)   # add standard error bands

# right panel of Figure 7.1 (slide 243)
plot(age,I(wage>250),type="n",ylim=c(0,0.2)) 
points(jitter(age),I(wage>250)/5,col="darkgrey",pch="l",cex=0.5)   # plot data
lines(age.grid,preds_prob,lwd=2,col="blue")   # add fit
matlines(age.grid,se_bands_prob,col="blue",lty=2)   # add standard error bands

######################################################
# Piecewise Constant Functions
######################################################

?cut
table(cut(age,4))   # knots automatically chosen by R
table(cut(age,breaks=c(min(age),33,48,63,max(age))))   # user defined knots
stepfit=lm(wage~cut(age,4),data=Wage)   # fit piecewise constant functions
summary(stepfit)   # fit summary
  
preds=predict(stepfit,newdata=data.frame(age=age.grid),se=TRUE)   # predictions
se_bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)   # standard error bands
# left panel of Figure 7.2 (slide 246)  
plot(age,wage,col="darkgrey")   # plot data
lines(age.grid,preds$fit,lwd=2,col="blue")   # add fit
matlines(age.grid,se_bands,col="blue",lty=2)   # add standard error bands

######################################################
# Splines
######################################################

install.packages("splines")   # install splines package
library(splines)   # load splines package

?bs   # fit splines
?ns   # fit natural cubic splines
?smooth.spline   # fit smoothing splines
?loess   # fit local regression

# fit a cubic spline with user defined knots
bsfit=lm(wage~bs(age,knots=c(25,40,60),degree=3),data=Wage)   
summary(bsfit)   # summary

dim(bs(age,knots=c(25,40,60),degree=3))   # dimension of matrix of basis functions
dim(bs(age,df=6))   # dimension of matrix of basis functions
attr(bs(age,df=6),"knots")   # knots defined by R automatically

preds=predict(bsfit,newdata=data.frame(age=age.grid),se=TRUE)   # predictions
se_bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)   # standard error bands

plot(age,wage,col="darkgrey")   # plot data
abline(v=c(25,40,60),col="darkgreen")   # add knots
lines(age.grid,preds$fit,col="blue",lwd=2)   # add fit
matlines(age.grid,se_bands,col="blue",lty=2)   # add standard error bands

# fit a natural cubic spline with knots chosen by R
nsfit=lm(wage~ns(age,df=4),data=Wage)
attr(ns(age,df=4),"knots")   # knots chosen by R
summary(nsfit)

preds=predict(nsfit,newdata=data.frame(age=age.grid),se=TRUE)   # predictions
se_bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)   # standard error bands

# left panel of Figure 7.5 (slide 260)  
plot(age,wage,col="darkgrey")   # plot data
abline(v=c(33.75,42,51),col="darkgreen")   # add knots
lines(age.grid,preds$fit,col="red",lwd=2)   # add fit
matlines(age.grid,se_bands,col="red",lty=2)   # add standard error bands

# fit a smoothing spline with specified effective degrees of freedom
ssfit1=smooth.spline(age,wage,df=16)
ssfit1
# fit a smoothing spline with effective degrees of freedom chosen by CV
ssfit2=smooth.spline(age,wage,cv=TRUE)
ssfit2$df

# Figure 7.8 (slide 265)  
plot(age,wage,col="darkgrey")   # plot data
lines(ssfit1,col="red",lwd=2)   # plot first smoothing spline
lines(ssfit2,col="blue",lwd=2)   # plot second smoothing spline
# add legends
legend("topright",legend=c("16 df","6.8 df"),col=c("red","blue"),lwd=2,cex=0.8)

# fit local regression models with different spans
loessfit1=loess(wage~age,span=0.2,data=Wage)
loessfit1
loessfit2=loess(wage~age,span=0.7,data=Wage)
loessfit2

preds1=predict(loessfit1,newdata=data.frame(age=age.grid))   # predictions
preds2=predict(loessfit2,newdata=data.frame(age=age.grid))   # predictions

# Figure 7.10 (slide 268)  
plot(age,wage,col="darkgrey")   # plot data
lines(age.grid,preds1,col="red",lwd=2)   # add first local regression fit
lines(age.grid,preds2,col="blue",lwd=2)   # add second local regression fit
# add legends
legend("topright",legend=c("Span=0.2","Span=0.7"),col=c("red","blue"),lwd=2,cex=0.8)

######################################################
# GAMs
######################################################

install.packages("gam")   # install gam package
library(gam)   # load gam package
?gam

# fit gam with natural splines for year and age
gam1=gam(wage~ns(year,df=4)+ns(age,df=5)+education,data=Wage)
summary(gam1)
# Figure 7.11 (slide 270)  
par(mfrow=c(1,3))   # set graphical parameters
plot.Gam(gam1,se=TRUE,col="red")

# fit gam with smoothing splines for year and age
gam2=gam(wage~s(year,df=4)+s(age,df=5)+education,data=Wage)
summary(gam2)
# Figure 7.12 (slide 271)  
par(mfrow=c(1,3))   # set graphical parameters
plot.Gam(gam2,se=TRUE,col="red")

preds=predict.Gam(gam2,newdata=Wage)   # predictions
preds

# fit gam with smoothing spline for year and local regression for age
gam3=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
par(mfrow=c(1,3))   # set graphical parameters
plot.Gam(gam3,se=TRUE,col ="red")


