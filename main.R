######################################################################################
# to configure the environment, run env.r file:
######################################################################################

library(CASdatasets)
library(MASS)
library(lsr)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(pdp)

data(freMPL1) # dataset from CASdatasets
data_set=freMPL1
str(data_set)


## feature engineering
data_set = data_set[,c("Exposure","Gender","DrivAge","VehAge","VehPrice","VehMaxSpeed","ClaimAmount")]
data_set$Gender=as.factor(data_set$Gender)
levels(data_set$Gender)
data_set$VehMaxSpeed=as.factor(data_set$VehMaxSpeed)
levels(data_set$VehMaxSpeed)
data_set$VehAge=as.factor(data_set$VehAge)
levels(data_set$VehAge)
data_set$VehAge<-factor(data_set$VehAge,levels(data_set$VehAge)[c(1,2,4,5,6,7,8,9,3)])
levels(data_set$VehAge)
data_set$VehPrice=as.factor(data_set$VehPrice)
levels(data_set$VehPrice)
#categorcial A-Z but treated as numercial
data_set$VehPrice<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26")[as.integer(data_set$VehPrice)]


# existing exposure and non-zero claim amount
data_set<-data_set[which(data_set$Exposure>0),]
data_set=data_set[data_set$ClaimAmount>0,]
dim(data_set)
data_set$VehPrice <- as.integer(data_set$VehPrice)

## pre-analysis

# Claim Amount distribution

par(mfrow=c(1,2))

plot(density(data_set$ClaimAmount), lwd=1.5, col="blue", 
     main="Empirical density",
     ylab="", xlab="Claim amount")
plot(density(log(data_set$ClaimAmount)), lwd=1.5, col="blue", 
     main="Empirical density",
     ylab="", xlab="Log Claim amount")

# comparison with theoretical quantiles of the normal distribution
qqnorm(data_set$ClaimAmount,main="Claim amount",col="green",pch=20)
qqnorm(log(data_set$ClaimAmount),main="Log Claim amount",col="green",pch=20)

par(mfrow=c(1,1))

# dependent variables:
# Gender 

#boxplots
boxplot(log(data_set$ClaimAmount)~data_set$Gender, outline=FALSE,
        xlab="Gender",col="blue", ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

# number of obsevations in categories
table(data_set$Gender)

# VehAge

# boxplots
boxplot(log(data_set$ClaimAmount)~data_set$VehAge, outline=FALSE,
        xlab="VehAge",col="blue", ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

# number of obsevations in categories
table(data_set$VehAge)

# recategorization
levels(data_set$VehAge)<-c("0","1","2","3","4","5-7","5-7","8+","8+")
table(data_set$VehAge)

boxplot(log(data_set$ClaimAmount)~data_set$VehAge, outline=FALSE,
        xlab="VehAge",col="blue", ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

# VehMaxSpeed

# boxplots
boxplot(log(data_set$ClaimAmount)~data_set$VehMaxSpeed, outline=FALSE,
        xlab="VehMaxSpeed",col="blue", ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

# number of observations in categories
table(data_set$VehMaxSpeed)

# recategorization
levels(data_set$VehMaxSpeed)<-c("0-150 km/h","0-150 km/h","0-150 km/h","150-160 km/h","160-170 km/h","170-180 km/h","180-190 km/h","190-200 km/h","200-220 km/h","220+ km/h")
table(data_set$VehMaxSpeed)

boxplot(log(data_set$ClaimAmount)~data_set$VehMaxSpeed, outline=FALSE,
        xlab="VehMaxSpeed",col="blue", ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

# DrivAge

# dividing into deciles
cut_range=min(data_set$DrivAge)-1
cut_name=0
for (j in seq(0.1:1,by=0.1)){
  cut_range=c(cut_range,quantile(data_set$DrivAge,j))
  cut_name=c(cut_name,100*j/10)
}
cut_range=unique(cut_range)
cut_name=cut_name[c(1:(length(cut_range)-1))]

DrivAge_cut=cut(data_set$DrivAge,breaks=cut_range,labels=cut_name)

boxplot(log(data_set$ClaimAmount)~DrivAge_cut,outline=FALSE,
        xlab="DrivAge",col="blue",ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

table(DrivAge_cut)

# VehPrice

# dividing into deciles
cut_range=min(data_set$VehPrice)-1
cut_name=0
for (j in seq(0.1:1,by=0.1)){
  cut_range=c(cut_range,quantile(data_set$VehPrice,j))
  cut_name=c(cut_name,100*j/10)
}
cut_range=unique(cut_range)
cut_name=cut_name[c(1:(length(cut_range)-1))]

VehPrice_cut=cut(data_set$VehPrice,breaks=cut_range,labels=cut_name)

boxplot(log(data_set$ClaimAmount)~VehPrice_cut,outline=FALSE,
        xlab="VehPrice",col="blue",ylab="Log scale",
        main="Claim amounts")
abline(h=mean(log(data_set$ClaimAmount)),col="orange")

table(VehPrice_cut)

# correlations

cor(data_set$VehPrice,data_set$DrivAge,method="spearman")

par(mfrow=c(1,2))

boxplot(data_set$VehPrice~data_set$VehMaxSpeed, outline=FALSE,
        xlab="VehMaxSpeed",col="blue", ylab="VehPrice",
        main="Dependencies")
abline(h=mean(data_set$VehPrice),col="orange")

boxplot(data_set$DrivAge~data_set$VehMaxSpeed, outline=FALSE,
        xlab="VehMaxSpeed",col="blue", ylab="DrivAge",
        main="Dependencies")
abline(h=mean(data_set$DrivAge),col="orange")

cramersV(DrivAge_cut,data_set$VehMaxSpeed)
cramersV(VehPrice_cut,data_set$VehMaxSpeed)

boxplot(data_set$VehPrice~data_set$Gender, outline=FALSE,
        xlab="Gender",col="blue", ylab="VehPrice",
        main="Dependencies")
abline(h=mean(data_set$VehPrice),col="orange")

boxplot(data_set$DrivAge~data_set$Gender, outline=FALSE,
        xlab="Gender",col="blue", ylab="DrivAge",
        main="Dependencies")
abline(h=mean(data_set$DrivAge),col="orange")

cramersV(DrivAge_cut,data_set$Gender)
cramersV(VehPrice_cut,data_set$Gender)

par(mfrow=c(1,1))

## log-linear model estimation (not generalized as it's gaussian)

model_lognormal=glm(log(ClaimAmount)~Gender+VehMaxSpeed+DrivAge,family=gaussian, data_set)
summary(model_lognormal)

# model analysis

coef(summary(model_lognormal))["VehMaxSpeed160-170 km/h", c("Estimate")]
exp(coef(summary(model_lognormal))["VehMaxSpeed160-170 km/h", c("Estimate")]) 

# examples

z1 = predict(model_lognormal, newdata = data.frame(Gender="Male",DrivAge=53,VehMaxSpeed="0-150 km/h"))
exp(z1) #unlog to get expected value of claim amount
z2 = predict(model_lognormal, newdata = data.frame(Gender="Male",DrivAge=32,VehMaxSpeed="160-170 km/h"))
exp(z2)

# elimination of variables (AIC criterion)

stepAIC(model_lognormal)
model_lognormal = glm(log(ClaimAmount)~DrivAge,family=gaussian, data_set)
summary(model_lognormal)

# model analysis

# Residuals
plot(residuals(model_lognormal,type="response")~predict(model_lognormal,type="response"), xlab="Prediction", ylab="Residual", col="green",pch=20)
abline(h=0,col="red")

# QQ plot
std_residuals=residuals(model_lognormal,type="response")
std_residuals=std_residuals/sqrt(summary(model_lognormal)$dispersion)
std_residuals=std_residuals/sqrt(1-hatvalues(model_lognormal))
sorted_std_residuals=sort(std_residuals)
normal_quantiles=qnorm(c(1:length(data_set$ClaimAmount))/(length(data_set$ClaimAmount)+1),0,1)
plot(sorted_std_residuals~normal_quantiles,col="green",pch=20,
     xlim=c(-4,4),ylim=c(-4,4),
     xlab="Theoretical",ylab="Sample",main="QQ plot for residuals")
abline(0,1,col="red")

# trying different models

model_lognormal2=glm(log(ClaimAmount)~ DrivAge+Gender*DrivAge,family=gaussian,data_set)
AIC(model_lognormal2)

model_lognormal3=glm(log(ClaimAmount)~Gender+DrivAge+I(DrivAge^2),family=gaussian,data_set)
AIC(model_lognormal3)

model_lognormal4=glm(log(ClaimAmount)~Gender+DrivAge+I(DrivAge^2)+VehMaxSpeed,family=gaussian,data_set)
AIC(model_lognormal4)

model_lognormal5=glm(1/ClaimAmount~DrivAge,family=Gamma(link="log"),data_set)
AIC(model_lognormal5)
summary(model_lognormal5)

# deviance function for gaussian distribution (loss function) 
gaussian_deviance_function<-function(x){
  
  if (is.vector(x)==TRUE){
    y=x[1]
    mu=x[2]
  }else{
    y=x[,1]
    mu=x[,2]
  }
  
  z=mean((y-mu)^2)
  
  return(z)
}

# data sampling for out-of-sample cv
cv_group=rep(1:10,length=nrow(data_set))
cv_group=sample(cv_group,length(cv_group),replace=FALSE)

cv_loss_lm=numeric(10)

# cv
for (k in c(1:10)){
  
  index_val=which(cv_group==k)  
  data_set_train=data_set[-index_val,]
  
  model_cv=glm(log(ClaimAmount)~DrivAge,family=gaussian,data_set_train)
  
  cv_prediction_lm=predict(model_cv,newdata=data_set[index_val,],type="response")
  
  cv_loss_lm[k]=gaussian_deviance_function(cbind(log(data_set[index_val,"ClaimAmount"]), cv_prediction_lm))
  
}

cv_loss_lm=mean(cv_loss_lm) # average of the value of the loss function from each iteration of cv
cv_loss_lm # value of the loss function

##########################################
## Boosting for Tree (gaussian case)
##########################################

J=2 # tree depth
M=50 # number of iterations for boosting
alpha=0.05 #shrinkage parameter

cv_loss_boosting_train=matrix(0,10,M)
cv_loss_boosting_val=matrix(0,10,M)

for (k in c(1:10)){
  
  index_val=which(cv_group==k)
  data_set_train=data_set[-index_val,] # k in-sample
  
  cv_boosting_train=rep(0,nrow(data_set_train)) # m boosting predictor for in-samples
  cv_boosting_val=rep(0,length(index_val)) # m boosting predictor for out-of-samples
  
  for (m in c(1:M)){
    
    # m-iteration model for residuals relative to the m-1 iteration predictor
    model_boosting=rpart((log(ClaimAmount)-cv_boosting_train)~Gender+DrivAge+VehMaxSpeed,
                         data=data_set_train,method="anova",
                         control=rpart.control(maxdepth=J,xval=1,minbucket=100,cp=0.0001))
    
    cv_loss_boosting_train[k,m]=gaussian_deviance_function(
      cbind(log(data_set[-index_val,"ClaimAmount"]), cv_boosting_train + predict(model_boosting)))
    
    cv_loss_boosting_val[k,m]=gaussian_deviance_function(
      cbind(log(data_set[index_val,"ClaimAmount"]), cv_boosting_val + predict(model_boosting,newdata=data_set[index_val,])))
    
    # Returned predictors for in-samples and out-of-samples
    cv_boosting_train<- cv_boosting_train + predict(model_boosting)*alpha
    cv_boosting_val<- cv_boosting_val + predict(model_boosting,newdata=data_set[index_val,])*alpha
  }
  
}

# comparison of losses for linear model and boosting iteration, showing the lowest (first boosting iteration is just a non-boosted tree model)
plot(apply(cv_loss_boosting_train,2,mean)*100,col="magenta",type="l",
     main="Boosting",xlab="Iterations",ylab="Loss", ylim = c(180,190))
lines(apply(cv_loss_boosting_val,2,mean)*100,col="orange")
abline(h=cv_loss_lm*100, col="green", lty=2)
abline(h=min(cv_loss_lm, apply(cv_loss_boosting_val,2,mean))*100, lty=1, col="blue")
legend(x="topright", col=c("magenta", "orange", "green","blue"), 
       lty=c(1,1,2,1), lwd=c(1,1,1,1), pch=c(19,-1,-1,-1), cex=0.5,
       legend=c("In-sample Boosting", "Out-of-sample Boosting", "LM", "Min"))

apply(cv_loss_boosting_val,2,mean) #value of loss for m-iteration

# variables that have the greatest predictive power in-sample in a tree model

model_boosting
rpart.rules(model_boosting)

# Partial Dependence Plots

partial(model_boosting, pred.var = c("Gender","DrivAge"), plot = TRUE)
partial(model_boosting, pred.var = c("Gender","VehMaxSpeed"), plot = TRUE)
partial(model_boosting, pred.var = c("VehMaxSpeed","DrivAge"), plot = TRUE)

#################################
# Data source:
# http://cas.uqam.ca
# http://cas.uqam.ca/pub/web/CASdatasets-manual.pdf
# for more information about boosting algorithm implementation I refer to 'Effective Statistical Learning Methods for Actuaries' I and II, Springer 2019
# Code author: Juliusz Juzaszek
################################

