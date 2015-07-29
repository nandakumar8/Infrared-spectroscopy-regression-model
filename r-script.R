library(caret)
library(AppliedPredictiveModeling)
library(pls)
library(e1071)
library(lattice)
library(pls)
library(MASS)
library(lars)
library(elasticnet)
library(car)

data(tecator)
set.seed(2)

###the dataset has two matrices absorp and endpoints.Absorp contains the absorbance data for the 215 samples at 100 different frequencies
###the endpoint matrix contains the percentage of water,fat,protein for the 215 samples

absorb<-as.data.frame(absorp[])
nrow(absorb)
head(absorb)
endpoint<-as.data.frame(endpoints[])
nrow(endpoint)
head(endpoint)

###create training and test data set
###set 75 percent of rows for training and rest for test
bound<-floor(0.75*nrow(absorb))
#absorb<- absorb[sample(nrow(absorb)), ]          
absorb.train <- absorb[1:bound, ]            
absorb.test <- absorb[(bound+1):nrow(endpoint), ] 
nrow(absorb.test)
head(absorb.test)
nrow(absorb.train)

bound2<-floor(0.75*nrow(endpoint))
#endpoint<- endpoint[sample(nrow(endpoint)), ]          
endpoint.train <- endpoint[1:bound2, ]            
endpoint.test<- endpoint[(bound2+1):nrow(endpoint), ] 
nrow(endpoint.test)
head(endpoint.test)
nrow(endpoint.train)

dataTrainX<-absorb.train
dataTestX<-absorb.test
dataTrainY<-endpoint.train
dataTestY<-endpoint.test

###check skewness 
skewness(dataTrainX$V7)
###apply box cox transformation
boxcox<-preProcess(dataTrainX,method ="BoxCox") 
dataTrainXtrans<-predict(boxcox,dataTrainX)
head(dataTrainXtrans)
hist(dataTrainXtrans$V2)
hist(dataTrainX$V2)

datatestXtrans<-predict(boxcox,dataTestX)
head(datatestXtrans)
hist(datatestXtrans$V1)
hist(dataTestX$V1)
###create training data

trainingData<-dataTrainXtrans
###divide fat rates by 100 to get decimal value
trainingData$fat<-dataTrainY$V2
head(trainingData)

###ordinary least squares regression
###fit the model
modelFit<-lm(fat~.,data=trainingData)
summary(modelFit)
windows()
plot(fat~V2,data=trainingData)
abline(lm(fat~V2,data=trainingData),col="red")
par(mfrow=c(2,2))
plot(modelFit)

###predict values
modelPred<-predict(modelFit,datatestXtrans)
###create observed and predicted values data frame
pred_df<-data.frame(obs=dataTestY$V2,pred=modelPred)
head(pred_df)
defaultSummary(pred_df)
RMSE(pred =modelPred,obs =dataTestY$V2)
###check RMSE of training set to check overfitting
trainpred<-predict(modelFit,dataTrainXtrans)
train_df<-data.frame(obs=dataTrainY$V2,pred=trainpred)
defaultSummary(train_df)
RMSE(pred=trainpred,obs=dataTrainY$V2)

###sampling, cross-validation

ctrl<-trainControl(method="cv",n=10)
set.seed(100)
lmTraincv<-train(x=dataTrainXtrans,y=dataTrainY$V2,method="lm",trControl =ctrl)
lmTraincv2<-train(fat~.,data=trainingData,method="lm",trControl =ctrl)
lmTraincv
lmTraincv2

###the model parameters for ordinary least sqares show overfitting of data.The high r squared values and differnece in training and test RMSE values prove this

###pls regression

###use pls package, plser function
###default algorithm is Dayal and Mcgregor kernel algorithm
plsFit<-plsr(fat~.,data=trainingData,validation="CV")

###predict first five fat values using 1 and 2 components
pls.pred<-predict(plsFit,datatestXtrans[1:5,],ncomp=1:2)

summary(plsFit)
validationplot(plsFit,val.type ="RMSEP")
pls.RMSEP<-RMSEP(plsFit,estimate="CV")
plot(pls.RMSEP,main="RMSEP PLS fat",xlab="Components")
min<-which.min(pls.RMSEP$val)
points(min,min(pls.RMSEP$val),pch=1,col="red")

plot(plsFit, ncomp=18, asp=1, line=True)

###use lowest RMSEP at 18 components
pls.pred2<-predict(plsFit,datatestXtrans,ncomp=18)
plot(dataTestY$V2,pls.pred2,main="observed and predicted fat %",xlab="Observed",ylab="PLS predicted")
abline(0,1,col="red")

pls.eval<-data.frame(obs=dataTestY$V2,pred=pls.pred2[,1,1])
defaultSummary(pls.eval)

###ridge regression
ridge<-lm.ridge(fat~.,data=trainingData,lambda =seq(0,100,by=1))
plot(ridge)
defaultSummary(ridge)
###to check the estimates
head(ridge)
head(ridge$coef)



