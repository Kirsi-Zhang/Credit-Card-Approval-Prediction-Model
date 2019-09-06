######Descriptive Analysis

#Load the data
Data3 <- read.csv("~/Desktop/MKT436 analytics/Homework 3/Student Data 3.csv", stringsAsFactors=FALSE)
library('earth')

#Implement MARS using earth function
earthFit<-earth(card~.,degree=2,data=Data3)
plotmo(earthFit)

######Predictive Analysis

###First predictive model: all the data except expenditure
data2<-Data3[,-6]
set.seed(218)

#Generate a training set that is 80% of the data, and a validation set that is 20% 
isTraining2 = runif(nrow(data2))<.8  
data2Train = subset(data2,isTraining2)
data2Valid = subset(data2,!isTraining2)

#Function returns the RMSE using the validation set for the detailing data
getcard2RMSE = function(model){
  actualY = data2Valid$card
  predictedY = predict(model,data2Valid)
  return(mean((actualY-predictedY)^2)^.5)
}

#linear
getcard2RMSE(lm(card~log(reports +1)+income+log(share)+age+owner +dependents+months+selfemp,data=data2Train))
getcard2RMSE(lm(card~log(reports +1)+income+log(share)+age+owner +dependents+months+selfemp+active+majorcards,data=data2Train))
getcard2RMSE(lm(card~log(reports +1)+log(share)+age+owner +dependents/income+months+selfemp+active+majorcards,data=data2Train))

#MARS
getcard2RMSE(earth(card~.,degree=2,data=data2Train))
getcard2RMSE(earth(card~.,degree=2,thres=0,data=data2Train))  #0.3653257
getcard2RMSE(earth(card~.,degree=2,thres=.001,data=data2Train))

#K-Fold Cross Validation 
set.seed(218) 
nFold = 2
#Step 1: Randomly choose which fold each row is in 
valNum = floor(runif(nrow(data2))*nFold)+1

#Create a matrix where we store prediction error 
modelPerformance = matrix(NA,nFold,2)


for(fold in 1:nFold){
  #Step 2i: Get the training and validation data for this fold
  trainingData = subset(data2,valNum!=fold)
  validationData = subset(data2,valNum==fold)
  
  #Step 2ii: Estimate the model for this training data
  model1 = lm(card~log(reports +1)+income+log(share)+age+owner +dependents+months+selfemp+active+majorcards,data=trainingData)
  
  model2 = earth(card~.,degree=2,data=trainingData)
  
  
  #Step 2iii: Calculate out of sample MSE for this validationData
  valid1 = mean((validationData$card - predict(model1,validationData))^2)^.5
  valid2 = mean((validationData$card - predict(model2,validationData))^2)^.5
  
  #Store model performance
  modelPerformance[fold,] = c(valid1,valid2)
}

colMeans(modelPerformance)  #0.3787836 0.3744732

#model2's MSE is smaller than model1's MSE
chosenModel=earth(card~.,degree=2,thres=0,data=data2Train)

###Second predictive model: all the data including expenditure
set.seed(217)
#Generate a training set that is 80% of the data, and a validation set that is 20% 
isTraining = runif(nrow(Data3))<.8  
data3Train = subset(Data3,isTraining)
data3Valid = subset(Data3,!isTraining)

#Function returns the RMSE using the validation set for the detailing data
getcard3RMSE = function(model){
  actualY = data3Valid$card
  predictedY = predict(model,data3Valid)
  return(mean((actualY-predictedY)^2)^.5)
}

#linear
getcard3RMSE(lm(card~reports+income+share+age+owner +dependents+months+selfemp+active*expenditure+majorcards,data=data3Train))
getcard3RMSE(lm(card~log(reports +1)+income+log(share)+age+owner +dependents+months+selfemp+active+majorcards+expenditure,data=data3Train))
getcard3RMSE(lm(card~log(reports +1)+log(share)+age+owner +dependents/income+months+selfemp+active+majorcards+expenditure,data=data3Train))

#MARS
getcard3RMSE(earth(card~.,degree=2,thres=.001,data=data3Train))
getcard3RMSE(earth(card~.,degree=3,data=data3Train))     #0.2874481
getcard3RMSE(earth(card~.,degree=3,thres=0.001,data=data3Train))     #0.2874481

#K-Fold Cross Validation 
set.seed(217) 
nFold = 2
#Step 1: Randomly choose which fold each row is in 
valNum = floor(runif(nrow(Data3))*nFold)+1

#Create a matrix where we store prediction error 
modelPerformance = matrix(NA,nFold,2)


for(fold in 1:nFold){
  #Step 2i: Get the training and validation data for this fold
  trainingData = subset(Data3,valNum!=fold)
  validationData = subset(Data3,valNum==fold)
  
  #Step 2ii: Estimate the model for this training data
  model1 = lm(card~log(reports +1)+income+log(share)+age+owner +dependents+months+selfemp+active+majorcards+expenditure,data=trainingData)
  
  model2 = earth(card~.,degree=2,thres=0,data=trainingData)
  
  
  #Step 2iii: Calculate out of sample MSE for this validationData
  valid1 = mean((validationData$card - predict(model1,validationData))^2)^.5
  valid2 = mean((validationData$card - predict(model2,validationData))^2)^.5
  
  #Store model performance
  modelPerformance[fold,] = c(valid1,valid2)
}

colMeans(modelPerformance)
# 0.3682341 0.2821581

#model2's MSE is smaller than model1's MSE
chosenModel2=earth(card~.,degree=3,data=data3Train)


######Save models to an R Workspace
save(chosenModel, chosenModel2, file='PredictionModels.Rdata')
