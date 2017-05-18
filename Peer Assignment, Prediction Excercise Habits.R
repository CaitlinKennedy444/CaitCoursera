#Coursera Course 8, Prediction Course project 

#Description and objectives 
#One thing that people regularly do is quantify how much of a particular activity they do, but they rarely 
#quantify how well they do it. In this project, your goal will be to use data from accelerometers on the 
#belt, forearm, arm and dumbell of 6 participants to predict the manner in which they did the exercise ("classe"). 
# You should create a report describing how you built your model. how you used cross validation, what you 
#think the expected out of sample error is, and why you made the choices you did. You will also use your prediction
#model to predict 20 different test cases. 

#The data for this project comes from this source: http://groupware.les.inf.puc-rio.br/har.
#The training data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
######################################################################################
setwd("C:/Coursera/Prediction")

#Load the packages used for accessing validation in training and testing validations 
library(caret);library(randomForest);library(doParallel);library(gbm);library(AppliedPredictiveModeling)

cl<-makeCluster(detectCores())
registerDoParallel(cl)
testBulk <- read.csv("pml-testing.csv",na.strings=c("NA",""))
trainBulk <- read.csv("pml-training.csv",na.strings=c("NA",""))
#Another way to load in csv files
#url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#data_train <- read.csv(url_train, header=TRUE)
#url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#data_test<- read.csv(url_test, header=TRUE)

#Clean the data
NAs <- apply(trainBulk,2,function(x) {sum(is.na(x))}) 
cleanTrain <- trainBulk[,which(NAs == 0)]
cleanTest <- testBulk[,which(NAs == 0)]

#Create training and testing sets 
trainIndex <- createDataPartition(y = cleanTrain$classe, p=0.7,list=FALSE)
trainSet <- cleanTrain[trainIndex,]
crossValidationSet <- cleanTrain[-trainIndex,]

# Removing variables that have time, or names in it, also new_window. Columns 1..6
removeIndex <- as.integer(c(1,2,3,4,5,6))
trainSet <- trainSet[,-removeIndex]
testSet <- cleanTest[,-removeIndex]

#Train the models using random forests and gbm 
modelFit_rf <- train(x=trainSet, y=trainSet$classe, method="rf")
modelFit_boosted <- train(x=trainSet, y=trainSet$classe, method="gbm")
modelFit_lda <- train(classe~., trainSet, method="lda")  

#Predict the accuracies on the test data
predict_rf <- predict(modelFit_rf, testSet)
predict_gbm <- predict(modelFit_boosted, newdata=testSet)
predict_lda <- predict(modelFit_lda, newdata=testSet)

#Stack the predictions together using the rf method
model_stacked <- data.frame(predict_rf, predict_gbm, predict_lda, classe=testSet$classe)
fit_stacked <- train(classe~., data=model_stacked, method="rf")

#Predict the accuracies of the stacked data on the test data
predict_stacked_test <- predict(fit_stacked, newdata=testSet)

#Preform confusion matrix to get the accuracy of the predictions
cm1 <- confusionMatrix(predict_rf, testSet$classe)
cm2 <- confusionMatrix(predict_gbm, testSet$classe)
cm3 <- confusionMatrix(predict_lda, testSet$classe)
cm4 <- confusionMatrix(predict_stacked_test, testSet$classe)

#Use the following code to print the accuracies and compare them (stacked should be better than using the other individual methods)
print(paste(cm1$overall[1], cm2$overall[1], cm3$overall[1], cm4$overall[1]))

#20 case test set predictions
pred2 <- predict(modelFit_rf, testSet)
testSet2 <- testSet
testSet2$classe <- pred2
pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)}}
answers <- testSet2$classe
pml_write_files(answers)
answers
#[1] B A B A A E D B A A B C B A E E A B B B
