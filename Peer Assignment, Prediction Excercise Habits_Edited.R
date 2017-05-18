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
library(caret)
set.seed(112233)

#Read raining and testing data
if(!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv") }
if(!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv") }

#Load the training and testing csv files into R 
data_training <- read.csv("pml-training.csv")
data_testing <- read.csv("pml-testing.csv")

# Remove NAs and missings from the training and testing data
na <- apply(data_training, 2, function(x) {sum(is.na(x))})
train_set_1 <- data_training[na_list == 0]
test_set_1 <- data_testing[na_list == 0]

#Remove columns with missing data
empty <- apply(train_set_1, 2, function(x) {sum(x == "")})
train_set_2 <- train_set_1[empty == 0]
test_set_2 <- test_set_1[empty == 0]

# Remove the columns that are not numeric and will not be contributing
train_set_cleaned <- train_set_2[-c(1:7)]
test_set_cleaned<- test_set_2[-c(1:7)]

# Final result of the cleaned data
str(train_set_cleaned)
str(test_set_cleaned)

#Split into training and test/validation datasets
inTrain <- createDataPartition(y=train_set_cleaned$classe, p=0.7, list = FALSE)
myTrain <- train_set_cleaned[inTrain, ]
myTest <- test_set_cleaned[-inTrain, ]
#############################################################################################
#The first model we will test is uses the rpart method. First we train the model 
#using the training dataset, then we predict the out of sample results on the test set. 
#Then check the accuracy of the prediction on the out of sample model in the confusion matrix.

#Then we apply the same methodology to two other types of modeling approaches, the random 
#forest approach and the boosted approach to see how those model prediction accuracies comapre. 
#The model with the best accuracy is used to determine the overall prediction for the question. 

mdl1 <- train(classe ~ . , method = "rpart", data = myTrain)
pred1 <- predict(mdl1, myTest)
confusionMatrix1(table(pred, myTest$classe))
#The accuracy of this model fit is 49%

mdl2 <- train(classe ~ . , method = "rf", data = myTrain, prox=TRUE)
pred2 <- predict(mdl2, myTest)
confusionMatrix3(table(pred, myTest$classe))
#The accuracy of thie model fit is 99%

mdl3 <- train(classe ~ . , method = "gbm", data = myTrain, verbose=FALSE)
pred3 <- predict(mdl3, myTest)
confusionMatrix3(table(pred, myTest$classe))
#The accuracy of this model fit is 96%
#The model that predicts with the greatest accuracy is the random forest model fit
#Thus moving forward in the analysis, we will predict using the random forest method
####################################################################################################
#Another way we could do this in one step is to stack the predictions together using the rf method
#(since the rf method was shown to preform the best). The stacked method combines all the models into
#one dataframe and then compares the accuracies in one overall print out. 
model_stacked <- data.frame(pred1, predict2, predict3, classe=myTrain$classe)
fit_stacked <- train(classe~., data=model_stacked, method="rf")

#Predict the accuracies of the stacked data on the test data
predict_stacked_test <- predict(fit_stacked, newdata=myTest)

#Preform confusion matrix to get the accuracy of the predictions
cm1 <- confusionMatrix(pred1, myTest$classe)
cm2 <- confusionMatrix(pred2, myTest$classe)
cm3 <- confusionMatrix(pred3, myTest$classe)
cm4 <- confusionMatrix(predict_stacked_test, myTest$classe)

#Use the following code to print the accuracies and compare them (stacked should be better than using the other individual methods)
print(paste(cm1$overall[1], cm2$overall[1], cm3$overall[1], cm4$overall[1]))
#Accuracies of the stacked model fit should predict better than the accuracies of the individual methods as described above
#############################################################################################################
#The last step in this analysis is to use this code to predict the outcome of twenty individual predictions. 
#20 case test set predictions
predictionResult <- predict(mdl2, train_set_cleaned)
print(predictionResult)
#[1] B A B A A E D B A A B C B A E E A B B B




