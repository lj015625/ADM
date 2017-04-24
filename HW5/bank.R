bank<-read.csv("C:/Users/lj015625/Desktop/DataMining Class/HW5/data/bank.csv")
bankFull<-read.csv("C:/Users/lj015625/Desktop/DataMining Class/HW5/data/bank-full.csv")
dim(bankFull)
names(bankFull)
summary(bankFull)
table(bankFull$y..category)

library(caret)
set.seed(123)
trainIndex <- createDataPartition(bankFull$y..category, p = 0.9, list = FALSE, times = 1)

bankFull_train <- bankFull[ trainIndex,]
bankFull_validate <- bankFull[ -trainIndex,]

dim(bankFull_train) #checking the split
dim(bankFull_validate) #checking the split
prop.table(table(bankFull_train$y..category)) #checking to see the class proportions between the training and test sets. 
prop.table(table(bankFull_validate$y..category))


library(rpart)
set.seed(123)
bank_yCat_rpart <- rpart(y..category ~., method="class", parms = list(split="gini"), minsplit = 20, cp = 0.01, data=bankFull_train)

library(rpart.plot)
rpart.plot(bank_yCat_rpart, type=0, extra=101)
rpart.plot(bank_yCat_rpart, type=1, extra=101)

library(party)
library(partykit)
plot(as.party(bank_yCat_rpart))


getResult<- function(bankFullModel, predictType)
{
  actual <- bankFull_validate$y..category
  predicted <- predict(bankFullModel, bankFull_validate, type=predictType)
  results.matrix <- confusionMatrix(predicted, actual, positive="yes")
  print(results.matrix)
}

getResult(bank_yCat_rpart, "class")



# 10-fold cross validation
fitControl <- trainControl(method="cv", number=10) #10-fold cross validation
set.seed(123)
bankFull_10folds<-train(y..category ~., data=bankFull_train, method="rpart", metric="Accuracy", trControl=fitControl)
bankFull_10folds

getResult(bankFull_10folds, "raw")

bankFull_10folds_pruned<-prune(bankFull_10folds$finalModel, cp=0.01932367)
plot(as.party(bankFull_10folds_pruned))



# Repeated 10-fold cross validation
fitControl <- trainControl(method="cv", number=10, repeats=5) 
set.seed(123)
bankFull_10folds_5repeat<-train(y..category ~., data=bankFull_train, method="rpart", metric="Accuracy", trControl=fitControl)
bankFull_10folds_5repeat

getResult(bankFull_10folds_5repeat, "raw")

bankFull_10folds_5repeat_pruned<-prune(bankFull_10folds_5repeat$finalModel, cp=0.01932367)
plot(as.party(bankFull_10folds_5repeat_pruned))



# Bootstrapping 
cvCtrl <- trainControl(method="boot", number=10) #10 bootstrapped samples.
set.seed(123)
bankFull_bootstrap<-train(y..category ~., data=bankFull_train, method="rpart", metric="Accuracy", trControl=cvCtrl)
bankFull_bootstrap

getResult(bankFulls_bootstrap, "raw")



# Bagging
library(ipred)
set.seed(123)
bankFull_bagging <- bagging(y..category ~., data = bankFull_train, nbagg = 10)

actual <- bankFull_validate$y..category
baggingPredicted <- predict(bankFull_bagging, newdata=bankFull_validate, type="class") 
results.matrix <- confusionMatrix(baggingPredicted$class, actual, positive="yes")
print(results.matrix)



# Bagging cross validation treebag
fitControl <- trainControl(method = "cv", number = 10) 
bankFull_treebag <- train(y..category ~., data = bankFull_train, method = "treebag", metric="Accuracy", na.action=na.omit, trControl = fitControl)

getResult(bankFull_treebag, "raw")



# Random Forest
library(randomForest)
set.seed(123) 
#default to try three predictors at a time and create 500 trees. 
bankFull_RForest <- randomForest(y..category ~., data=bankFull_train, mtry=3, ntree=500, na.action = na.omit, importance=TRUE) 
print(bankFull_RForest) 
importance(bankFull_RForest) 
varImpPlot(bankFull_RForest) 

getResult(bankFull_RForest, "class")



# Boosting (Slow)
library(adabag) #a popular boosting algorithm
set.seed(123)
bankFull_adaboost <- boosting.cv(y..category ~., data=bankFull_train, boos=TRUE, v=10) #.cv is adding cross validation
#don't worry about warning message. Also, this take a while to run.
bankFull_adaboost$confusion #confusion matrix for boosting
bankFull_adaboost$error #error rate for boosting (OOB)
bankFull_adaboost_accuracy <- 1 - BC_adaboost$error #accuracy rate for boosting (OOB)
bankFull_adaboost_accuracy




# Bagging cross validation SVM (Very slow)
fitControl <- trainControl(method = "cv", number = 1)
bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)
bankFull_svmbag <- train(y..category ~ ., data = bankFull_train, "bag", trControl = fitControl, na.action=na.omit, bagControl = bagctrl)

getResult(bankFull_svmbag, "raw")



# Logistic Regression
bankFull_train$poutcome..category_success <- ifelse(bankFull_train$poutcome..category=="success",1,0)
bankFull_train$contact..category_unknown <- ifelse(bankFull_train$contact..category=="unknown",1,0)
bankFull_train$duration..number_low <- ifelse(bankFull_train$duration..number<132,1,0)
bankFull_train$duration..number_median <- ifelse(bankFull_train$duration..number>=132 & bankFull_train$duration..number<504,1,0)
bankFull_train$duration..number_High <- ifelse(bankFull_train$duration..number>=504 & bankFull_train$duration..number<808,1,0)

bankFull_train_logit <- bankFull_train[,17:22]

bankFull_validate$poutcome..category_success <- ifelse(bankFull_validate$poutcome..category=="success",1,0)
bankFull_validate$contact..category_unknown <- ifelse(bankFull_validate$contact..category=="unknown",1,0)
bankFull_validate$duration..number_low <- ifelse(bankFull_validate$duration..number<132,1,0)
bankFull_validate$duration..number_median <- ifelse(bankFull_validate$duration..number>=132 & bankFull_validate$duration..number<504,1,0)
bankFull_validate$duration..number_High <- ifelse(bankFull_validate$duration..number>=504 & bankFull_validate$duration..number<808,1,0)

bankFull_validate <- bankFull_validate[,17:22]

fitControl <- trainControl(method = "cv", number = 10)
set.seed(123)
bankFull_lg <- train(y..category ~., data=bankFull_train_logit, method="glm", family="binomial", trControl = fitControl)
summary(bankFull_lg)
getResult(bankFull_lg, "raw")



# Naive Bayes
bankFull_train <- bankFull[ trainIndex,]
bankFull_validate <- bankFull[ -trainIndex,]

bootControl <- trainControl(method="boot", number=10)
set.seed(123)
bankFull_nb <- train(y..category ~ ., data=bankFull_train, method = "nb", trControl = bootControl)

summary(bankFull_nb)
getResult(bankFull_nb, "raw")

#Neural Network
bankFull_train <- bankFull[ trainIndex,]
bankFull_validate <- bankFull[ -trainIndex,]

cvControl <- trainControl(method="cv", number=5)
set.seed(123)
bankFull_nn <- train(y..category ~ ., data=bankFull_train, method = "nnet", trControl = cvControl)
summary(bankFull_nn)
getResult(bankFull_nn, "raw")

#ROC 
#Create a ROC curve
library(ROCR)

R10Fold_predict_prob<-predict(bankFull_10folds, type="prob", bankFull_validate)
R10Fold_pred = prediction(R10Fold_predict_prob[,2], bankFull_validate$y..category)
R10Fold_perf = performance(R10Fold_pred,"tpr", "fpr") #true pos and false pos

RForest_predict_prob<-predict(bankFull_RForest, type="prob", bankFull_validate)
RForest_pred = prediction(RForest_predict_prob[,2], bankFull_validate$y..category)
RForest_perf = performance(RForest_pred,"tpr", "fpr") #true pos and false pos

RBagging_predict_prob<-predict(bankFull_treebag, type="prob", bankFull_validate)
RBagging_pred = prediction(RBagging_predict_prob[,2], bankFull_validate$y..category)
RBagging_perf = performance(RBagging_pred,"tpr", "fpr") #true pos and false pos

RBoost_predict_prob<-predict(bankFull_boost, type="prob", bankFull_validate)
RBoost_pred = prediction(RBoost_predict_prob[,2], bankFull_validate$y..category)
RBoost_perf = performance(RBoost_pred,"tpr", "fpr") #true pos and false pos

RLogit_predict_prob<-predict(bankFull_lg, type="prob", bankFull_logit_validate)
RLogit_pred = prediction(RLogit_predict_prob[,2], bankFull_validate$y..category)
RLogit_perf = performance(RLogit_pred,"tpr", "fpr") #true pos and false pos

RNN_predict_prob<-predict(bankFull_nn, type="prob", bankFull_validate)
RNN_pred = prediction(RNN_predict_prob[,2], bankFull_validate$y..category)
RNN_perf = performance(RNN_pred,"tpr", "fpr") #true pos and false pos


# Start plotting
plot(R10Fold_perf, main="ROC Curves", col="black", lwd=2)
plot(RForest_perf, add=TRUE, col="red", lwd=2)
plot(RBagging_perf, add=TRUE, col="green", lwd=2)
plot(RBoost_perf, add=TRUE, col="blue", lwd=2)
plot(RLogit_perf, add=TRUE, col="cyan", lwd=2)
plot(RNN_perf, add=TRUE, col="brown", lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
legend('bottomright', c("10-fold Decision Tree", "Random Forest","Bagging", "Boost", "Logistic Regression", "Neural Network"), 
       lty=c(1,1), lwd=c(1,1), col=c("black", "red","green", "blue", "cyan", "brown"))




       