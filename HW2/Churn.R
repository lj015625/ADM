options(scipen=999)
setwd("C:/Users/lj015625/Desktop/DataMining Class/HW2/data")
churns<-read.csv("Churn_Calls.csv")
names(churns)
str(churns)
dim(churns)
summary(churns)
summary(churns$state)
set.seed(123) #set a seed to do draws from a random uniform distribution.
churns_rand <- churns[order(runif(5000)), ] 
churns_train <- churns_rand[1:4000, ] #Training data set; 4000 observations
churns_test  <-churns_rand[4001:5000, ]


library(rpart)
set.seed(123)
churns_rpart <- rpart(churns_train$churn~., method="class", minsplit = 20, 
                      parms = list(split="gini"), data=churns_train)
#summary(churns_rpart)

library(rpart.plot)
rpart.plot(churns_rpart, type=0, extra=101)
rpart.plot(churns_rpart, type=1, extra=101)

library(party)
library(partykit)
plot(as.party(churns_rpart))


# prune the tree
cptable<-printcp(churns_rpart)
cptable
set.cp.value<-cptable[which.min(cptable[,"xerror"]),"CP"]
set.cp.value
pruned_churns_rpart <- prune(churns_rpart, set.cp.value)
rpart.plot(pruned_churns_rpart, type=0, extra=101)
plot(as.party(pruned_churns_rpart))

plotcp(churns_rpart, minline=TRUE, col="red") 
pruned_churns_rpart <- prune(churns_rpart, 0.03671329)
rpart.plot(pruned_churns_rpart, type=1, extra=101)
plot(as.party(pruned_churns_rpart))

# test on testing set
library(caret)
actual <- churns_test$churn
predicted <- predict(pruned_churns_rpart, churns_test, type="class")
results.matrix <- confusionMatrix(predicted, actual, positive="yes")
print(results.matrix)


# method two use information gain
churns_rpart2 <- rpart(churns_train$churn~., method="class", minsplit = 20, 
                       parms = list(split="information"), data=churns_train)
rpart.plot(churns_rpart2, type=0, extra=101)
cptable<-printcp(churns_rpart2)
cptable
set.cp.value<-cptable[which.min(cptable[,"xerror"]),"CP"]
set.cp.value

plotcp(churns_rpart2, minline=TRUE, col="red") 
pruned_churns_rpart2 <- prune(churns_rpart2, 0.02272727)
rpart.plot(pruned_churns_rpart2, type=0, extra=101)
plot(as.party(pruned_churns_rpart2))

actual <- churns_test$churn
predicted2 <- predict(pruned_churns_rpart2, churns_test, type="class")
results.matrix2 <- confusionMatrix(predicted2, actual, positive="yes")
print(results.matrix2)

##############################################################################
#Logistic Regression#
set.seed(123) #set a seed to do draws from a random uniform distribution.
churns_rand <- churns[order(runif(5000)), ]
churns_logit<- churns_rand[,c(20,7,19,4,17,16,5)]
str(churns_logit)

churns_logit$total_day_minutes_large<-ifelse(churns_logit$total_day_minutes<264.75,"no","yes")
churns_logit$number_customer_service_calls_large<-ifelse(churns_logit$number_customer_service_calls<3.5,"no","yes")
churns_logit$total_intl_calls_large<-ifelse(churns_logit$total_intl_calls<2.5,"no","yes")
churns_logit$total_intl_minutes_large<-ifelse(churns_logit$total_intl_minutes<13.1,"no","yes")

churns_logit$total_day_minutes <- NULL
churns_logit$number_customer_service_calls <- NULL
churns_logit$total_intl_calls <- NULL
churns_logit$total_intl_minutes <- NULL

str(churns_logit)

churns_train_logit <- churns_logit[1:4000, ] #Training data set; 4000 rows
churns_test_logit  <-churns_logit[4001:5000, ] #Testing data set; 1000 rows

churnLogitModel <- glm(churn~., data=churns_train_logit, family=binomial()) #Fit a logistic regression
summary(churnLogitModel) #coefficients are presented as log-odds (probabilities on logit scale)
exp(cbind(Odds_Ratio=coef(churnLogitModel))) #Take exponent of log odds gives "odds" ratio.

anova(churnLogitModel,test="Chisq") 


#churns_test_logit$predictChurnProb<-predict(churnLogitModel, newdata=churns_test_logit, type="response")

churns_test_logit$predictChurnLogit<-predict(churnLogitModel, newdata=churns_test_logit)
odds <- exp(churns_test_logit$predictChurnLogit)
churns_test_logit$PredictedProb <- data.frame(odds/(1+odds))
library(psych)
describe(churns_test_logit$PredictedProb)
churns_test_logit$predictChurnLogit<-NULL
churns_test_logit$PredictedProb<-NULL

churns_test_logit_CI<-cbind(churns_test_logit, predict(churnLogitModel, newdata=churns_test_logit, type="link", se=TRUE))
churns_test_logit_CI <- within(churns_test_logit_CI, 
                                    {
                                      PredictedProb <- plogis(fit)
                                      LL <- plogis(fit - (1.96 * se.fit))
                                      UL <- plogis(fit + (1.96 * se.fit))
                                    }) 

churns_test_logit_CI.predictedChurn <- rep("no" ,1000)
churns_test_logit_CI.predictedChurn[churns_test_logit_CI$PredictedProb >.5] <- "yes"
table(churns_test_logit_CI.predictedChurn, churns_test_logit_CI$churn)

TP = 58
TN = 837
FP = 28
FN = 77
Sensitivity = TP/(TP+FN) #true positive rate; recall; TP/(TP+FN)
Specificity = TN/(TN+FP) #how often is the prediction negative when actual is negative?
Precision = TP/(TP+FP) #how often is prediction positive when actual is positive?
Accuracy = (TP+TN)/(TP+TN+FP+FN) #how often is classifier correct

Value<-round(c(TP,TN,FP,FN,Sensitivity,Specificity,Precision,Accuracy),digits=3)
Measure<-c("True Positive","True Negative","False Positive","False Negative","Sensitivity/Recall=TP/(TN+FP)",
           "Specificity=TN/(TN+TP)","Precision=TP/(TP+FP)","Accuracy=(TP+TN)/total")

table<-as.data.frame(cbind(Measure,Value))

library(knitr)
kable(table)

##############################################################################
#KNN#
set.seed(123)
churns_rand <- churns[order(runif(5000)), ] 
churns_rand$international_plan_yes <-ifelse(churns_rand$international_plan=="yes",1,0)
churns_rand$international_plan <- NULL 

churns_rand$voice_mail_plan_yes <-ifelse(churns_rand$voice_mail_plan=="yes",1,0)
churns_rand$voice_mail_plan <- NULL 

churns_rand$state <- NULL
churns_rand$area_code <- NULL

str(churns_rand)
churns_rand<-churns_rand[, c(16,1:15,17:18)] #Rearranging the columns so that our target variable is first
str(churns_rand)

# z scale
churns_rand_z <- as.data.frame(scale(churns_rand[-1]))
str(churns_rand_z)

churns_train_z <- churns_rand_z[1:4000,]
churnss_test_z <- churns_rand_z[4001:5000,]

churns_train_labels<-churns_rand[1:4000,1]
churns_test_labels<-churns_rand[4001:5000,1]

num_of_knn <- sqrt(4000)
num_of_knn
library(class)
library(gmodels)
churns_pred <- knn(train=churns_train_z, test=churnss_test_z, cl=churns_train_labels, k=63)
CrossTable(x=churns_pred, y=churns_test_labels, prop.chisq = FALSE)
length(churns_pred)
length(churns_test_labels)

TP = 15
TN = 863
FP = 0
FN = 122
Sensitivity = TP/(TP+FN) #true positive rate; recall; TP/(TP+FN)
Specificity = TN/(TN+FP) #how often is the prediction negative when actual is negative?
Precision = TP/(TP+FP) #how often is prediction positive when actual is positive?
Accuracy = (TP+TN)/(TP+TN+FP+FN) #how often is classifier correct

Value<-round(c(TP,TN,FP,FN,Sensitivity,Specificity,Precision,Accuracy),digits=3)
Measure<-c("True Positive","True Negative","False Positive","False Negative","Sensitivity/Recall=TP/(TN+FP)",
           "Specificity=TN/(TN+TP)","Precision=TP/(TP+FP)","Accuracy=(TP+TN)/total")

table<-as.data.frame(cbind(Measure,Value))

library(knitr)
kable(table)

##############################################################################
# KNN use variables from Decision Tree
set.seed(123)
churns_rand <- churns[order(runif(5000)), ] 
churns_knn<- churns_rand[,c(20,7,19,4,17,16,7,5)]
str(churns_knn)

# change to factor
churns_knn$total_day_minutes_large<-ifelse(churns_knn$total_day_minutes<264.75,0,1)
churns_knn$number_customer_service_calls_large<-ifelse(churns_knn$number_customer_service_calls<3.5,0,1)
churns_knn$total_intl_calls_large<-ifelse(churns_knn$total_intl_calls<2.5,0,1)
churns_knn$total_intl_minutes_large<-ifelse(churns_knn$total_intl_minutes<13.1,0,1)
churns_knn$international_plan_yes <-ifelse(churns_knn$international_plan=="yes",1,0)
churns_knn$total_day_minutes.1_large<-ifelse(churns_knn$total_day_minutes.1<163.5,0,1)
churns_knn$voice_mail_plan_yes <-ifelse(churns_knn$voice_mail_plan=="yes",1,0)


churns_knn$total_day_minutes <- NULL
churns_knn$number_customer_service_calls <- NULL
churns_knn$total_intl_calls <- NULL
churns_knn$total_intl_minutes <- NULL
churns_knn$total_day_minutes <- NULL
churns_knn$total_day_minutes.1 <- NULL
churns_knn$international_plan <- NULL 
churns_knn$voice_mail_plan <- NULL 

str(churns_knn)

churns_knn_z <- as.data.frame(scale(churns_knn[-1]))

churns_knn_train_z <- churns_knn_z[1:4000,]
churns_knn_test_z <- churns_knn_z[4001:5000,]

churns_train_labels <- churns_knn[1:4000,1]
churns_test_labels <- churns_knn[4001:5000,1]

num_of_knn <- sqrt(4000)
num_of_knn
library(class)
library(gmodels)
set.seed(123)
churns_knn_z_pred <- knn(train=churns_knn_train_z, test=churns_knn_test_z, cl=churns_train_labels, k=63)
CrossTable(x=churns_knn_z_pred, y=churns_test_labels, prop.chisq = FALSE)


TP = 98
TN = 819
FP = 24
FN = 98
Sensitivity = TP/(TP+FN) #true positive rate; recall; TP/(TP+FN)
Specificity = TN/(TN+FP) #how often is the prediction negative when actual is negative?
Precision = TP/(TP+FP) #how often is prediction positive when actual is positive?
Accuracy = (TP+TN)/(TP+TN+FP+FN) #how often is classifier correct

Value<-round(c(TP,TN,FP,FN,Sensitivity,Specificity,Precision,Accuracy),digits=3)
Measure<-c("True Positive","True Negative","False Positive","False Negative","Sensitivity/Recall=TP/(TN+FP)",
           "Specificity=TN/(TN+TP)","Precision=TP/(TP+FP)","Accuracy=(TP+TN)/total")

table<-as.data.frame(cbind(Measure,Value))

library(knitr)
kable(table)
