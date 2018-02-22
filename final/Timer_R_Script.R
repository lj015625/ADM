# Use R & ggplot for Timer Data
# @Author Leonardo Ji 
options(scipen=999)
library(ggplot2)
library(scales)
library(tidyverse)
library(cluster)
library(Matrix)
library(arules)
library(dplyr)
library(splitstackshape)
library(arulesViz)
library(fpc)
library(clusterSim)
library(stats)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(caret)
library(VIM)
library(ipred)
library(randomForest)
library(e1071)
library(fastAdaboost)

## Data
timer <- read.csv("C:/Users/lj015625/Desktop/DataMining Class/Final/Data/Timer_04-24-2017.csv")
dim(timer)
str(timer)
names(timer)
summary(timer)
head(timer)

summary(timer$TIMERNAME)

freqData <- as.data.frame(table(timer$TIMERNAME))

timerMedian <- aggregate(data = timer, ELAPSEDTIME ~ TIMERNAME, median, na.rm = TRUE) 
timerMedian <- timerMedian[order(-timerMedian$ELAPSEDTIME), ] 
timerMedian

timerAvg <- aggregate(data = timer, ELAPSEDTIME ~ TIMERNAME, mean, na.rm = TRUE) 
dim(timerAvg)
str(timerAvg)
timerAvg <- timerAvg[order(-timerAvg$ELAPSEDTIME), ]
timerAvg
rownames(timerAvg)<-timerAvg$TIMERNAME
ggplot(data=timerAvg, aes(x=TIMERNAME, y=ELAPSEDTIME, group=1)) + geom_line()
timerAvg$TIMERNAME <- NULL


userTimerAvg <- aggregate(data = timer, ELAPSEDTIME ~ USERPRINCIPALS, mean, na.rm = TRUE) 
dim(userTimerAvg)
str(userTimerAvg)
userTimerAvg


# 1. Hierarchical clustering on Timer average time
dissimilarity.matrix <- dist(as.matrix(timerAvg, method="euclidean"))
timer_hclust <- hclust(dissimilarity.matrix, method="ward.D") 

dendrogram_table <- as.data.frame(timer_hclust$height) 
print(dendrogram_table)

plot(timer_hclust, hang = -1) 
rect.hclust(timer_hclust, k=4, border="red")


#summary(timer$USERPRINCIPALS)

timer_user <- subset(timer, select = c('USERPRINCIPALS', 'TIMERNAME'))
timerNames <- unique(timer_user$TIMERNAME)
length(timerNames)

users <- unique(timer_user$USERPRINCIPALS)
length(users)

timer_user <- timer_user[order(timer_user$USERPRINCIPALS),]
dim(timer_user)
head(timer_user)



userTransaction <- matrix(0, nrow = 11375, ncol = 1)
for (i in 1:length(users)) {
    timerForUser <- filter(timer_user, timer_user$USERPRINCIPALS==users[i])
    #timerForUser <- timer_user[which(timer_user$USERPRINCIPALS == users[i]),]
    
    ### flatten unique timer name into a single line
    uniqueTimerForUser <- unique(timerForUser$TIMERNAME);
    singleline <- paste(uniqueTimerForUser, collapse = ',')
    userTransaction[i,] <- singleline
}

## write the transaction
#trans <- as(timer_user, "transactions") does not combine user transactions 
write(userTransaction, file="C:/Users/lj015625/Desktop/DataMining Class/Final/Data/timer_basket") 
userTran<-read.transactions("C:/Users/lj015625/Desktop/DataMining Class/Final/Data/timer_basket", format="basket", sep=",")

dim(userTran)
freq_userTran_df <- as.data.frame(itemFrequency(userTran))
freq_userTran_df

itemFrequencyPlot(userTran, topN=10, names=TRUE)
itemFrequencyPlot(userTran, support = 0.1)

image(userTran)

image(userTran[1:5])

image(sample(userTran, 100))

summary(userTran)


# 2. Association rule by Apriori on User sessions
userTranRules <- apriori(userTran, parameter = list(support = 0.1, confidence = 0.9, minlen = 2)) 
print(userTranRules) 
summary(userTranRules)

userTranRules_sorted <-sort(userTranRules, by = c("lift", "confidence"))
inspect(userTranRules_sorted)

plot(userTranRules_sorted, shading="order", control=list(main="Two-key plot"))
plot(userTranRules_sorted, method="grouped", control=list(k=5))
plot(userTranRules_sorted[1:10], method="graph")
plot(userTranRules_sorted[1:10], method="graph", control=list(type="itemsets"))



# 3. KMean Clustering by user timer frequencies
timer_user_prop <- as.data.frame(table(timer_user))
dim(timer_user_prop)

userTimerSparse <- matrix(0, nrow = 11375, ncol = 75)
userTimerSparse[cbind(timer_user_prop$USERPRINCIPALS, timer_user_prop$TIMERNAME)] <- timer_user_prop$Freq;
dim(userTimerSparse)

userTimerSparse_df <- as.data.frame(userTimerSparse)
colnames(userTimerSparse_df) <- unique(timer_user_prop$TIMERNAME)
rownames(userTimerSparse_df) <- unique(timer_user_prop$USERPRINCIPALS)


hist(userTimerSparse_df$`CAP:MPG.RCM.POST-ACUTE-CURASPAN`)

quantile(userTimerSparse_df$`CAP:MPG.RCM.POST-ACUTE-CURASPAN`,prob = seq(0, 1, length = 6), na.rm=TRUE)

hist(userTimerSparse_df$`CAP:RCM_UM_WORKLIST.01`)

quantile(userTimerSparse_df$`CAP:RCM_UM_WORKLIST.01`,prob = seq(0, 1, length = 6), na.rm=TRUE)

## Optimal cluster size k
user_clusters_optimal<-kmeansruns(userTimerSparse_df, krange=3:9) #finds the "best"" K between 3 and 9
user_clusters_optimal$bestk

## Method 1: Use the visualizations
set.seed(123)
user_clusters_3 <- kmeans(userTimerSparse_df, centers=3) 
plotcluster(userTimerSparse_df, user_clusters_3$cluster, main="k=3")
# Cluster size
user_clusters_3$size
# Within Sum of Squares
user_clusters_3$withinss
# Between Sum of Squares
user_clusters_3$betweenss
# Total Sum of Squares
user_clusters_3$totss


set.seed(123)
user_clusters_4 <- kmeans(userTimerSparse_df, centers=4) 
plotcluster(userTimerSparse_df, user_clusters_4$cluster, main="k=4") 
user_clusters_4$size
user_clusters_4$withinss
# Between Sum of Squares
user_clusters_4$betweenss
# Total Sum of Squares
user_clusters_4$totss

set.seed(123)
user_clusters_5 <- kmeans(userTimerSparse_df, centers=5) 
plotcluster(userTimerSparse_df, user_clusters_5$cluster, main="k=5") 
user_clusters_5$size
user_clusters_5$withinss
# Between Sum of Squares
user_clusters_5$betweenss
# Total Sum of Squares
user_clusters_5$totss

set.seed(123)
user_clusters_6 <- kmeans(userTimerSparse_df, centers=6) 
plotcluster(userTimerSparse_df, user_clusters_6$cluster, main="k=6") 
user_clusters_6$size
user_clusters_6$withinss
# Between Sum of Squares
user_clusters_6$betweenss
# Total Sum of Squares
user_clusters_6$totss

set.seed(123)
user_clusters_7 <- kmeans(userTimerSparse_df, centers=7) 
plotcluster(userTimerSparse_df, user_clusters_7$cluster, main="k=7") 
user_clusters_7$size
user_clusters_7$withinss
# Between Sum of Squares
user_clusters_7$betweenss
# Total Sum of Squares
user_clusters_7$totss

set.seed(123)
user_clusters_8 <- kmeans(userTimerSparse_df, centers=8) 
plotcluster(userTimerSparse_df, user_clusters_8$cluster, main="k=8")
user_clusters_8$size
user_clusters_8$withinss
# Between Sum of Squares
user_clusters_8$betweenss
# Total Sum of Squares
user_clusters_8$totss

set.seed(123)
user_clusters_9 <- kmeans(userTimerSparse_df, centers=9) 
plotcluster(userTimerSparse_df, user_clusters_9$cluster, main="k=9") 
user_clusters_9$size
user_clusters_9$withinss
# Between Sum of Squares
user_clusters_9$betweenss
# Total Sum of Squares
user_clusters_9$totss

##Method 2: Examine the betweenss and withinss ratios.
##Separation is determined by between sum of square / total sum of square k = 9 has the highest between sum of square which means most separation.
clusters3<- user_clusters_3$betweenss/user_clusters_3$totss
clusters4<- user_clusters_4$betweenss/user_clusters_4$totss
clusters5<- user_clusters_5$betweenss/user_clusters_5$totss
clusters6<- user_clusters_6$betweenss/user_clusters_6$totss
clusters7<- user_clusters_7$betweenss/user_clusters_7$totss
clusters8<- user_clusters_8$betweenss/user_clusters_8$totss
clusters9<- user_clusters_9$betweenss/user_clusters_9$totss

betweenss.metric <- c(clusters3, clusters4, clusters5, clusters6, clusters7, clusters8, clusters9)
print(betweenss.metric) # Look for a ratio that is closer to 1.


##Cluster Cohesion within sum of square / total sum of square ratio k = 9 has the lowest within sum of square which means most cohesion.
clusters3<- user_clusters_3$tot.withinss/user_clusters_3$totss
clusters4<- user_clusters_4$tot.withinss/user_clusters_4$totss
clusters5<- user_clusters_5$tot.withinss/user_clusters_5$totss
clusters6<- user_clusters_6$tot.withinss/user_clusters_6$totss
clusters7<- user_clusters_7$tot.withinss/user_clusters_7$totss
clusters8<- user_clusters_8$tot.withinss/user_clusters_8$totss
clusters9<- user_clusters_9$tot.withinss/user_clusters_9$totss

totwithinss <- c(clusters3, clusters4, clusters5, clusters6, clusters7, clusters8, clusters9)
print(totwithinss) # Look for a ratio that is closer to 1.


## Method 3: Using the "Elbow Method"

wss <- NULL
# for one cluster the withiness is equals to 85608131 (sum of variance on all items).
wss[1] <- (nrow(userTimerSparse_df)-1)*sum(apply(userTimerSparse_df, 2, var))
# test within ss from 2 to 9 clusters.
# elbow point at K = 3, 6
set.seed(123)
for (i in 1:9)
  wss[i] <- sum(kmeans(userTimerSparse_df, centers=i)$withinss)

# Within Sum of Squares plot
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares", 
     main = "Number of Clusters (k) versus Cluster Cohesiveness")


bss <- NULL
# for one cluster the betweeness is equals to 85608131 (sum of variance on all items).
bss[1] <- (nrow(userTimerSparse_df)-1)*sum(apply(userTimerSparse_df, 2, var))
# test between ss from 2 to 9 clusters.
# elbow point at K = 3, 6
set.seed(123)
for (i in 1:9) 
  bss[i] <- sum(kmeans(userTimerSparse_df, centers=i)$betweenss)
# Between Sum of Squares plot
plot(1:9, bss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")



## Method 4: Using pseudo-F statistic

## K = 3 has the larest pseudo-F statistic

#help(index.G1) #read the ../doc/indexG1_details.pdf
a<-index.G1(userTimerSparse_df, user_clusters_3$cluster, centrotypes = "centroids") 
b<-index.G1(userTimerSparse_df, user_clusters_4$cluster, centrotypes = "centroids") 
c<-index.G1(userTimerSparse_df, user_clusters_5$cluster, centrotypes = "centroids") 
d<-index.G1(userTimerSparse_df, user_clusters_6$cluster, centrotypes = "centroids")
e<-index.G1(userTimerSparse_df, user_clusters_7$cluster, centrotypes = "centroids")
f<-index.G1(userTimerSparse_df, user_clusters_8$cluster, centrotypes = "centroids")
g<-index.G1(userTimerSparse_df, user_clusters_9$cluster, centrotypes = "centroids")

pseudoF<-c(a,b,c,d,e,f,g)
pseudoF


## Pick k = 3
## Creating an Aggregate Profile for Our Clusters
user_clusters_3$size #Get the size of each cluster
Clusters_3<-data.frame(t(user_clusters_3$centers)) #Transpose for easier reading
Clusters_3
## We can sort the centroids for each cluster.
Clusters_3[order(-Clusters_3$X1), ] 
Clusters_3[order(-Clusters_3$X2), ]
Clusters_3[order(-Clusters_3$X3), ]

##Cluster Profiles
##Segment 1 (9120): Casual users. loading component 14 times per week.
##Segment 2 (320): Heavy users/utilization review nurses. use utilization review worklist 34 times per week, write Interqual review 21 times per week, send fax 10 times per week.
##Segment 3 (1953): Medium ussage/discharge nurses. Use discharge worklist 10 times per week, more than um worlist, use discharge app curaspan 7 times per week.


userTimerSparse_df$cluster <- user_clusters_3$cluster

# mean launch um worklist by cluster
aggregate(data = userTimerSparse_df, `CAP:RCM_UM_WORKLIST.01` ~ cluster, mean)
# mean launch curaspan by cluster
aggregate(data = userTimerSparse_df, `CAP:MPG.RCM.POST-ACUTE-CURASPAN` ~ cluster, mean)


# 4. PCA and clustering by user timer frequencies
userTimerSparse_df_scaled <- scale(userTimerSparse_df)

p <- prcomp(userTimerSparse_df_scaled, scale=TRUE, center=TRUE)

summary(p)

loadings<-p$rotation[,]
View(loadings)

p.variance.explained <-(p$sdev^2 / sum(p$sdev^2))*100
print(p.variance.explained) 

## plot percentage of variance explained for each principal component    
barplot(p.variance.explained, las=2, xlab="Principal Component", ylab="% Variance Explained", main="Principal Components versus Percent of Variance Explained")

screeplot(p,type="line")

hc_tree<-hclust(dist(p$x[,1:2]), method="complete") # 1:2 = based on 2 components
plot(hc_tree)
groups <- cutree(hc_tree, k=3) 

userTimerSparse_df$clusterP<-groups #writing out the cluster assignment for each movie

table(userTimerSparse_df$clusterP)


## mean launch um worklist by cluster
aggregate(data = userTimerSparse_df, `CAP:RCM_UM_WORKLIST.01` ~ clusterP, mean)
## mean launch curaspan by cluster
aggregate(data = userTimerSparse_df, `CAP:MPG.RCM.POST-ACUTE-CURASPAN` ~ clusterP, mean)



# Decision Tree on user and eng timers
userTimerNames <- timerNames[grepl("USR*", timerNames)]
userTimerNames

usr_timer <- timer[which(timer$TIMERNAME %in% userTimerNames),]
dim(usr_timer)
unique(usr_timer$TIMERNAME)
summary(usr_timer)
#aggr_plot <- aggr(usr_timer, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(usr_timer), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
usr_timer$METADATA1 = NULL
usr_timer$METADATA2 = NULL
usr_timer$METADATA3 = NULL
usr_timer$FRAMEWORKVERSION = NULL
usr_timer$PROPERTYVALUES = NULL
usr_timer$START_OFFSET = NULL
usr_timer$START_OFFSET = NULL
usr_timer$STOP_OFFSET = NULL
usr_timer$THREADID = NULL
usr_timer$RESULTCOUNT = NULL
usr_timer$TIMER_ID = NULL
usr_timer$VERSION = NULL
usr_timer$ENTERPRISE = NULL
usr_timer$CLIENT = NULL
usr_timer$CLIENTNAME = NULL
usr_timer$CLIENTIP = NULL
usr_timer$USERPRINCIPALS = NULL
usr_timer$NODEIP = NULL
usr_timer$PROCESSID <- as.numeric(usr_timer$PROCESSID)

aggregate(data = usr_timer, ELAPSEDTIME ~ TIMERNAME, mean, na.rm = TRUE) 
summary(usr_timer$ELAPSEDTIME)
## remove outliers
usr_timer <- usr_timer[which(usr_timer$ELAPSEDTIME>0 & usr_timer$ELAPSEDTIME<=200),]
## run out of memory with too large data
usr_timer <- usr_timer[1:1000,]
dim(usr_timer)

usr_timer$SLOWNESS <- ifelse(usr_timer$ELAPSEDTIME>2,'yes','no')
usr_timer$SLOWNESS <- as.factor(usr_timer$SLOWNESS)
usr_timer$ELAPSEDTIME <- NULL

usr_timer$dateTime <- strptime(usr_timer$ENDDATETIME, "%d-%b-%Y %H:%M:%S")
usr_timer$DAY <- as.numeric(format(as.Date(usr_timer$ENDDATETIME,format="%d-%b-%Y"), "%d"))
usr_timer$ENDTIMEINMINUTE <- (usr_timer$DAY - 16) * 1440 + hour(usr_timer$dateTime) * 60 + minute(usr_timer$dateTime)
usr_timer$DAY <- NULL
usr_timer$dateTime <- NULL
usr_timer$ENDDATETIME <- NULL

set.seed(123)
trainIndex <- createDataPartition(usr_timer$SLOWNESS, p = .8,list = FALSE, times = 1)
timer_train <- usr_timer[ trainIndex,]
timer_validate <- usr_timer[ -trainIndex,]

set.seed(123)
timer_rpart <- rpart(timer_train$SLOWNESS~., method="class", parms = list(split="gini"), data=timer_train)
plot(as.party(timer_rpart))

# use a function to print result.
getResult<- function(myModel, predictType, testData)
{
  actual <- testData$SLOWNESS
  predicted <- predict(myModel, testData, type=predictType)
  results.matrix <- confusionMatrix(predicted, actual, positive="yes")
  print(results.matrix)
}
getResult(timer_rpart, "class", timer_validate)

cptable<-printcp(timer_rpart)
cptable
set.cp.value<-cptable[which.min(cptable[,"xerror"]),"CP"]
pruned_timer_rpart <- prune(timer_rpart, cp=set.cp.value)
plot(as.party(pruned_timer_rpart))
getResult(pruned_timer_rpart, "class", timer_validate)


# 10-fold cross validation repeat 5 times
fitControl <- trainControl(method="cv", number=10, repeats=5) 
set.seed(123)
timer_10folds<-train(SLOWNESS~., data=timer_train, method="rpart", metric="Accuracy", trControl=fitControl)
plot(as.party(timer_10folds$finalModel))
getResult(timer_10folds, "raw", timer_validate)


# Bagging
fitControl <- trainControl(method = "cv", number = 10) 
timer_treebag <- train(SLOWNESS~., data=timer_train, method = "treebag", metric="Accuracy", trControl=fitControl)
plot(as.party(timer_treebag$finalModel))
getResult(timer_treebag, "raw", timer_validate)


# Boosting
fitControl <- trainControl(method="cv", number=10) 
set.seed(123)
timer_boost<-train(SLOWNESS~., data=timer_train, method="adaboost", metric="Accuracy", trControl=fitControl)
plot(as.party(timer_boost$finalModel))
getResult(timer_boost, "raw", timer_validate)

# Naive Bayes
set.seed(123)
timer_nb <- naiveBayes(SLOWNESS~., data=timer_train)
nbPredicted <- predict(timer_nb, timer_validate)
nbConfusionMatrix <- confusionMatrix(nbPredicted, timer_validate$SLOWNESS, positive="yes")
print(nbConfusionMatrix)

timer_nb <- train(SLOWNESS~., data=timer_train, method = "nb", metric="Accuracy")
plot(as.party(timer_nb$finalModel))
getResult(timer_treebag, "raw", timer_validate)


# Neural Network
cvControl <- trainControl(method="cv", number=10)
set.seed(123)
timer_nn <- train(SLOWNESS~., data=timer_train, method = "nnet")
summary(timer_nn)
getResult(timer_nn, "raw", timer_validate)


# Logistic Regression
long_timer <- c("USR:MPG.RCM.CLINICAL_REVIEW - ADD CLINICAL REVIEW ADDENDUMS","USR:MPG.RCM.CLINICAL_REVIEW - ADD MANUAL CLINICAL REVIEW",
                "USR:MPG.RCM.CLINICAL_REVIEW - LOAD INTERQUAL CLINICAL REVIEW","USR:MPG.RCM.CLINICAL_REVIEW - MODIFY MANUAL CLINICAL REVIEW",
                "USR:MPG.RCM.CLINICAL_REVIEW - SEND CLINICAL FAX","USR:MPG.RCM.DISCHARGEREVIEWWORKLIST - GET ORDER DETAILS","USR:MPG.RCM.UTILIZATION_MANAGEMENT - LOAD COMPONENT",
                "USR:MPG.RCM.UTILIZATIONREVIEWWORKLIST - GET ORDER DETAILS")
slow_node <- c("BHSFLCTX32","BHSFLCTX69","EJEFLACTX66","FLHOFLCTX232","IHCUTCTX386","LACCACTX09","UTHDCHSMPCPA014")

timer_train$ENDTIMEINMINUTELONG <- ifelse(timer_train$ENDTIMEINMINUTE>=31,1,0)
timer_train$SLOW_TIMER <- ifelse(timer_train$TIMERNAME %in% long_timer,1,0)
timer_train$SLOW_NODE <- ifelse(timer_train$NODENAME %in% slow_node,1,0)
timer_logit_train <- timer_train[,c(6:10)]

timer_validate$ENDTIMEINMINUTELONG <- ifelse(timer_validate$ENDTIMEINMINUTE>=31,1,0)
timer_validate$SLOW_TIMER <- ifelse(timer_validate$TIMERNAME %in% long_timer,1,0)
timer_validate$SLOW_NODE <- ifelse(timer_validate$NODENAME %in% slow_node,1,0)
timer_logit_validate <- timer_validate[,c(6:10)]

fitControl <- trainControl(method = "cv", number = 10)
set.seed(123)
timer_lg <- train(SLOWNESS~., data=timer_logit_train, method="glm", family="binomial", trControl = fitControl)
summary(timer_lg)
getResult(timer_lg, "raw", timer_logit_validate)

#ROC 
#Create a ROC curve
library(ROCR)

R10Fold_predict_prob<-predict(timer_10folds, type="prob", timer_validate)
R10Fold_pred = prediction(R10Fold_predict_prob[,2], timer_validate$SLOWNESS)
R10Fold_perf = performance(R10Fold_pred,"tpr", "fpr") #true pos and false pos

nb_predict_prob<-predict(timer_nb, type="raw", timer_validate)
nb_pred = prediction(nb_predict_prob[,2], timer_validate$SLOWNESS)
nb_perf = performance(nb_pred,"tpr", "fpr") #true pos and false pos

RBagging_predict_prob<-predict(timer_treebag, type="prob", timer_validate)
RBagging_pred = prediction(RBagging_predict_prob[,2], timer_validate$SLOWNESS)
RBagging_perf = performance(RBagging_pred,"tpr", "fpr") #true pos and false pos

RBoost_predict_prob<-predict(timer_boost, type="prob", timer_validate)
RBoost_pred = prediction(RBoost_predict_prob[,2], timer_validate$SLOWNESS)
RBoost_perf = performance(RBoost_pred,"tpr", "fpr") #true pos and false pos

logit_predict_prob<-predict(timer_lg, type="prob", timer_validate)
logit_pred = prediction(logit_predict_prob[,2], timer_validate$SLOWNESS)
logit_perf = performance(logit_pred,"tpr", "fpr") #true pos and false pos
 
nnet_predict_prob<-predict(timer_nn, type="prob", timer_validate)
nnet_pred = prediction(nnet_predict_prob[,2], timer_validate$SLOWNESS)
nnet_perf = performance(nnet_pred,"tpr", "fpr") #true pos and false pos


# Start plotting
plot(R10Fold_perf, main="ROC Curves", col="black", lwd=2)
plot(nb_perf, add=TRUE, col="red", lwd=2)
plot(RBagging_perf, add=TRUE, col="green", lwd=2)
plot(RBoost_perf, add=TRUE, col="blue", lwd=2)
plot(logit_perf, add=TRUE, col="cyan", lwd=2)
plot(nnet_perf, add=TRUE, col="brown", lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
legend('bottomright', c("Decision Tree", "Random Forest","Bagging", "Boost", "Logistic Regression", "Neural Network"), 
       lty=c(1,1), lwd=c(2.5,1), col=c("black","red","green", "blue", "cyan", "brown"))



