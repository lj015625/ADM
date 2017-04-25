options(scipen=999)
setwd("C:/Users/lj015625/Desktop/DataMining Class/HW3/data")
bakery<-read.csv("bakery-binary.csv",header=TRUE, sep=",")
dim(bakery)
names(bakery)
str(bakery)
summary(bakery)

set.seed(123)
bakery_clusters_4 <- kmeans(bakery, centers=4) 
# outputs from kmeans
names(bakery_clusters_4) 
# Size: Number of bakery items in each cluster.
bakery_clusters_4$size
#Let's show the coordinates of the cluster centroids for the interest variables
#bakery_clusters_4$centers 
t(bakery_clusters_4$centers)

set.seed(123)
bakery_clusters_5 <- kmeans(bakery, centers=5) 
bakery_clusters_5$size
#bakery_clusters_5$centers 
t(bakery_clusters_5$centers)

set.seed(123)
bakery_clusters_6 <- kmeans(bakery, centers=6) 
bakery_clusters_6$size
#bakery_clusters_6$centers 
t(bakery_clusters_6$centers)

set.seed(123)
bakery_clusters_7 <- kmeans(bakery, centers=7) 
bakery_clusters_7$size
#bakery_clusters_7$centers 
t(bakery_clusters_7$centers)

set.seed(123)
bakery_clusters_8 <- kmeans(bakery, centers=8) 
bakery_clusters_8$size
#bakery_clusters_8$centers 
t(bakery_clusters_8$centers)

#### Method 1: Use the visualizations 
library(fpc) 
plotcluster(bakery, bakery_clusters_4$cluster, main="k = 4")
plotcluster(bakery, bakery_clusters_5$cluster, main="k = 5")
plotcluster(bakery, bakery_clusters_6$cluster, main="k = 6")
plotcluster(bakery, bakery_clusters_7$cluster, main="k = 7")
plotcluster(bakery, bakery_clusters_8$cluster, main="k = 8")

#### Method 2: Examine the betweenss and withinss ratios.
# Within Sum of Squares
bakery_clusters_4$withinss
# Between Sum of Squares
bakery_clusters_4$betweenss
# Total Sum of Squares
bakery_clusters_4$totss

bakery_clusters_5$withinss
bakery_clusters_5$betweenss
bakery_clusters_5$totss

bakery_clusters_6$withinss
bakery_clusters_6$betweenss
bakery_clusters_6$totss

bakery_clusters_7$withinss
bakery_clusters_7$betweenss
bakery_clusters_7$totss

bakery_clusters_8$withinss
bakery_clusters_8$betweenss
bakery_clusters_8$totss

# Separation is determined by between sum of square / total sum of square  
# k = 8 has the highest between sum of square which means most separation.
clusters4<- bakery_clusters_4$betweenss/bakery_clusters_4$totss
clusters5<- bakery_clusters_5$betweenss/bakery_clusters_5$totss
clusters6<- bakery_clusters_6$betweenss/bakery_clusters_6$totss
clusters7<- bakery_clusters_7$betweenss/bakery_clusters_7$totss
clusters8<- bakery_clusters_8$betweenss/bakery_clusters_8$totss

betweenss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(betweenss.metric) # Look for a ratio that is closer to 1.

# Cluster Cohesion within sum of square / total sum of square ratio 
# k = 8 has the lowest within sum of square which means most cohesion.
clusters4<- bakery_clusters_4$tot.withinss/bakery_clusters_5$totss
clusters5<- bakery_clusters_5$tot.withinss/bakery_clusters_5$totss
clusters6<- bakery_clusters_6$tot.withinss/bakery_clusters_6$totss
clusters7<- bakery_clusters_7$tot.withinss/bakery_clusters_7$totss
clusters8<- bakery_clusters_8$tot.withinss/bakery_clusters_8$totss

totwithinss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(totwithinss.metric) #Looking for a ratio that is closer to 0. 

#### Method 3: Using the "Elbow Method"
# for one cluster the withiness is equals to 3483 (sum of variance on all items).
wss <- NULL
wss[1] <- (nrow(bakery)-1)*sum(apply(bakery, 2, var))
# test within ss from 2 to 8 clusters.
# elbow point at K = 5, 7
set.seed(123)
for (i in 1:9)
  wss[i] <- sum(kmeans(bakery, centers=i)$withinss)

plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares", 
     main = "Number of Clusters (k) versus Cluster Cohesiveness")

#Between sum of square  
# elbow point at K= 4, 6
bss <- NULL
bss[1] <- (nrow(bakery)-1)*sum(apply(bakery, 2, var))
set.seed(123)
for (i in 1:9) 
  bss[i] <- sum(kmeans(bakery, centers=i)$betweenss)
plot(1:9, bss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")

#### Method 4: Using pseudo-F statistic
# K = 5 has largest pseudo F-statistic
library(clusterSim)
help(index.G1) #read the ../doc/indexG1_details.pdf

a<-index.G1(bakery, bakery_clusters_4$cluster, centrotypes = "centroids") 
b<-index.G1(bakery, bakery_clusters_5$cluster, centrotypes = "centroids") 
c<-index.G1(bakery, bakery_clusters_6$cluster, centrotypes = "centroids")
d<-index.G1(bakery, bakery_clusters_7$cluster, centrotypes = "centroids")
e<-index.G1(bakery, bakery_clusters_8$cluster, centrotypes = "centroids")

pseudoF<-c(a,b,c,d,e)
pseudoF

library(fpc) 
bakery_clusters_optimal<-kmeansruns(bakery, krange=4:8) #finds the "best"" K between 5 and 8
bakery_clusters_optimal$bestk


### Creating an Aggregate Profile for Our Clusters

bakery_clusters_7$size #Get the size of each cluster

Clusters_7<-data.frame(t(bakery_clusters_7$centers)) #Transpose for easier reading
Clusters_7

#### We can sort the centroids for each cluster.
Clusters_7[order(-Clusters_7$X1), ] 
Clusters_7[order(-Clusters_7$X2), ]
Clusters_7[order(-Clusters_7$X3), ]
Clusters_7[order(-Clusters_7$X4), ]
Clusters_7[order(-Clusters_7$X5), ]
Clusters_7[order(-Clusters_7$X6), ]
Clusters_7[order(-Clusters_7$X7), ]

bakery$cluster <- bakery_clusters_5$cluster #adds the cluster number to each recond
aggregate(data=bakery, Weekend~cluster, mean) #frequency of weekend for each of five clusters.


### only use weekend
bakery_weekend <- bakery[which(bakery$Weekend==1),]
dim(bakery_weekend)
summary(bakery_weekend)
# percent of each items
bakery_weekend_sum <- as.data.frame(apply(bakery_weekend,2,mean))
bakery_weekend_sum

library(fpc) 
bakery_weekend_clusters_optimal<-kmeansruns(bakery_weekend, krange=4:8) #finds the "best"" K between 5 and 8
bakery_weekend_clusters_optimal$bestk

# 4 clusters
set.seed(123)
bakery_weekend_clusters_4 <- kmeans(bakery_weekend, centers=4) 
t(bakery_weekend_clusters_4$centers)
bakery_weekend_clusters_4$size #Get the size of each cluster

# 5 clusters
set.seed(123)
bakery_weekend_clusters_5 <- kmeans(bakery_weekend, centers=5) 
t(bakery_weekend_clusters_5$centers)
bakery_weekend_clusters_5$size #Get the size of each cluster

# 6 clusters
set.seed(123)
bakery_weekend_clusters_6 <- kmeans(bakery_weekend, centers=6) 
bakery_weekend_clusters_6$size
t(bakery_weekend_clusters_6$centers)
bakery_weekend_clusters_6$size #Get the size of each cluster

# 7 clusters
set.seed(123)
bakery_weekend_clusters_7 <- kmeans(bakery_weekend, centers=7) 
t(bakery_weekend_clusters_7$centers)
bakery_weekend_clusters_7$size #Get the size of each cluster

# 8 clusters
set.seed(123)
bakery_weekend_clusters_8 <- kmeans(bakery_weekend, centers=8) 
t(bakery_weekend_clusters_8$centers)
bakery_weekend_clusters_8$size #Get the size of each cluster

# Separation is determined by between sum of square / total sum of square  
# k = 4 has the highest between sum of square which means most separation.
clusters4<- bakery_weekend_clusters_4$betweenss/bakery_weekend_clusters_4$totss
clusters5<- bakery_weekend_clusters_5$betweenss/bakery_weekend_clusters_5$totss
clusters6<- bakery_weekend_clusters_6$betweenss/bakery_weekend_clusters_6$totss
clusters7<- bakery_weekend_clusters_7$betweenss/bakery_weekend_clusters_7$totss
clusters8<- bakery_weekend_clusters_8$betweenss/bakery_weekend_clusters_8$totss

betweenss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(betweenss.metric) # Look for a ratio that is closer to 1.

# Cluster Cohesion within sum of square / total sum of square ratio 
# k = 8 has the lowest within sum of square which means most cohesion.
clusters4<- bakery_weekend_clusters_4$tot.withinss/bakery_weekend_clusters_5$totss
clusters5<- bakery_weekend_clusters_5$tot.withinss/bakery_weekend_clusters_5$totss
clusters6<- bakery_weekend_clusters_6$tot.withinss/bakery_weekend_clusters_6$totss
clusters7<- bakery_weekend_clusters_7$tot.withinss/bakery_weekend_clusters_7$totss
clusters8<- bakery_weekend_clusters_8$tot.withinss/bakery_weekend_clusters_8$totss

totwithinss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(totwithinss.metric) #Looking for a ratio that is closer to 0. 

#### Method 3: Using the "Elbow Method"
# for one cluster the withiness is equals to 3483 (sum of variance on all items).
wss <- NULL
wss[1] <- (nrow(bakery_weekend)-1)*sum(apply(bakery_weekend, 2, var))
# test within ss from 2 to 8 clusters.
# elbow point at K = 5, 7
set.seed(123)
for (i in 1:9)
  wss[i] <- sum(kmeans(bakery_weekend, centers=i)$withinss)

plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares", 
     main = "Number of Clusters (k) versus Cluster Cohesiveness")

#Between sum of square  
# elbow point at K= 4, 6
bss <- NULL
bss[1] <- (nrow(bakery_weekend)-1)*sum(apply(bakery_weekend, 2, var))
set.seed(123)
for (i in 1:9) 
  bss[i] <- sum(kmeans(bakery_weekend, centers=i)$betweenss)
plot(1:9, bss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")

#### Using pseudo-F statistic
# K = 7 has largest pseudo F-statistic
library(clusterSim)
help(index.G1) #read the ../doc/indexG1_details.pdf
a<-index.G1(bakery_weekend, bakery_weekend_clusters_4$cluster, centrotypes = "centroids") 
b<-index.G1(bakery_weekend, bakery_weekend_clusters_5$cluster, centrotypes = "centroids") 
c<-index.G1(bakery_weekend, bakery_weekend_clusters_6$cluster, centrotypes = "centroids")
d<-index.G1(bakery_weekend, bakery_weekend_clusters_7$cluster, centrotypes = "centroids")
e<-index.G1(bakery_weekend, bakery_weekend_clusters_8$cluster, centrotypes = "centroids")
pseudoF<-c(a,b,c,d,e)
pseudoF


Clusters_8<-data.frame(t(bakery_weekend_clusters_8$centers)) #Transpose for easier reading
Clusters_8

bakery_weekend_clusters_8$size

Clusters_8[order(-Clusters_8$X1), ] 
Clusters_8[order(-Clusters_8$X2), ]
Clusters_8[order(-Clusters_8$X3), ]
Clusters_8[order(-Clusters_8$X4), ]
Clusters_8[order(-Clusters_8$X5), ] 
Clusters_8[order(-Clusters_8$X6), ]
Clusters_8[order(-Clusters_8$X7), ]
Clusters_8[order(-Clusters_8$X8), ]
