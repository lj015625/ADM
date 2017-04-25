#You will need to install gapminder and tidyverse packages. 

library(gapminder)
library(tidyverse)
#If you cannot install tidyverse, try installing dplyr package instead.

#Getting the data 
#dplyr package, which is included in tidyverse, allows for filter/select/rename 
# %>% is called a "pipe." Go ahead...google "pipe" and "piping" in R & dplyr.
gapminder_Americas<- gapminder %>% filter(continent=="Americas", year==2007) %>% select(country, gdpPercap, lifeExp) 

gapminder_Americas<-as.data.frame(gapminder_Americas)

#replacing row names
names(gapminder_Americas)
Americas<-gapminder_Americas[,-1]
names(Americas);
# put gapminder_Americas$county for row names
rownames(Americas)<-gapminder_Americas$country


library(fastcluster) #makes hclust() runs faster
Americas_hclust <- hclust(dist(Americas), method="ward.D") 

#Looks up the hclust help file
?stats::hclust

# Different distance methods:
# Ward's minimum variance method aims at finding compact, spherical clusters. 
# option "ward.D2" implements that criterion (Murtagh and Legendre 2014). With the latter, the dissimilarities are squared before cluster updating. 
# The complete linkage method finds similar clusters. 
# The single linkage method (which is closely related to the minimal spanning tree) adopts a 'friends of friends' clustering strategy. 
# The other methods can be regarded as aiming for clusters with characteristics somewhere between the single and complete link methods. 

Americas_hclust <- hclust(dist(Americas), method="complete") 
Americas_hclust <- hclust(dist(Americas), method="single") 
Americas_hclust <- hclust(dist(Americas), method="average") 
Americas_hclust <- hclust(dist(Americas), method="ward.D2")
Americas_hclust <- hclust(dist(Americas), method="mcquitty") 
Americas_hclust <- hclust(dist(Americas), method="median") 
Americas_hclust <- hclust(dist(Americas), method="centroid") 

#Dendrogram
plot(Americas_hclust, hang = -1) 
# hang is numeric scalar indicating how the height of leaves 
#should be computed from the heights of their parents
#ie I dropped it by -1 so I can the the X axis better.

# draw dendogram with red borders around 2 clusters 
# try 3 clusters to see the difference
rect.hclust(Americas_hclust, k=3, border="red")
rect.hclust(Americas_hclust, k=4, border="red")
rect.hclust(Americas_hclust, k=5, border="red")
rect.hclust(Americas_hclust, k=6, border="red")

#try the "rule of thumb" we discussed in class and see what you get.
# The default rule of thumb is to prune the tree by the largest difference between two steps (i.e. nodes).
# Distance between Clusters: In kmeans and kmedoids, we measure distance between centroid (or medoid) and a data point. 
# In hierarchical clustering, we measure distance between groups of data points (i.e. clusters). 
# If we are going to look at distance between clusters, we need to decide HOW to do this. After all, 
# a cluster has multiple data points! The default method of measuring distance between clusters in the hclust() 
# function in R is "complete linkage," or the largest distance between a data point in one cluster and 
# a data point in another cluster.
#This gives us the height of each node in the dendrogram.
height <- Americas_hclust$height 
height
#This creates a vector with a 0 for the mininum height (bottom of tree) and without the highest height (1 cluster; top of tree)
# add first 0 and remove last item
height.2 <- c(0,height[-length(height)]) 
height.2
#Find the largest increase in distance
round(height-height.2,3)
#Find the step with the largest increase
max(round(height-height.2,3))
#The first branch from roof node has larest height (canada and US vs rest of countries)
#It seems the very last value on the list is the max height. Hence, we should have two clusters. (Remember we removed the option of having 1 cluster.)

#use the Americas data frame to perform k-medoids cluster analysis
library(wbstats)
head(Americas)
tail(Americas)
ggplot(Americas, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "blue", shape=1, size = 1) + theme_bw() + 
  xlab("GDP Per Capita") + ylab("Life Expetancy") +
  ggtitle("GDP Per Capita vs Life Expectancy") 

#Dissimilarity matrix contains the dissimilarity between the data points. Dissimilarity is also referred to as "distance." The default distance measure for function dist() in R is Euclidean. The function dist() is used to create the dissimilarity matrix.
#Size of the matrix is calculated as follows (n k) combation:

#n!/(2! * (n-2)!) = (n*n-1)/2
# factorial(25)/(2*factorial(23)) = n*(n-1)/2 = 25*(25-1)/2 = 300

library(cluster)

dissimilarity.matrix <- dist(as.matrix(Americas, method="euclidean"))
#dissimilarity.matrix
americas_pam <- pam(dissimilarity.matrix,3)
summary(americas_pam) #Look at the assigned cluster for each data value and it nearest neighboring cluster
plot(americas_pam) #Silhouette Plot




