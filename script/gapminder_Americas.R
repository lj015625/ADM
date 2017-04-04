#You will need to install gapminder and tidyverse packages. 

library(gapminder)
library(tidyverse)

#If you cannot install tidyverse, try installing dplyr package instead.

#Getting the data 
#dplyr package, which is included in tidyverse, allows for filter/select/rename 
# %>% is called a "pipe." Go ahead...google "pipe" and "piping" in R & dplyr.
gapminder_Americas<- gapminder %>% filter(continent=="Americas", year==2007) %>% select(country,lifeExp, gdpPercap) 

gapminder_Americas<-as.data.frame(gapminder_Americas)

#replacing row names
Americas<-gapminder_Americas[,-1]
rownames(Americas)<-gapminder_Americas[,1]


library(fastcluster) #makes hclust() runs faster
Americas_hclust <- hclust(dist(Americas), method="ward.D") 

#Looks up the hclust help file
?stats::hclust


#Dendrogram
plot(Americas_hclust, hang = -1) 
# hang is numeric scalar indicating how the height of leaves 
#should be computed from the heights of their parents
#ie I dropped it by -1 so I can the the X axis better.

# draw dendogram with red borders around 2 clusters 
# try 3 clusters to see the difference
rect.hclust(Americas_hclust, k=3, border="red")

#try the "rule of thumb" we discussed in class and see what you get.


#use the Americas data frame to perform k-medoids cluster analysis




