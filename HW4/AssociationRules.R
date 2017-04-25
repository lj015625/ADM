options(scipen=999)
getwd()
setwd("C:/Users/lj015625/Desktop/DataMining Class/HW4/data")

library(Matrix)
library(arules)
bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",  	# Note: 2 
                                 sep="\t",                    	# Note: 3 
                                 cols=c("userid", "title"),    	# Note: 4 
                                 rm.duplicates=T)       	# Note: 5

# Note 1: 
#   Load the arules package. 

# Note 2: 
#   Specify the file and the file format. 

# Note 3: 
#   Specify the column separator (a tab). 

# Note 4: 
#   Specify the column of transaction IDs and of 
#   item IDs, respectively. 

# Note 5: 
#   Tell the function to look for and remove 
#   duplicate entries (for example, multiple entries 
#   for "The Hobbit" by the same user). 

# 92,108 book purchases.
#220,447 user IDs.
inspect(bookbaskets[1:5]) #Examine the first five transactions
summary(bookbaskets);

# image(bookbaskets)
# image(bookbaskets[1:5])
# image(sample(bookbaskets, 100))


basketSizes<-size(bookbaskets) 
basketSizes

# Calculate the support for each book title 
bookFreq<-itemFrequency(bookbaskets)
# Get the absolute count of book occurrences. 
bookCount <- (bookFreq/sum(bookFreq))*sum(basketSizes) 

# High frequency items
orderedBooks = sort(bookCount, decreasing = TRUE)
head(orderedBooks, 10)
orderedBooks_df <- as.data.frame(orderedBooks)

#Let's plot the frequency of items
# Support number low because lower frequency on books inside dataset.
# Let's impose a rule. Let's say we only want to see items with at least 1% support
itemFrequencyPlot(bookbaskets, support = 0.003)
#Now let's say we only want to see "top 30" items (i.e. 30 items most frequently purchased)
itemFrequencyPlot(bookbaskets, topN = 30)


#### Apriori method
#Only keep transactions with more than one book purchased. 
bookbaskets_use<-bookbaskets[basketSizes>1] 
basketSizes<-size(bookbaskets_use) 

bookbasketrules <- apriori(bookbaskets_use, parameter = list(support = 0.001, confidence = 0.8, minlen = 2, maxlen = 10)) 
print(bookbasketrules)
summary(bookbasketrules)

#inspect(bookbasketrules[1:10]) 

# sort rules by lift and confidence
bookbasketrules_sorted <- sort(bookbasketrules, by = c("lift", "confidence"))
inspect(bookbasketrules_sorted)

bookbasketrules_sorted_df <- as(bookbasketrules_sorted, "data.frame")
bookbasketrules_sorted_df


library(arulesViz)
plot(bookbasketrules_sorted)

##A two-key plot
plot(bookbasketrules_sorted, shading="order", control=list(main="Two-key plot"))

## Grouped Matrix Plot
plot(bookbasketrules_sorted, method="grouped")
plot(bookbasketrules_sorted, method="grouped", control=list(k=30)) 


plot(bookbasketrules_sorted[1:10], method="graph")
plot(bookbasketrules_sorted[1:10], method="graph", control=list(type="itemsets"))

oraphBooks <- read.csv("oprahBookClub.csv")
oraphBooks_intersect <- intersect(rownames(orderedBooks_df), oraphBooks$Title)

# 2 Run Apriori oprah's book that existed in data set 
oprahRule_df_total_df <- data.frame(rules=factor(),
                                        support=double(),
                                        Factors=double(),
                                        lift=double(),
                                        stringsAsFactors=FALSE)
for (i in 1:length(oraphBooks_intersect)) {
  oprahRule <- apriori(bookbaskets_use, parameter = list(support = 0.00001, confidence = 0.6, minlen = 2, maxlen = 10),
                  appearance=list(lhs=oraphBooks_intersect[i], default="rhs")) 
  oprahRule_df <- as(oprahRule, "data.frame")
  if (length(oprahRule_df$rules) > 0) {
    oprahRule_df_total_df <- rbind(oprahRule_df, oprahRule_df_total_df)
  }
}
oprahRule_sorted_total_df <- sort(oprahRule_df_total_df, by = c("lift", "confidence"))

library(arulesViz)
plot(oprahRule_sorted_total_df)

#test
oprahRule <- apriori(bookbaskets_use, parameter = list(support = 0.00003, confidence = 0.6, minlen = 2, maxlen = 2),
                     appearance=list(lhs=c(oraphBooks_intersect[51]), default="rhs"))
oprahRule <- as(oprahRule, "data.frame")
print(oprahRule)

oprahRule1 <- apriori(bookbaskets_use, parameter = list(support = 0.00003, confidence = 0.6, minlen = 2, maxlen = 10),
                     appearance=list(lhs=c(oraphBooks_intersect[50]), default="rhs"))
oprahRule1 <- as(oprahRule1, "data.frame")
print(oprahRule1)

oprahRule_total <- rbind(oprahRule_total, oprahRule_df_total_df)
oprahRule_total[order(-oprahRule_total$lift), ]


oprahRule_df_total_df <- data.frame(rules=factor(),
                                    support=double(),
                                    Factors=double(),
                                    lift=double(),
                                    stringsAsFactors=FALSE)


# 3
bookbaskets_noAnimus <- apriori(bookbaskets_use, parameter = list(support = 0.001, confidence = 0.8, minlen = 2, maxlen = 10),
appearance = list(none = c("Wild Animus")))
bookbaskets_noAnimus_sorted <- sort(bookbaskets_noAnimus, by = c("lift", "confidence"))
inspect(bookbaskets_noAnimus_sorted)





#ECLAT
bookBasketEclatRules<-eclat(bookbaskets, parameter=list(supp=0.005, maxlen=10))
print(bookBasketEclatRules)
inspect(bookBasketEclatRules)


bookBasketEclatRulesReview <- ruleInduction(bookBasketEclatRules, bookbaskets)
bookBasketEclatRulesReview_sorted <- sort(bookBasketEclatRulesReview, by = c("lift", "confidence"))
inspect(ruleInduction)









