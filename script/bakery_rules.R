#playing with bakery data
library(arules)
library(arulesViz)
bake <- read.csv("")
names(bake)
bake <- bake[-51]
bake_basket <- as(as.matrix(bake), "transactions")
summary(bake_basket)

# examine the frequency of items. This is also called "support." 
itemFrequency(bake_basket)
#Putting the output above into a data frame so we can "view" it.
freq_bake_basket_data_frame <- as.data.frame(itemFrequency(bake_basket))
View(freq_bake_basket_data_frame)

#Let's pare this down a bit. Let's look at the first 15 items.
itemFrequency(bake_basket[, 1:15])

#Let's plot the frequency of items
itemFrequencyPlot(bake_basket) 
#Too confusing. Let's impose a rule. Let's say we only want to see items with at least 10% support
itemFrequencyPlot(bake_basket, support = 0.1)
#Now let's say we only want to see "top 20" items (i.e. 20 items most frequently purchased)
itemFrequencyPlot(bake_basket, topN = 20) #top 20

## Step 2: Training a model on the data ----
library(arules) #Finding the associations

#Let's explore the apriori() function in arules package. 
?apriori
#Notice the default parameter settings
#support = How often does the rule happen? Default support = 0.1 (or 10%)
#Agrawal, Imielinski, and Swami (1993) noted support is equivalent to "statistical significance"
#confidence = How often is the rule correct? Default confidene = 0.8 (or 80%)
#Agrawal, Imielinski, and Swami (1993) noted confidence is the rule's strength
#maxlen = maximum number of items in a rule. Default maxlen = 10
#minlen = minimum number of items in a rule. Default minlen = 1

#Let's try the default settings first.
apriori(bake_basket) #Not a single rule found! 

#Let's try again. Support = 0.001. Confidence = 0.1. Minlen = 2. Feel free to change the parameter settings. 
bakerules <- apriori(bake_basket, parameter = list(support =
                                                      0.01, confidence = 0.5, minlen = 2)) 


#Count the number of rules found

summary(bakerules)

inspect(bakerules[1:5])
