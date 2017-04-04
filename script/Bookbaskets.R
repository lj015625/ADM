setwd(" ")

library(arules) #Note1
bookbaskets <- read.transactions("bookdata.tsv.gz", format="single",  	# Note2 
                                 sep="\t",                    	# Note3 
                                 cols=c("userid", "title"),    	# Note4 
                                 rm.duplicates=T)       	# Note5

# Note1: 
#   Load the arules package. 

# Note2: 
#   Specify the file and the file format. 

# Note3: 
#   Specify the column separator (a tab). 

# Note4: 
#   Specify the column of transaction IDs and of 
#   item IDs, respectively. 

# Note5: 
#   Tell the function to look for and remove 
#   duplicate entries (for example, multiple entries 
#   for “The Hobbit” by the same user). 

# 92,108 book purchases.
#220,447 user IDs.

inspect(bookbaskets[1:5]) #Examine the first five transactions

basketSizes<-size(bookbaskets) #Calculate number of books purchased by "userID"


bookFreq<-itemFrequency(bookbaskets) #Calculate the support for each book title 


bookCount <- (bookFreq/sum(bookFreq))*sum(basketSizes) 
#   Get the absolute count of book occurrences. 


bookbasket_use<-bookbaskets[basketSizes>1] #Only keep transactions with more than one book purchased. 

bookbasket_use
####
#start your code here.

