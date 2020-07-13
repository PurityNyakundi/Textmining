library(tm)
data(acq)
tdm <- TermDocumentMatrix(acq)
nTerms(tdm)
head(acq)
findFreqTerms(tdm,30)
#Let's find out the words that correlate to word stock with the correlation of atleast 0.70
findAssocs(tdm, "stock", 0.70)
#The generated term document matrix may be huge. The size of our DTM is 50 X 2013 It says 96% of the rows are zero, that is, the majority of words will appear in a few documents. We can reduce the sparsity of the document forcomputational 
tdm  
#The output shows that 96% of the terms occur just a few times, thus the document term matrix becomes large even for a small sized data set. We need to remove the sparse terms:
  inspect(removeSparseTerms(tdm, 0.3))
  library(tm)
  # convert to lower case
  acq <- tm_map(acq, content_transformer(tolower))
  #remove whitespaces
  acq <- tm_map(acq, stripWhitespace)
  #remove stop words(english)
  acq <- tm_map(acq, removeWords, stopwords("english"))
  
  