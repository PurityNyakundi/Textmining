library(readr)
Womens <- read_csv("Womens Clothing E-Commerce Reviews.csv")
#View(Womens_Clothing_E_Commerce_Reviews)
head(Womens)
str(Womens)
#recommendation is either 1 or 0
#change it to factor
library(tidyverse)

names(Womens)
names(Womens)[7]<-"recommender"
names(Womens)
names(Womens)[5]<-"Review"
Womens1<-select(Womens,c("Review","Rating","recommender"))
head(Womens1)
library(tm)
review_corpus <- VCorpus(VectorSource(Womens1$Review))
print(review_corpus)
inspect(review_corpus[1:3])
lapply(review_corpus[1:4],as.character)
#start cleaning
corpus_clean <- tm_map(review_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
lapply(corpus_clean[1:4],as.character)
#remove stop words
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
as.character(corpus_clean[[7]])

corpus_clean <- tm_map(corpus_clean, stripWhitespace)
lapply(corpus_clean[1:4],as.character)
#sms_dtm <- DocumentTermMatrix(corpus_clean)
review_dtm <- DocumentTermMatrix(corpus_clean)

review_dtm2 <- DocumentTermMatrix(review_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
Womens1$recommender<-as.factor(Womens1$recommender)
str(Womens1)
colSums(is.na(Womens1))
Womens1<-Womens1%>%
  drop_na()
head(Womens1)
review_dtm_train <- review_dtm[1:2000, ]
review_dtm_test <- review_dtm[2001:3000, ]
review_corpus_train <- corpus_clean[1:2000]
review_corpus_test <- corpus_clean[2001:3000]
review_train_raw <- Womens1[1:2000, ]
review_test_raw <- Womens1[2001:3000, ]
prop.table(table(review_train_labels))
#sms_dtm <- DocumentTermMatrix(corpus_clean)
wordcloud(review_corpus_train, min.freq = 40, random.order = FALSE)
library(wordcloud)
wordcloud(corpus_clean, min.freq = 50, random.order = FALSE)
wordcloud(words = corpus_clean, 
          min.freq = 100, # minimum number of times a word must be present before it appears 
          random.order = FALSE, # Arrange most frequent words to be in the center of the word cloud
          color = (colors = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027")) # Colour gradient for the font
)
review_freq_words <- findFreqTerms(review_dtm_train, 5)
str(review_freq_words)
review_train <- DocumentTermMatrix(review_corpus_train ,
                                list(dictionary = review_freq_words))
review_test <- DocumentTermMatrix(review_corpus_test,
                                 list(dictionary = review_freq_words))

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
review_train <- apply(review_train, MARGIN = 2, convert_counts)
review_test <- apply(review_test, MARGIN = 2, convert_counts)

library(naivebayes)
review_classifier <- naive_bayes(review_train, review_train_raw$recommender)
review_test_pred <- predict(review_classifier, review_test)
CrossTable(review_test_pred, review_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
library(caret)
confusionMatrix(review_test_pred,review_test_labels)
