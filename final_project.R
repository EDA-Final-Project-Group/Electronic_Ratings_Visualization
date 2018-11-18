#"""Create a list of common words to remove"""

# Reference: https://www.rdocumentation.org/packages/tm/versions/0.7-5
amazon_electronics <- read.csv("../data/DatafinitiElectronicsProductData.csv", sep= ",", header= TRUE)
stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", 
            "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
            "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", 
            "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", 
            "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", 
            "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", 
            "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", 
            "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", 
            "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than",
            "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")
review_text <- amazon_electronics$reviews.text

library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)

corpus_review <- Corpus(VectorSource(amazon_electronics$reviews.text))
#View(corpus_review)
corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)
View(corpus_review)
#term_count <- table(unlist(strsplit(corpus_review, " ")))

review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
barplot(review_term_freq[1:20],horiz=TRUE, cex.names =0.8, las=2) 

review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))


#library(sentimentr)
#mytext <- c(
#  'do you like it?  But I hate really bad dogs',
#  'I am the best friend.',
#  "Do you really like it?  I'm not a fan",
#  "It's like a tree."
#)

## works on a character vector but not the preferred method avoiding the 
## repeated cost of doing sentence boundary disambiguation every time 
## `sentiment` is run.  For small batches the loss is minimal.
## Not run: 
#sentiment(mytext)
