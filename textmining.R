library(udpipe)
library(lattice)
library(gridExtra)
library(grid)
electron_data <- 
  read.csv(file="../data/DatafinitiElectronicsProductData.csv", 
           header=TRUE, sep=",")
amazon_electronics_do_recommend <- electron_data[electron_data$reviews.doRecommend == TRUE, ]
amazon_electronics_donot_recommend <- electron_data[electron_data$reviews.doRecommend == FALSE, ]


# load model
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

# load dataset (positive reviews and negative reviews)
s_1 <- udpipe_annotate(udmodel_english, amazon_electronics_do_recommend$reviews.text)
s_2 <- udpipe_annotate(udmodel_english, amazon_electronics_donot_recommend$reviews.text)
x_1 <- data.frame(s_1)
x_2 <- data.frame(s_2)

word_extraction <- function(x, type, title) {
  if (type == "keyword"){
    # Automated keyword extraction
    stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                           relevant = x$upos %in% c("NOUN", "ADJ"))
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "#5CACEE", 
             main = title, 
             xlab = "importance score")
  }else if (type == "noun-verb") {
    # TOP NOUN-VERB Pairs as Keyword pairs
    x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
    stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                              is_regex = TRUE, detailed = FALSE)
    stats <- subset(stats, ngram > 1 & freq > 3)
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
             main = "Keywords - simple noun phrases", xlab = "Frequency")
  }else {
    ## NOUNS
    stats <- subset(x, upos %in% c("NOUN")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
             main = "Most occurring nouns", xlab = "Freq")
  }
    
}

l <- word_extraction(x_1, "keyword", "Top 10 Keywords in Positive Reviews")
r <- word_extraction(x_2, "keyword", "Top 10 Keywords in Negative Reviews")
grid.arrange(l, r, ncol=2)


### Finding
#In order to provide useful insight to product development, we need to look at what user says about these products. A straightforward way is to look at keywords and phrases frequently mentioned, in both positive and negative reviews. If we assume that the aspects of products appearing in positive reviews are the ones users tend to feel satistied about, whereas those in negative reviews are the ones users tend to complain about, then we can suggest items in these negative reviews for further improvement. 

#Practically we can examine keywords product by product to generate product-specific suggestions, but here we examine all products as a whole -- what users emphasize that could affect their experience with electronic products.  

#Top 5 electronic features that contribute to users' positve experience are blue tooth, passive radiator, finger print, best sounding and surge protector. 
#Top 5 electronic features that contribute to users negative expererience are noise cancelling, fingerprint reader, customer service, touch screen and listening experience. 
