#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
  
library(tidyverse)
library(lubridate)
library(plotly)
library(tm)
library(wordcloud)
electron_data <-
  read.csv(file="DatafinitiElectronicsProductData.csv",
           header=TRUE, sep=",")

# electron_data <- 
#   read.csv(file="../data/DatafinitiElectronicsProductData.csv", 
#                              header=TRUE, sep=",")

colSums(is.na(electron_data))

yx_select <- select(electron_data, name, brand, reviews.date, reviews.rating)
yx_select <- yx_select[rowSums(is.na(yx_select)) == 0, ] #remove na rows
yx_select$reviews.date <- as.Date(yx_select$reviews.date)
library(ggplot2)

library(tidyverse)
library(shiny)
library(DT)
library(dplyr)

product_name <- unique(yx_select$name)
brand_name <- unique(electron_data$brand)

average_rating_select <- yx_select %>% group_by(name, month = floor_date(reviews.date, unit = "month")) %>% summarise(average.rating = mean(reviews.rating, na.rm=TRUE))

average_rating_select2 <- yx_select %>% group_by(brand, month = floor_date(reviews.date, unit = "month")) %>% summarise(average.rating = mean(reviews.rating, na.rm=TRUE))


#sentiment score analysis for brand
library(reshape2)
library(sentimentr)
library(plotly)
sen_text <- get_sentences(as.character(electron_data$reviews.text))
sen_text <- sentiment_by(sen_text)

electron_data_with_sentiment <- electron_data
electron_data_with_sentiment['sentiment.score'] <- sen_text$ave_sentiment

summarise_table_0 <- electron_data_with_sentiment %>% 
  select(brand, reviews.doRecommend, reviews.rating, sentiment.score)%>% 
  na.omit() %>%
  group_by(brand) %>% 
  summarise(n = n(), average.rating = sum(reviews.rating)/n(), average.sentiment = sum(sentiment.score)/n(),    prop.recommend = sum(reviews.doRecommend)/n())

sentiment_plot_brand <- plot_ly(summarise_table_0, x = ~average.rating, y = ~average.sentiment, z = ~prop.recommend, size=4, text=~paste('Rating:',round(average.rating, 3),'<br>Sentiment:',round(average.sentiment, 3), '<br>Recommend:', round(prop.recommend, 3), '<br>Brand:', brand)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Rating'),
                      yaxis = list(title = 'Sentiment'),
                      zaxis = list(title = 'Do Recommend')))

#sentiment score analysis for product
summarise_table_0 <- electron_data_with_sentiment %>% 
  select(name, reviews.doRecommend, reviews.rating, sentiment.score)%>% 
  na.omit() %>%
  group_by(name) %>% 
  summarise(n = n(), average.rating = sum(reviews.rating)/n(), average.sentiment = sum(sentiment.score)/n(),    prop.recommend = sum(reviews.doRecommend)/n())

sentiment_plot_product <- plot_ly(summarise_table_0, x = ~average.rating, y = ~average.sentiment, z = ~prop.recommend,size=4, text=~paste('Rating:',round(average.rating, 3),'<br>Sentiment:',round(average.sentiment, 3), '<br>Recommend:', round(prop.recommend, 3), '<br>name:', substr(name, 1, 30))) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Rating'),
                      yaxis = list(title = 'Sentiment'),
                      zaxis = list(title = 'Do Recommend')))

#two y axis plot product
my_text = get_sentences(as.character(electron_data$reviews.text))
sen_text = sentiment_by(my_text)

electron_data$reviews.date <- as.Date(electron_data$reviews.date)


sentiment_df = data.frame(electron_data$name, electron_data$reviews.date,sen_text$ave_sentiment, electron_data$reviews.rating)

colnames(sentiment_df)[1]<-"name"
colnames(sentiment_df)[2]<-"review_date"
colnames(sentiment_df)[3]<-"text_scores"
colnames(sentiment_df)[4]<-"rating"

sentiment_df <- sentiment_df[rowSums(is.na(sentiment_df)) == 0, ] #remove na rows

#two y axis plot brand
sentiment_df_brand = data.frame(electron_data$brand, electron_data$reviews.date,sen_text$ave_sentiment, electron_data$reviews.rating)

colnames(sentiment_df_brand)[1]<-"brand"
colnames(sentiment_df_brand)[2]<-"review_date"
colnames(sentiment_df_brand)[3]<-"text_scores"
colnames(sentiment_df_brand)[4]<-"rating"

sentiment_df_brand <- sentiment_df_brand[rowSums(is.na(sentiment_df_brand)) == 0, ] #remove na rows

amazon_electronics_do_recommend <- electron_data[electron_data$reviews.doRecommend == TRUE, ]
amazon_electronics_donot_recommend <- electron_data[electron_data$reviews.doRecommend == FALSE, ]

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


#word cloud for product 
amazon_electronics_do_recommend <- electron_data[electron_data$reviews.doRecommend == TRUE, ]
amazon_electronics_donot_recommend <- electron_data[electron_data$reviews.doRecommend == FALSE, ]

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

#word cloud for brand 
brand_do_recommend <- electron_data[electron_data$reviews.doRecommend == TRUE, ]
brand_donot_recommend <- electron_data[electron_data$reviews.doRecommend == FALSE, ]

library(shinythemes)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = shinytheme("yeti"),
                        setBackgroundColor(
                          #color = c("#ffb93b", "#fff1d8"),
                          color = c("#cef8ed", "#b9cbff"),
                          gradient = "radial",
                          direction = c("top", "left")
                        ),
                        
                        # Application title
                        titlePanel("Amazon Electronic Product Data Analysis"),
                        
                        navbarPage(
                          "Menu", 
                          tabPanel("Overview", 
                                   p("Our analysis revolves around a dataset containing 7,000 online reviews for electronic products sold in Amazon, Walmart, BestBuy and other online platforms provided by ",span("Datafiniti's Product Database", style = "color:blue"),". Furthermore, the reviews cover 50 different electronic products under 38 different brands."),
                                   p("The challenge we face is to address a series of product-related key questions that helps explain how customer feedback could influence electronic product buying process via interactive visualization:"),
                                   em("1. What are the trends for these electronic products?"),br(),
                                   em("2. What is the correlation between ratings and reviews?"),br(),
                                   em("3. How is online reputation of different brands?"),br(),
                                   em("4. What is shared/different pattern of ratings among brands?"),br(),br(),
                                   p("A brief walkthrough of this app's sections is as following:"),
                                   h4("Tab: Product"),p(strong("SelectInput widget: "), "allows user can examine a specific product."),
                                   p(strong("General tab: "),"display (i) average rating of selected product given by users during different
                                     months; (ii) perentage of users who vote recommend, not recommend or NA; 
                                     (iii) distribution of 1-5 star ratings given by all users."),
                                   p(strong("User Review Highlight tab: "), "designed to give buyers some insight from other customers' review. Buyers can view product users' sentiment score and number of reviews over time; a word cloud that displays key words from these reviews; and top 10 reiews that other users think are most helpful."),
                                   h4("Tab: Brand"), p(strong("SelectInput widget: "), "allows user can examine a specific brand."),
                                   p(strong("General tab: "),"display (i) average rating of selected brand given by users during different
                                     months; (ii) perentage of users who vote recommend, not recommend or NA; 
                                     (iii) distribution of 1-5 star ratings given by all users."),
                                   p(strong("User Review Highlight tab: "), "designed to give buyers some insight from other customers' review of a certain brand. Buyers can view product users' sentiment score and number of reviews over time; a word cloud that displays key words from these reviews; and top 10 reiews that other users think are most helpful."),
                                   h4("Tab: Comparison"), p("Two 3-D plots displaying online image across all 35 brands and 50 products. The plot reveals information from 3 different angles: Rating, review sentiment score and percentage recommend. User can compare products or brands acccording to their relative position in the 3-d space. This also reveals what aspect the product/brand performs least satifiably so that it can draw more attention during product development."),
                                   h4("Tab: Data"), p("Display raw data if users want to look up a specfic review.")
                                   ),
                          tabPanel("Product",
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 3,
                                       #textInput("title", "Plot title:", value = "x v y"),
                                       fluidRow(selectInput("select_product", "Select a Product:", choices = product_name, selected = "Microsoft Surface Pro 4 Type Cover with Fingerprint ID"),
                                                div(style = "height:0px;"))
                                     ),
                                     
                                     # Show a plot of the generated distribution
                                     mainPanel(
                                       tabsetPanel(
                                         id = "inTabset",
                                         tabPanel(title = "General", value = "panel1", 
                                                  fluidRow(h3("Genenral Overview for Selected Product"),
                                                           h4("Popularity Over Time"),
                                                           plotlyOutput(outputId = "product_plot")),
                                                  
                                                  fluidRow(
                                                    class = "myRow2",
                                                    div(style = "margin-top:2em"),
                                                    column(width = 6, h4("User DoRecommend"), plotOutput(outputId = "pie")), 
                                                    column(width = 6, h4("Rating Distribution"), plotOutput(outputId = "bar"))
        
                                                  )
                                         ),
                                         tabPanel(title = "User Review Text Highlight", value = "panel2", 
                                                  fluidRow(
                                                    column(width = 6, h3("Positive Review"), plotOutput(outputId = "product_wordcloud_positive")), 
                                                    column(width = 6, h3("Negative Review"), plotOutput(outputId = "product_wordcloud_negative"))),
                                                  fluidRow(h3("User Review Text Sentimental Score Over Time"), plotOutput(outputId = "product_review_analysis")),
                                                  fluidRow(h3("Top 9 Helpful Comments"), dataTableOutput('product_helpful_comments'))
                                                 
                                         )
                                    
                                       )
                                     )
                                   )),
                          #------------------------------------------------------------------------------#
                          ##Brand UI
                          #------------------------------------------------------------------------------#
                          tabPanel("Brand",
                                   sidebarLayout(
                                     sidebarPanel(
                                       width = 3,
                                       fluidRow(selectInput("select_brand", "Select a Brand:", choices = brand_name, selected = "Microsoft"),
                                                div(style = "height:0px;"))
                                     ),
                                     
                                     mainPanel(
                                       tabsetPanel(
                                         id = "inTabset2",
                                         tabPanel(title = "General", value = "brand_panel1", 
                                                  fluidRow(h3("Genenral Overview for Selected Brand"),
                                                           h4("Popularity Over Time"),
                                                           plotlyOutput(outputId = "brand_plot")),
                                                  
                                                  fluidRow(
                                                    div(style = "margin-top:2em"),
                                                    column(width = 6, h4("User DoRecommend"), plotOutput(outputId = "pie2")), 
                                                    column(width = 6, h4("Rating Distribution"), plotOutput(outputId = "bar2"))
                                                  )
                                         ),
                                         tabPanel(title = "User Review Text Highlight", value = "brand_panel2", 
                                                  fluidRow(
                                                    column(width = 6, h3("Positive Review"), plotOutput(outputId = "brand_wordcloud_positive")), 
                                                    column(width = 6, h3("Negative Review"), plotOutput(outputId = "brand_wordcloud_negative"))),
                                                  fluidRow(h3("User Review Text Sentimental Score Over Time"), plotOutput(outputId = "brand_review_analysis")),
                                                  fluidRow(h3("Top 9 Helpful Comments"), dataTableOutput('brand_helpful_comments'))
                                         )
                                       )
                                     )  
                                   )),
                          tabPanel("Comparison",
                                   fluidRow(column(width = 12, 
                                                   h3("Comparison by Product"),
                                                   plotlyOutput(outputId = "product_comparison"),
                                                   h3("Comparison by Brand"),
                                                   plotlyOutput(outputId = "brand_comparison")
                                                   
                                   ))
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("data_table")
                                   #hover = hoverOpts(id ="plot_hover"),
                                   #uiOutput("my_tooltip"))
                          ))
                        
                          )
)
#------------------------------------------------------------------------------#
##server script
#------------------------------------------------------------------------------#
server <- shinyServer(function(input, output) {
  
  
  #------------------------------------------------------------------------------#
  ##product page 
  #------------------------------------------------------------------------------#
  output$product_plot <- renderPlotly({
    
    selected = average_rating_select[average_rating_select$name == input$select_product,]
    
    p <- ggplot(selected,
                aes(x=month, y=average.rating)) +
      geom_line() + stat_smooth(fill="mintcream") + xlab("Month") + ylab("Average Rating") + theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    
    ggplotly(p, height = 400)
    
    
  })
  
  output$product_review_analysis <- renderPlot({
    
    product = sentiment_df[sentiment_df$name == input$select_product,]
    tidy_table <- product %>% group_by(month = floor_date(review_date, unit = "month")) %>% summarise(ave_review_text_scores = sum(text_scores)/n(), ave_rating = sum(rating)/n())
    tidy_table = data.frame(tidy_table)
    p <- ggplot(tidy_table, aes(x = month))
    p <- p + geom_line(aes(y = ave_review_text_scores, colour = "Sentimental Scores"))
    p <- p + geom_line(aes(y = ave_rating/13, colour = "Ave_rating")) 
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*13, name = "Average Rate stars"))
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(y = "Sentimental Scores",
                  x = "Month",
                  colour = "Parameter")
    p <- p + theme(legend.position = c(0.8, 0.9)) + theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )
    p
  }, bg="transparent")
  
  output$product_wordcloud_positive <- renderPlot({
    amazon_electronics_do_recommend_product <- amazon_electronics_do_recommend[amazon_electronics_do_recommend$name == input$select_product, ]
    corpus_review <- Corpus(VectorSource(amazon_electronics_do_recommend_product$reviews.text))
    #View(corpus_review)
    corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)
    
    review_dtm <- DocumentTermMatrix(corpus_review)
    review_tdm <- TermDocumentMatrix(corpus_review)
    review_m <- as.matrix(review_tdm)
    review_term_freq <- rowSums(review_m)
    review_term_freq <- sort(review_term_freq, decreasing = T)
    review_word_freq <- data.frame(term = names(review_term_freq),
                                   num = review_term_freq)
    par(mar=c(0,0,0,0))
    wordcloud(review_word_freq$term, review_word_freq$num,
              max.words = 30, colors = c("blue","darkgoldenrod","tomato"))
  }, bg="transparent")
  
  output$product_wordcloud_negative <- renderPlot({
    amazon_electronics_donot_recommend_product <- amazon_electronics_donot_recommend[amazon_electronics_donot_recommend$name == input$select_product, ]
    corpus_review <- Corpus(VectorSource(amazon_electronics_donot_recommend_product$reviews.text))
    #View(corpus_review)
    corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)
    
    review_dtm <- DocumentTermMatrix(corpus_review)
    review_tdm <- TermDocumentMatrix(corpus_review)
    review_m <- as.matrix(review_tdm)
    review_term_freq <- rowSums(review_m)
    review_term_freq <- sort(review_term_freq, decreasing = T)
    review_word_freq <- data.frame(term = names(review_term_freq),
                                   num = review_term_freq)
    par(mar=c(0,0,0,0))
    wordcloud(review_word_freq$term, review_word_freq$num,
              max.words = 30, colors = c("blue","darkgoldenrod","tomato"))
  }, bg="transparent")
  
  output$product_helpful_comments<- renderDataTable(
    electron_data[electron_data$name == input$select_product, c("reviews.text", "reviews.numHelpful")] %>% arrange(desc(reviews.numHelpful)) %>% top_n(9)
  )
  
  output$pie <- renderPlot({
    
    product_recommend <- electron_data[electron_data$name == input$select_product, 'reviews.doRecommend']
    product_recommend[is.na(product_recommend)] <- "NA"
    product_recommend <- data.frame(product_recommend)
    par(mar=c(0,0,0,0))
    #pie(table(product_recommend, exclude = NULL), col = c("pink", "grey", "light green"))
    p <- ggplot(product_recommend, aes(x='', fill=product_recommend)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y", start = 0) + theme(legend.position="top",axis.title=element_blank(),axis.ticks = element_blank())+   theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    p 
    
  }, bg="transparent")
  
  output$bar <- renderPlot({
    par(mar=c(0,0,0,0))
    product_rating <- electron_data[electron_data$name == input$select_product, 'reviews.rating']
    product_rating <- product_rating[!is.na(product_rating)]
    product_rating <- data.frame(product_rating)
    p <- ggplot(product_rating, aes(x=product_rating)) +
      geom_bar(fill="chocolate") + theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    p 
    
  }, bg="transparent")
  
  #------------------------------------------------------------------------------#
  ##brand page 
  #------------------------------------------------------------------------------#
  output$brand_plot <- renderPlotly({
    selected = average_rating_select2[average_rating_select2$brand == input$select_brand,]
    
    p <- ggplot(selected,
                aes(x=month, y=average.rating)) +
      geom_line() + stat_smooth(fill="mintcream") + theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    
    ggplotly(p, height = 400)
  })
  
  output$brand_review_analysis <- renderPlot({
    
    product = sentiment_df_brand[sentiment_df_brand$brand == input$select_brand,]
    tidy_table <- product %>% group_by(month = floor_date(review_date, unit = "month")) %>% summarise(ave_review_text_scores = sum(text_scores)/n(), ave_rating = sum(rating)/n())
    tidy_table = data.frame(tidy_table)
    p <- ggplot(tidy_table, aes(x = month))
    p <- p + geom_line(aes(y = ave_review_text_scores, colour = "Sentimental Scores"))
    p <- p + geom_line(aes(y = ave_rating/13, colour = "Ave_rating"))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*13, name = "Average Rate stars"))
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(y = "Sentimental Scores",
                  x = "Month",
                  colour = "Parameter")
    p <- p + theme(legend.position = c(0.8, 0.9)) + theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )
    p
  }, bg="transparent")
  
  output$brand_wordcloud_positive <- renderPlot({
    do_recommend_product <- brand_do_recommend[brand_do_recommend$brand == input$select_brand, ]
    corpus_review <- Corpus(VectorSource(do_recommend_product$reviews.text))
    #View(corpus_review)
    corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)
    
    review_dtm <- DocumentTermMatrix(corpus_review)
    review_tdm <- TermDocumentMatrix(corpus_review)
    review_m <- as.matrix(review_tdm)
    review_term_freq <- rowSums(review_m)
    review_term_freq <- sort(review_term_freq, decreasing = T)
    review_word_freq <- data.frame(term = names(review_term_freq),
                                   num = review_term_freq)
    par(mar=c(0,0,0,0))
    wordcloud(review_word_freq$term, review_word_freq$num,
              max.words = 30, colors = c("blue","darkgoldenrod","tomato"))
  }, bg="transparent")
  
  output$brand_wordcloud_negative <- renderPlot({
    donot_recommend_brand <- brand_donot_recommend[brand_donot_recommend$brand == input$select_brand, ]
    corpus_review <- Corpus(VectorSource(donot_recommend_brand$reviews.text))
    #View(corpus_review)
    corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)
    
    review_dtm <- DocumentTermMatrix(corpus_review)
    review_tdm <- TermDocumentMatrix(corpus_review)
    review_m <- as.matrix(review_tdm)
    review_term_freq <- rowSums(review_m)
    review_term_freq <- sort(review_term_freq, decreasing = T)
    review_word_freq <- data.frame(term = names(review_term_freq),
                                   num = review_term_freq)
    par(mar=c(0,0,0,0))
    wordcloud(review_word_freq$term, review_word_freq$num,
              max.words = 30, colors = c("blue","darkgoldenrod","tomato"))
  }, bg="transparent")
  
  output$brand_helpful_comments<- renderDataTable(
    electron_data[electron_data$brand == input$select_brand, c("reviews.text", "reviews.numHelpful")] %>% arrange(desc(reviews.numHelpful)) %>% top_n(9)
  )
  
  output$pie2 <- renderPlot({
    brand_recommend <- electron_data[electron_data$brand == input$select_brand, 'reviews.doRecommend']
    brand_recommend[is.na(brand_recommend)] <- "NA"
    brand_recommend <- data.frame(brand_recommend)
    par(mar=c(0,0,0,0))
    p <- ggplot(brand_recommend, aes(x='', fill=brand_recommend)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y", start = 0) + theme(legend.position="top",axis.title=element_blank(),axis.ticks = element_blank())+ theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    p
  }, bg="transparent")
  
  output$bar2 <- renderPlot({
    par(mar=c(0,0,0,0))
    brand_rating <- electron_data[electron_data$brand == input$select_brand, 'reviews.rating']
    brand_rating <- brand_rating[!is.na(brand_rating)]
    brand_rating <- data.frame(brand_rating)
    p <- ggplot(brand_rating, aes(x=brand_rating)) +
      geom_bar(fill="chocolate")+ theme(
        panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    p
  }, bg="transparent")
  
  
  #------------------------------------------------------------------------------#
  ##comparison page 
  #------------------------------------------------------------------------------# 
  output$product_comparison <- renderPlotly({
    ggplotly(sentiment_plot_product)
  })
  
  output$brand_comparison <- renderPlotly({
    ggplotly(sentiment_plot_brand)
  })
  
  #------------------------------------------------------------------------------#
  ##data page 
  #------------------------------------------------------------------------------# 
  options_new <- list(columnDefs = list(list(
    targets = "_all",
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 20 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
      "}")
  )))
  
  output$data_table = renderDT(electron_data,
                               options = options_new, server = TRUE)
  
  
})

shinyApp(ui, server)





