library(ggplot2)
library(tidyverse)
library(shiny)
library(DT)
library(tm)
library(wordcloud)
electron_data <- read.csv("../data/DatafinitiElectronicsProductData.csv", sep= ",", header= TRUE)
yx_select <- select(electron_data, name, reviews.date, reviews.rating)
yx_select <- yx_select[rowSums(is.na(yx_select)) == 0, ] #remove na rows
yx_select$reviews.date <- as.Date(yx_select$reviews.date)
yx_select$name <- abbreviate(yx_select$name, minlength = 2)
product_name <- unique(yx_select$name)
#electron_data %>% map_if(is.factor, as.character) %>% as_data_frame -> electron_data
# Define UI for application that draws a histogram

# generate word cloud
amazon_electronics_do_recommend <- electron_data[electron_data$reviews.doRecommend == TRUE, ]
#amazon_electronics_do_recommend_surfacepro <- amazon_electronics_do_recommend[amazon_electronics_do_recommend$asins == "B0168YIWSI", ]

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

#corpus_review <- Corpus(VectorSource(amazon_electronics_do_recommend_surfacepro$reviews.text))
#View(corpus_review)
#corpus_review <- tm_map(tm_map(tm_map(corpus_review, tolower), removePunctuation), removeWords, stop_words)

#review_dtm <- DocumentTermMatrix(corpus_review)
#review_tdm <- TermDocumentMatrix(corpus_review)
#review_m <- as.matrix(review_tdm)
#review_term_freq <- rowSums(review_m)
#review_term_freq <- sort(review_term_freq, decreasing = T)
#review_word_freq <- data.frame(term = names(review_term_freq),
#                               num = review_term_freq)
#wordcloud(review_word_freq$term, review_word_freq$num,
#          max.words = 30, colors = c("blue","darkgoldenrod","tomato"))



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Amazon Electronic Product"),
  
  navbarPage(
    "Navbar!", 
    tabPanel("Overview"),
    tabPanel("Product",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 #textInput("title", "Plot title:", value = "x v y"),
                 fluidRow(selectInput("select_product", "Select a Product:", choices = product_name, selected = "Microsoft Surface Pro 4 Type Cover with Fingerprint ID"),
                          div(style = "height:0px;"))
                 # fluidRow(radioButtons("plot_type", "Plot Type", choices = c("Time Series", "User Review Highlight"), selected = "Time Series"),
                 #          div(style = "height:50px;"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   id = "inTabset",
                   tabPanel(title = "Time Series", value = "panel1", 
                            fluidRow(plotlyOutput(outputId = "product_plot")),
                            
                            fluidRow(
                              class = "myRow2",
                              div(style = "margin-top:2em"),
                              column(width = 6, plotOutput(outputId = "pie")), 
                              column(width = 6, plotOutput(outputId = "bar"))
                            )
                   ),
                   tabPanel(title = "User Review Highlight", value = "panel2", plotOutput(outputId = "product_review_analysis"))
                 )
               )
             )),
    tabPanel("brand",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 fluidRow(selectInput("select_brand", "Select a Brand:", choices = brand_name, selected = "Microsoft"),
                          div(style = "height:0px;"))
               ),
               
               mainPanel(
                 tabsetPanel(
                   id = "inTabset2",
                   tabPanel(title = "Time Series", value = "brand_panel1", 
                            fluidRow(plotlyOutput(outputId = "brand_plot")),
                            
                            fluidRow(
                              div(style = "margin-top:2em"),
                              column(width = 6, plotOutput(outputId = "pie2")), 
                              column(width = 6, plotOutput(outputId = "bar2"))
                            )
                   ),
                   tabPanel(title = "User Review Highlight", value = "brand_panel2", plotOutput(outputId = "brand_review_analysis"))
                 )
               )
             )  
    ),
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
      geom_line() + stat_smooth()
    
    ggplotly(p, height = 400)
    
    
  })
  
  output$product_review_analysis <- renderPlot({
    
    hist(rnorm(100))
  })
  
  output$pie <- renderPlot({
    
    product_recommend <- electron_data[electron_data$name == input$select_product, 'reviews.doRecommend']
    product_recommend[is.na(product_recommend)] <- "NA"
    product_recommend <- data.frame(product_recommend)
    par(mar=c(0,0,0,0))
    #pie(table(product_recommend, exclude = NULL), col = c("pink", "grey", "light green"))
    p <- ggplot(product_recommend, aes(x='', fill=product_recommend)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y", start = 0) + theme(legend.position="top",axis.title=element_blank(),axis.ticks = element_blank())
    p 
    
  })
  
  output$bar <- renderPlot({
    
    
    par(mar=c(0,0,0,0))
    product_rating <- electron_data[electron_data$name == input$select_product, 'reviews.rating']
    product_rating <- product_rating[!is.na(product_rating)]
    product_rating <- data.frame(product_rating)
    p <- ggplot(product_rating, aes(x=product_rating)) +
      geom_bar(fill=unique(product_rating$product_rating))
    p 
    
  })
  
  #------------------------------------------------------------------------------#
  ##brand page 
  #------------------------------------------------------------------------------#
  output$brand_plot <- renderPlotly({
    selected = average_rating_select2[average_rating_select2$brand == input$select_brand,]
    
    p <- ggplot(selected,
                aes(x=month, y=average.rating)) +
      geom_line() + stat_smooth()
    
    ggplotly(p, height = 400)
  })
  
  output$pie2 <- renderPlot({
    brand_recommend <- electron_data[electron_data$brand == input$select_brand, 'reviews.doRecommend']
    brand_recommend[is.na(brand_recommend)] <- "NA"
    brand_recommend <- data.frame(brand_recommend)
    par(mar=c(0,0,0,0))
    p <- ggplot(brand_recommend, aes(x='', fill=brand_recommend)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y", start = 0) + theme(legend.position="top",axis.title=element_blank(),axis.ticks = element_blank())
    p
  })
  
  output$bar2 <- renderPlot({
    par(mar=c(0,0,0,0))
    brand_rating <- electron_data[electron_data$brand == input$select_brand, 'reviews.rating']
    brand_rating <- brand_rating[!is.na(brand_rating)]
    brand_rating <- data.frame(brand_rating)
    p <- ggplot(brand_rating, aes(x=brand_rating)) +
      geom_bar(fill=unique(brand_rating$brand_rating))
    p
  })
  
  
})

shinyApp(ui, server)





