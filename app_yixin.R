library(ggplot2)
library(tidyverse)
library(shiny)
library(DT)
electron_data <- read.csv("../data/DatafinitiElectronicsProductData.csv", sep= ",", header= TRUE)
yx_select <- select(electron_data, name, reviews.date, reviews.rating)
yx_select <- yx_select[rowSums(is.na(yx_select)) == 0, ] #remove na rows
yx_select$reviews.date <- as.Date(yx_select$reviews.date)
yx_select$name <- abbreviate(yx_select$name, minlength = 2)
product_name <- unique(yx_select$name)
#electron_data %>% map_if(is.factor, as.character) %>% as_data_frame -> electron_data
# Define UI for application that draws a histogram

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Amazon Electronic Product"),
  
  navbarPage(
    "Navbar!", 
    tabPanel("Product",
             sidebarLayout(
               sidebarPanel(
                 #textInput("title", "Plot title:", value = "x v y"),
                 selectInput("select_product", "Select a product:", choices = product_name, selected = "MSP4TCwFI"),
                 #selectInput("plot_type", "Select a Plot type:", choices = c("Time Series", "Product Health", "User Review Highlight"), selected = "Time Series")
                 radioButtons("plot_type", "radioButton", choices = c("Time Series", "Product Health", "User Review Highlight"), selected = "Time Series")
                 # sliderInput("bins",
                 #             "Number of bins:",
                 #             min = 1,
                 #             max = 50,
                 #             value = 30)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 #h1("First level title"),
                 #h2("Second level title"),
                 #h3("Average Rating Over Time"),
                 fluidRow(column(width = 12, 
                                 plotOutput(outputId = "time_series"))),
                 
                 # fluidRow(
                 #   column(width = 12,
                 #          verbatimTextOutput("hover_info")
                 #   )
                 # ),
                 
                 fluidRow(
                   column(width = 4, plotOutput(outputId = "pie1")), 
                   column(width = 4, plotOutput(outputId = "pie2")),
                   column(width = 4, plotOutput(outputId = "pie3"))
                 )
                 
                 #plotOutput("distPlot")
               ) 
             )),
    tabPanel("Brand"),
    tabPanel("Review Analysis"),
    tabPanel("Data",
             DT::dataTableOutput("data_table"),
             hover = hoverOpts(id ="plot_hover"),
             uiOutput("my_tooltip"))
  )
  
)
)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
server <- shinyServer(function(input, output) {
  
  data <- reactive({
    review_count_select <- yx_select %>% group_by(name, season = floor_date(reviews.date, unit = "season")) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
    selected = review_count_select[review_count_select$name == input$select_product,]
    return(selected)
  })
  
  
  output$time_series <- renderPlot({
    if (input$plot_type == "Time Series") {
      p <- ggplot(data(),
                  aes(x=season, y=freq, group=name, color=name)) +
        geom_line() + stat_smooth()
      return (p)
    }
    else  {
      return (hist(rnorm(100)))
    }
    #ggplotly(p)
  })
  
  
  output$pie1 <- renderPlot({
    product_recommend <- electron_data[electron_data$name == input$select_product, 'reviews.doRecommend']
    product_recommend[is.na(product_recommend)] <- "NA"
    pie(table(product_recommend, exclude = NULL), col = c("pink", "grey", "light green"))
  })
  
  
  options_new <- list(columnDefs = list(list(
    targets = "_all",
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 20 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
      "}")
  )))
  
  output$data_table = renderDataTable(electron_data,
                                      options = options_new)
  
  output$my_tooltip <- renderUI({
    verbatimTextOutput("hover_info")
  })
  
  output$hover_info <- renderPrint({
    hover <- input$plot_hover
    x = "test"
    x
  })
  
})

shinyApp(ui, server)

