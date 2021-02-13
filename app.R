library(shiny)

amazon_electronics <- read.csv("../data/DatafinitiElectronicsProductData.csv", sep= ",", header= TRUE)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "product", label = "Select a Brand", choices = amazon_electronics$name)
    ),
    mainPanel(
      fluidRow(column(width = 12, plotOutput(outputId = "pie4"))),
      fluidRow(
        column(width = 4, plotOutput(outputId = "pie")), 
        column(width = 4, plotOutput(outputId = "pie2")),
        column(width = 4, plotOutput(outputId = "pie3"))
      )
    )
  )
)

server <- function(input, output, session) {
  reactive_data <- reactive({
    product_recommend <- amazon_electronics[amazon_electronics$name == input$product, ]
    # if pie chart, need the following line
    product_recommend <- amazon_electronics[amazon_electronics$name == input$product, 'reviews.doRecommend']
    product_recommend[is.na(product_recommend)] <- "NA"
    return (product_recommend)
  })
  
  output$pie <- renderPlot({
    d <- reactive_data()
    # pie chart
    pie(table(d, exclude = NULL), col = c("pink", "grey", "light green"))
    # bar chart
    #ggplot(d, aes(x=factor(reviews.doRecommend)))+
    #  geom_bar(width=0.5, fill="steelblue")+
    #  theme_minimal()
  })
  
  output$pie2 <- renderPlot({
    d <- reactive_data()
    # pie chart
    pie(table(d, exclude = NULL), col = c("pink", "grey", "light green"))
  })
  
  output$pie3 <- renderPlot({
    d <- reactive_data()
    # pie chart
    pie(table(d, exclude = NULL), col = c("pink", "grey", "light green"))
  })
  
  output$pie4 <- renderPlot({
    d <- reactive_data()
    # pie chart
    pie(table(d, exclude = NULL), col = c("pink", "grey", "light green"))
    # bar chart
    #ggplot(d, aes(x=factor(reviews.doRecommend)))+
    #  geom_bar(width=0.5, fill="steelblue")+
    #  theme_minimal()
  })
}

shinyApp(ui, server)



