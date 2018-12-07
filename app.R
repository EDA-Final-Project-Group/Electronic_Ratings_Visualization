library(shiny)

amazon_electronics <- read.csv("../data/DatafinitiElectronicsProductData.csv", sep= ",", header= TRUE)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "product", label = "Select a Brand", choices = amazon_electronics$name)
    ),
    mainPanel(
      plotOutput(outputId = "pie")
    )
  )
)

server <- function(input, output, session) {
  reactive_data <- reactive({
    product_recommend <- amazon_electronics[amazon_electronics$name == input$product, ]
    # if pie chart, need the following line
    #product_recommend <- amazon_electronics[amazon_electronics$name == input$product, 'reviews.doRecommend']
    #product_recommend[is.na(product_recommend)] <- "NA"
    return (product_recommend)
  })
  
  output$pie <- renderPlot({
    d <- reactive_data()
    # pie chart
    #pie(table(d, exclude = NULL))
    # bar chart
    ggplot(d, aes(x=factor(reviews.doRecommend)))+
      geom_bar(width=0.5, fill="steelblue")+
      theme_minimal()
  })
}

shinyApp(ui, server)

#for (name in unique(amazon_electronics$name)) {
#  print(name)
#  print(table(amazon_electronics[amazon_electronics$name == name, 'reviews.doRecommend']))
#}

