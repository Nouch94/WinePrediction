# Define UI
ui <- fluidPage(
  titlePanel("Wine Quality Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File:",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("plotType", "Select Visualization Type:",
                  choices = c("Correlation Plot", "Quality vs Feature Plot")),
      conditionalPanel(
        condition = "input.plotType == 'Quality vs Feature Plot'",
        uiOutput("featureSelect") 
      ),
      actionButton("refresh", "Refresh Plot")
    ),
    mainPanel(
      plotOutput("mainPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  
  output$featureSelect <- renderUI({
    req(dataset())
    features <- names(dataset()) %>% setdiff("quality")
    selectInput("selectedFeature", "Choose a Feature to Compare with Quality:", choices = features)
  })
  
  # Render Plot
  output$mainPlot <- renderPlot({
    input$refresh 
    winetrain <- dataset()
    
    if (input$plotType == "Correlation Plot") {
      # Correlation Plot
      numeric_data <- winetrain %>%
        select(where(is.numeric)) %>%
        filter_all(all_vars(!is.infinite(.) & !is.na(.)))
      
      if (ncol(numeric_data) >= 2) {
        cor_matrix <- cor(numeric_data)
        corrplot::corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.8,
                           title = "Correlation Matrix")
      } else {
        showNotification("Not enough numeric columns for correlation plot.", type = "error")
      }
      
    } else if (input$plotType == "Quality vs Feature Plot") {
      # Quality vs Selected Feature
      req(input$selectedFeature)
      feature <- input$selectedFeature
      
      if (is.numeric(winetrain[[feature]])) {
        # Scatter plot for numeric features
        ggplot(winetrain, aes_string(x = feature, y = "quality")) +
          geom_point(color = "Maroon", alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE, color = "blue") +
          labs(title = paste("Quality vs", feature),
               x = feature, y = "Quality") +
          theme_minimal()
      } else {
        # Boxplot for categorical features
        ggplot(winetrain, aes_string(x = feature, y = "quality", fill = feature)) +
          geom_boxplot() +
          scale_fill_viridis_d() +
          labs(title = paste("Quality by", feature),
               x = feature, y = "Quality") +
          theme_minimal()
      }
    }
  })
  
  # Render Summary Table
  output$summaryTable <- renderTable({
    req(input$file)
    winetrain <- dataset()
    
    if (input$plotType == "Correlation Plot") {
      numeric_data <- winetrain %>% select(where(is.numeric)) %>% na.omit()
      round(cor(numeric_data), 2)
    } else if (input$plotType == "Quality vs Feature Plot") {
      req(input$selectedFeature)
      winetrain %>%
        group_by(.data[[input$selectedFeature]]) %>%
        summarise(Average_Quality = mean(quality, na.rm = TRUE)) %>%
        arrange(desc(Average_Quality))
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)