library(shiny)
library(datasets)
library(readxl)
library(foreign)
library(jmvReadWrite)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_source", "Select data source:", 
                  choices = c("Built-in datasets", "Upload your own data")),
      conditionalPanel(
        condition = "input.data_source == 'Built-in datasets'",
        selectInput("dataset", "Select dataset:", 
                    choices = c("mtcars", "iris", "CO2", "airquality",
                                "attitude", "BOD", "ChickWeight", "faithful",
                                "freeny", "freeny.x", "freeny.y", "InsectSprays",
                                "JohnsonJohnson", "LakeHuron", "lifeCycleSavings",
                                "longley", "Nile", "Orange", "OrchardSprays",
                                "PlantGrowth", "Puromycin", "quakes", "rock",
                                "sleep", "stack.loss", "swiss", "trees",
                                "USArrests", "warpbreaks", "women"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Upload your own data'",
        fileInput("file", "Upload your own data file", 
                  accept = c(".csv", ".xlsx", ".sav", ".dta", ".omv"))
      ),
      uiOutput("var_selectors")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotOutput("scatterplot")),
        tabPanel("Correlation Matrix", plotOutput("correlation_matrix")),
        id = "tool"
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  data <- reactive({
    if (input$data_source == "Built-in datasets") {
      get(input$dataset)
    } else {
      req(input$file)
      ext <- tools::file_ext(input$file$datapath)
      switch(ext,
             "csv" = read.csv(input$file$datapath, stringsAsFactors = FALSE),
             "xlsx" = read_excel(input$file$datapath),
             "sav" = read.spss(input$file$datapath, to.data.frame = TRUE),
             "dta" = read.dta(input$file$datapath),
             "omv" = read_omv(input$file$datapath)
      )
    }
  })
  
  output$var_selectors <- renderUI({
    choice <- names(data())
    tagList(
      conditionalPanel(
        condition = "input.tool == 'Scatterplot'",
        selectInput("x_var", "Select X variable:", choices = choice),
        selectInput("y_var", "Select Y variable:", choices = choice)
      ),
      conditionalPanel(
        condition = "input.tool == 'Correlation Matrix'",
        selectInput("vars", "Select variables:", choices = choice, multiple = TRUE)
      )
    )
  })
  
  selected_data <- reactive({
    if (input$tool == "Scatterplot") {
      data()[, c(input$x_var, input$y_var)]
    } else {
      data()[, input$vars]
    }
  })
  
  selected_data <- reactive({
    data()[, c(input$x_var, input$y_var)]
  })
  
  output$scatterplot <- renderPlot({
    data <- selected_data()
    x_var <- input$x_var
    y_var <- input$y_var
    
    # Check that the x and y variables are numeric and do not contain missing or infinite values
    if (!is.numeric(data[, x_var]) || any(is.na(data[, x_var])) || any(!is.finite(data[, x_var]))) {
      showNotification("Invalid x variable selected", type = "warning")
      return(NULL)
    }
    if (!is.numeric(data[, y_var]) || any(is.na(data[, y_var])) || any(!is.finite(data[, y_var]))) {
      showNotification("Invalid y variable selected", type = "warning")
      return(NULL)
    }
    
    # Plot the scatterplot
    plot(data[, x_var], data[, y_var],
         xlab = x_var, ylab = y_var)
  })
  
  output$correlation_matrix <- renderPlot({
    corr_matrix <- cor(data()[c(input$x_axis, input$y_axis)], use = "complete.obs")
    ggcorrplot(corr_matrix, type = "lower", method = "circle")
  })
  
}

# Run the app
shinyApp(ui, server)
