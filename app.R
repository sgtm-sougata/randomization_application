library(DT)
library(glue)
library(tidyverse)
library(blockrand)
library(shinydashboard)
library(shinythemes)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Define UI
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Shiny Application"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Select input", tabName = "input", icon = icon("dashboard")),
      menuItem("Data Summary", tabName = "summary", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                column(6, offset = 3,
                       fileInput("file", "Upload allocation template .csv file", accept = ".csv"),
                       actionButton("uploadBtn", "Upload")
                       )
              )
      ),
      tabItem(tabName = "input",
              fluidRow(
                column(6, offset = 3,  # Adjust the offset to center the column
                       selectInput("selectcol", "Choose Randomize variable", choices = NULL),
                       numericInput("number", "Number of sample", value = 100, min = 1),
                       numericInput("blocksize", "Number of block", value = 2, min = 1),
                       actionButton("inputBtn", "Submit")
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                column(1, offset = 0,
                       downloadButton("downloadBtnDev", "Developments", icon = icon("download"))),
                column(1, offset = 2,
                       downloadButton("downloadBtnPro", "Production", icon = icon("download")))
              ),
              fluidRow(
                box(
                  title = "Data Table",
                  width = 12,
                  solidHeader = TRUE,
                  dataTableOutput("data_table")
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                # Add your analysis components here
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Handle CSV data upload
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  observeEvent(input$uploadBtn, {
    updateTabItems(session, "sidebar", "input")
  })
  observeEvent(data(), {
    updateSelectInput(session, "selectcol", choices = names(data()))
  })
  observeEvent(input$inputBtn, {
    updateTabItems(session, "sidebar", "summary")
  })
  
  # Randomization function
  randomization_function <- function(df1, category_column, n, block_size) {
    df11 <- df1 |> select(- randomization_group)
    unique_val <- lapply(df11, unique) 
    all_possible_outcome <- expand.grid(unique_val)
    df <- all_possible_outcome |>
      unite(col = "stratum", remove = FALSE)
    
    unique_categories = pull(unique(df1[{{category_column}}]))
    num_categories = length(unique_categories)
    blocks = seq(num_categories, block_size * (num_categories), num_categories)
    
    data <- data.frame()
    for (i in 1:nrow(df)) {
      block <- blockrand(
        n = n,
        num.levels = num_categories,
        levels = unique_categories,
        stratum = df[i, 1],
        id.prefix = "",
        block.sizes = blocks,
        block.prefix = df[i, 1]
      )
      data <- rbind(data, block)
    }
    final_df <- left_join(df, data, by = "stratum") |> 
      select(treatment, colnames(all_possible_outcome)) |> 
      rename({{category_column}} := treatment)
    
    return(final_df)
  }
  
  # Data processing
  processed_data <- reactive({
    req(input$selectcol, input$number, input$blocksize)
    dp <- data()
    last_col_name <- names(dp)[ncol(dp)]
    dp <- dp |> mutate(!!last_col_name := as.numeric(str_extract(!!sym(last_col_name), "\\d+")))
    
    set.seed(99)
    df_development <- randomization_function(
      dp, input$selectcol, input$number, input$blocksize
    )
    
    set.seed(123)
    df_production <- randomization_function(
      dp, input$selectcol, input$number, input$blocksize
    )
    
    list(df_development = df_development, df_production = df_production)
  })
  
  # Render the uploaded data as a datatable
  output$data_table <- renderDataTable({
    req(input$selectcol, input$number, input$blocksize)
    df_development <- processed_data()$df_development
    data.frame(df_development)
  })
  
  # Download handler for Development button
  output$downloadBtnDev <- downloadHandler(
    filename = function() {
      paste("development_data.csv")
    },
    content = function(file) {
      write.csv(processed_data()$df_development, file, row.names = FALSE)
    }
  )
  
  # Download handler for Production button
  output$downloadBtnPro <- downloadHandler(
    filename = function() {
      paste("production_data.csv")
    },
    content = function(file) {
      write.csv(processed_data()$df_production, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
