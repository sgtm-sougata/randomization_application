# Load the libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(blockrand)
library(tidyverse)
library(tidyr)
library(DT)
library(glue)


options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Define UI
ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title= div(h3('RGen'), 'Random Number Generator')
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "sidebar",
                        menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload")),
                        menuItem("Select input", tabName = "input", icon = icon("dashboard")),
                        menuItem("Data Table", tabName = "summary", icon = icon("table")),
                        menuItem("Summary", tabName = "analysis", icon = icon("chart-bar"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "upload",
                                fluidRow(
                                  column(12, offset = 1,
                                         style = "display: flex; align-items: center; justify-content: center; margin-top: 10vh",
                                         box(
                                           style = "padding-top: 25px",
                                           fileInput("file", "Upload allocation template .csv file", accept = ".csv"),
                                           actionButton("uploadBtn", "Upload")
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName = "input",
                                fluidRow(
                                  column(12, offset = 1, # Adjust the offset to center the column
                                         style = "display: flex; margin-top: 10vh; justify-content: center;",
                                         box(
                                           style = "padding-top: 25px",
                                           selectInput("selectcol", "Choose Randomize variable", choices = NULL),
                                           numericInput("number", "Number of sample", value = 100, min = 1),
                                           numericInput("blocksize", "Number of block", value = 2, min = 1),
                                           actionButton("inputBtn", "Submit")
                                         )
                                         
                                  )
                                )
                        ),
                        tabItem(tabName = "summary",
                                tabsetPanel(
                                  tabPanel("Development", 
                                           fluidRow(
                                             box(
                                               downloadButton("downloadBtnDev", "",icon = icon("download")),
                                               width = 12,
                                               solidHeader = TRUE,
                                               dataTableOutput("data_table_dev")
                                             )
                                           )
                                  ),
                                  tabPanel("Production", 
                                           fluidRow(
                                             box(
                                               downloadButton("downloadBtnPro", "", icon = icon("download")),
                                               width = 12,
                                               solidHeader = TRUE,
                                               dataTableOutput("data_table_pro")
                                             )
                                           )
                                  )
                                )
                        ),
                        tabItem(tabName = "analysis",
                                fluidRow(
                                  tabsetPanel(
                                    tabPanel("Development", 
                                             fluidRow(
                                               box(
                            
                                                 width = 12,
                                                 solidHeader = TRUE,
                                                 dataTableOutput("summary_table_dev")
                                               )
                                             )
                                    ),
                                    tabPanel("Production", 
                                             fluidRow(
                                               box(
                                                 
                                                 width = 12,
                                                 solidHeader = TRUE,
                                                 dataTableOutput("summary_table_pro")
                                               )
                                             )
                                    )
                                  )
                                )
                        )
                      ),
                      tags$footer(
                        style = "text-align: center; position: absolute; bottom: 0; width: 100%; padding-bottom: 10px; ",
                        "Randomization | Allocation",
                        br(),
                        "Developed and Maintained by Department of Radiation Oncology, Tata Medical Center, Kolkata",
                        br(),
                        br()
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
  
  # Render the uploaded data as a datatable for Development
  output$data_table_dev <- renderDataTable({
    req(input$selectcol, input$number, input$blocksize)
    df_development <- processed_data()$df_development
    data.frame(df_development)
  })
  
  # Render the uploaded data as a datatable for Production
  output$data_table_pro <- renderDataTable({
    req(input$selectcol, input$number, input$blocksize)
    df_production <- processed_data()$df_production
    data.frame(df_production)
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
  
  # output summary table development and production with component
  output$summary_table_dev <- renderDataTable({
    req(input$selectcol, input$number, input$blocksize)
    df_development <- processed_data()$df_development
    df_dev_summary <- df_development %>% group_by_all() %>% count()
    data.frame(df_dev_summary)
  })
  
  output$summary_table_pro <- renderDataTable({
    req(input$selectcol, input$number, input$blocksize)
    df_production <- processed_data()$df_production
    df_pro_summary <- df_production %>% group_by_all() %>% count()
    data.frame(df_pro_summary)
  })
}

# Run the application
shinyApp(ui, server)
