

library(DT)
library(glue)
library(tidyverse)
library(blockrand)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Application"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Select input", tabName = "input", icon = icon("input")),
      menuItem("Data Summary", tabName = "summary", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                fileInput("file", "Choose CSV File", accept = ".csv"),
                actionButton("uploadBtn", "Upload")
              )
      ),
      tabItem(tabName = "input",
              fluidRow(
                column(6,
                       selectInput("selectcol", "Select Column 1", choices = NULL),
                       numericInput("number", "Enter a Number", value = 100, min = 1),
                       actionButton("inputBtn", "Submit")
                )
              )
      ),
      
      tabItem(tabName = "summary",
              fluidRow(
                column(1, offset = 0,
                       actionButton("downloadBtnDev", "Developments", icon = icon("download"))),
                column(1, offset = 2,
                       actionButton("downloadBtnPro", "Production", icon = icon("download")))
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
  
  # ----- Randomization function ----------
  
  randomization_function <- function(df1, category_column, n) {
    
    # remove randomization group from df1 data frame
    df11 <- df1 |> select(- randomization_group)
    
    # Create a list of unique values in each column of the df11 data frame. 
    # This is needed to get all possible combination of values of stratification variables
    unique_val <- lapply(df11, unique) 
    
    # this function creates a data frame with all possible combination of statification variables
    all_possible_outcome <- expand.grid(unique_val)
    
    # Create a data frame with a stratum code for the possible combination of stratification variable
    # Stratum frame -----------------------------------------------------------
    
    df <- all_possible_outcome |>
      unite(col = "stratum",remove = F)
    
    # unique categories, number of categories, block size 
    unique_categories = pull(unique(df1[{{category_column}}]))
    num_categories = length(unique_categories)
    block_size = seq(num_categories, 5*(num_categories), num_categories)
    
    data <- data.frame()
    for (i in 1:nrow(df)) {
      block <-  blockrand(
        n = n,                       # total sample size of trial
        num.levels = num_categories, # three treatments
        levels = unique_categories,  # arm names
        stratum = df[i,1],           # stratum name
        id.prefix = "",              # stratum abbrev
        block.sizes = block_size,    # times arms = 3,6,9
        block.prefix = df[i,1]       # stratum abbrev
      )
      data <- rbind(data, block)
    }
    final_df <- left_join(df, data, by="stratum") |> 
      select(treatment, colnames(all_possible_outcome)) |> 
      rename({{category_column}} := treatment)
    
    return(final_df)
  }
  
  # -------------- Data Process ------------------- 
  
  # Render the uploaded data as a datatable
  output$data_table <- renderDataTable({
    req(input$number,input$selectcol)
    dp <- data()  # read data frame
    
    # last column name detect and reform
    last_col_name <- names(dp)[ncol(dp)]
    dp |> mutate(!!last_col_name := as.numeric(
      str_extract(!!sym(last_col_name), "\\d+"))) -> dp

    set.seed(99)
    final_df <- randomization_function(dp, input$selectcol, input$number)
      # binds(df, input$number, input$selectcol, all_possible_outcome)
    data.frame(final_df)
    
  })
  
}

shinyApp(ui, server)
