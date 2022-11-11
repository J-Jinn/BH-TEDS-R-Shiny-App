#####################################################################################################
# BH-TEDS Project R Shiny App Prototype
# Author: Joseph Jinn
# E-mail: jinnjo@mail.gvsu.edu
#####################################################################################################

# TODO: Ability to limit features by specific users (authentication and levels of privileges?).
# TODO: Modify source visualization function to redirect selection of original vars to modified vars. 
# TODO: Filter by different quarters within the fiscal year and display as time-series data.

# Reference: https://deanattali.com/blog/advanced-shiny-tips/ 

library(shiny) # Shinay apps.
library(dplyr) # Data manipulation.
library(ggplot2) # Plotting.
library(roxygen2) # Function documentation.
library(stringr) # String manipulation functions.
library(shinycssloaders) # Waiting/loading animated icons.
library(shinyBS) # Add tooltips to GUI elements.
library(foreign) # Read SPSS files.

source("bh_teds.R") # Data visualization function.

#####################################################################################################
# Define global variables.
#####################################################################################################

# Note: "program" has "undeclared level(s)"  
bhteds_data <- readRDS("data/bhteds_data.RDS") # Data
bhteds_data_labels <- readRDS("data/bhteds_data_labels.RDS") # Data labels
# Get variables associated with labels.
bhteds_data_labels <- bhteds_data_labels %>%
  mutate(variable = row.names(bhteds_data_labels))
# Placeholder if variable labels are missing. 
bhteds_data_labels <- bhteds_data_labels %>% mutate(
  variable_label = ifelse(variable_label == "", variable, variable_label)
) 
# Convert variable labels to list format.
var_lab_list <- list()
for(i in 1:nrow(bhteds_data_labels)) {
  var_lab_list[bhteds_data_labels[i, 2]] <- bhteds_data_labels[i, 1]
}
# Create labels for variables without one (or modify them).
var_lab_list["anyopioid"] <- "Medication Assisted Opioid Treatment (any opioid)"
var_lab_list["daystx2"] <- "Days to Treatment Group"
var_lab_list["ageyr10"] <- "Age Group"
var_lab_list["adultchild"] <- "Adult/Child"
var_lab_list["educatGrp"] <- "Education"
var_lab_list["income2"] <- "Income Group"
# Initial selection of variables of interest. 
my_vars <- c("inttx", "srvset")
my_labs <- c("Integrated Tx", "Service Setting")

#####################################################################################################
# Define GUI.
#####################################################################################################
ui <- fluidPage(
  # Application title
  titlePanel("BH-TEDS Project Prototype App"),
  tabsetPanel(
    #####################################################################################################
    tabPanel(
      title = "BH-TEDS Demographic Visualizations",
      sidebarLayout(
        sidebarPanel(
          helpText("Note: Quantitative variables may require binning and Categorical variables may have too many levels."),
          
          popify(
            content = NULL,
            el = (fileInput(inputId = "select_file", 
                            label = "Upload SPSS Data (.sav file):", 
                            multiple = FALSE, 
                            accept = c(".sav"), 
                            width = "400px")), 
            title = "Select a SPSS data file (.sav) to upload to the app.", 
            placement = "top", 
            trigger = "hover"),
          
          bsPopover(id = "select_variables", 
                    content = NULL,
                    title = "Select variables of interest from the BH-TEDS data to generate plots and tables for.", 
                    placement = "top", 
                    trigger = "hover", options = list(container = "body")),
          
          bsPopover(id = "select_county", 
                    content = NULL, 
                    title = "Select county of interest to filter the BH-TEDS data by.", 
                    placement = "top", 
                    trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_variables"),
          uiOutput(outputId = "select_county"),
          
          bsPopover(id = "select_ref_county", 
                    content = NULL, 
                    title = "Select one or more reference county(ies) to compare against the selected county above.", 
                    placement = "top", 
                    trigger = "hover", options = list(container = "body")),
          
          bsPopover(id = "select_ref_county_stat", 
                    content = NULL, 
                    title = "Select the type of aggregate statistic to compute for the reference county(ies).", 
                    placement = "top", 
                    trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_ref_county"),
          
          selectInput(inputId = "select_ref_county_stat", 
                      label = "Reference County(ies) Stat Type:", 
                      width = "400px", 
                      choices = c("mean", "sum", "median"),
                      selected = "Barry", 
                      multiple = FALSE),
          
          bsPopover(
            id = "select_subset_variable", 
            content = NULL, 
            title = "Select a variable to subset the BH-TEDS data by (choose relational operation and level within the variable below).", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          bsPopover(
            id = "select_subset_operation", 
            content = NULL, 
            title = "Select the type of subset operation to perform (choose a relational operator).", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          bsPopover(
            id = "select_subset_variable_level", 
            content = NULL, 
            title = "Select a level within the subsetting variable to filter by (dynamically updates based on selected subset variable).", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_subset_variable"),
          
          selectInput(
            inputId = "select_subset_operation", 
            label = "Subset relational operation type:", 
            width = "400px", 
            choices = c("!=", "==", ">", "<", ">=", "<="), # Supported relational operators.
            selected = "==", 
            multiple = FALSE),
          # Reference: https://stackoverflow.com/questions/34929206/selectinput-that-is-dependent-on-another-selectinput
          selectInput(
            inputId = "select_subset_variable_level", 
            label = "Subset by this level within the variable:", 
            width = "400px", 
            choices = c("Dynamic Update: User selection"), # Level(s) within user-selected variable.
            selected = "Male", 
            multiple = FALSE),
          
          # FIXME: Can't seem to generate tooltip for actionButton() 
          popify(
            el = (actionButton(inputId = "action", label = "Generate Plots-Tables")), 
            content = NULL, 
            title = "Click to generate plots and tables based on current chosen settings. 
            Output dynamically resizes based on the number of plots + tables.", 
            placement = "top", 
            trigger = "hover"),
          
          bsPopover(
            id = "file_name", 
            content = NULL, 
            title = "Enter the name for the file to export to (excluding file extension).", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          textInput(
            inputId = "file_name", 
            label = "PDF File-Name:", 
            value = "default", 
            width = "400px"),
          
          bsPopover(
            id = "download_output", 
            content = NULL, 
            title = "Click to export plots and tables to PDF file.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          downloadButton("download_output", "Export to PDF")
        ),
        mainPanel(
          withSpinner( # Animated load/wait icon for render output.
            plotOutput("ggplot", width = "auto", height = "auto")), # Render ggplot + tableGrob().
        )
      )
    ),
    #####################################################################################################
    tabPanel(
      title = "BH-TEDS Admissions",
      fluidRow(
        column(width = 2, 
               radioButtons("radio", label = h3("Radio buttons"),
                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                            selected = 1),
        )
      )
    ),
    #####################################################################################################
    tabPanel(
      title = "BH-TEDS Discharge",
      fluidRow(
        column(width = 2, 
               radioButtons("radio", label = h3("Radio buttons"),
                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                            selected = 1),
        )
      )
    )
    #####################################################################################################
  )
)
#####################################################################################################
# Define Server.
#####################################################################################################

server <- function(input, output) {
  
  #####################################################################################################
  # Admissions Tab
  #####################################################################################################
  
  # Radio button input values. 
  function(input, output) {
    
    # You can access the values of the widget (as a vector)
    # with input$radio, e.g.
    output$value <- renderPrint({ input$radio })
    
  }
  
  #####################################################################################################
  # Discharge Tab
  #####################################################################################################
  
  # Radio button input values. 
  function(input, output) {
    
    # You can access the values of the widget (as a vector)
    # with input$radio, e.g.
    output$value <- renderPrint({ input$radio })
    
  }
  
  #####################################################################################################
  # Demographics Visualizations Tab
  #####################################################################################################
  
  options(shiny.maxRequestSize=30*1024^2) # change max file upload size limit.
  
  # Update data source if user uploads a file.
  data_source <- reactive({
    # Reference: https://shiny.rstudio.com/gallery/file-upload.html
    tryCatch(
      {
        df <- read.spss(
          file = input$select_file$datapath,  
          use.value.labels = TRUE, 
          to.data.frame = TRUE,
          trim.factor.names = TRUE, 
          trim_values = TRUE, 
          use.missings = TRUE, 
          add.undeclared.levels = "append")
        return(df) # Use uploaded dataset.
      },
      error = function(e) {
        # stop(safeError(e)) # Show user error message
        warning("No new data source loaded: defaulting to FY2022 Q1-Q3")
        return(bhteds_data) # Use default dataset. 
      }
    )
  })
  
  # Update data source variable labels if user uploads a file. 
  # Note: Create labels with/without custom variables due to current source function limitations. 
  # TODO: Modify source visualization function to redirect selection of original vars to modified vars. 
  data_source_labels <- reactive({
    # Reference: https://stackoverflow.com/questions/29378493/update-on-file-upload-shiny-r
    if(is.data.frame(data_source())) { # Extract only if data exists.
      my_variable_labels <- data.frame(variable_labels = attributes(data_source())$variable.labels)
      # Get variables associated with labels. 
      my_variable_labels <- my_variable_labels %>% 
        mutate(variable = row.names(my_variable_labels)) 
      # Placeholder if variable labels are missing. 
      my_variable_labels <- my_variable_labels %>% mutate(
        variable_labels = ifelse(variable_labels == "", variable, variable_labels)
      ) 
      #  Create labels for variables without one (or modify them). 
      my_variable_labels_mod <- my_variable_labels %>% rbind(
        c(variable = "anyopioid", variable_label = "Medication Assisted Opioid Treatment (any opioid)"), 
        c(variable = "daystx2", variable_label = "Days to Treatment Group"), 
        c(variable = "ageyr10", variable_label = "Age Group"), 
        c(variable = "adultchild", variable_label = "Adult/Child"), 
        c(variable = "educatGrp", variable_label = "Education"), 
        c(variable = "income2", variable_label = "Income Group")
      )
      # Convert variable labels to list format.
      variable_labels_list <- list()
      for(i in 1:nrow(my_variable_labels_mod)) {
        variable_labels_list[my_variable_labels_mod[i, 2]] <- my_variable_labels_mod[i, 1]
      }
      variable_labels_list_reversed <- list()
      for(i in 1:nrow(my_variable_labels)) {
        variable_labels_list_reversed[my_variable_labels[i, 1]] <- my_variable_labels[i, 2]
      }
    }
    return(list(labels = variable_labels_list, labels_reversed = variable_labels_list_reversed, 
                labels_df = my_variable_labels, labels_df_mod = my_variable_labels_mod))
  })
  
  output$select_variables <- renderUI({
    selectInput(inputId = "input_select_variables",
                label = "Data Variables:",
                width = "400px",
                choices = c(data_source_labels()[[3]]$variable_labels, "all variables"), # BH-TEDS variables in dataset.
                selected = my_labs,
                multiple = TRUE)
  })
  
  output$select_county <- renderUI({
    selectInput(inputId = "input_select_county",
                label = "County Filter:",
                width = "400px",
                choices = unique(data_source()$county), # BH-TEDS counties in dataset.
                selected = "Allegan",
                multiple = FALSE)
  })
  
  output$select_ref_county <- renderUI({
    selectInput(inputId = "input_select_ref_county",
                label = "Reference County(ies) Filter(s):",
                width = "400px",
                choices = c(as.character(unique(data_source()$county)), "none"), # BH-TEDS counties in dataset.
                selected = "none",
                multiple = TRUE)
  })
  
  output$select_subset_variable <- renderUI({
    selectInput(inputId = "input_select_subset_variable",
                label = "Subset BH-TEDS data by:",
                width = "400px",
                choices = c(data_source_labels()[[3]]$variable_labels, "none"), # BH-TEDS variables in dataset.
                selected = "none",
                multiple = FALSE)
  })
  
  # Generate plots + tables (upon user clicking action button).
  create_plot_table <- eventReactive(eventExpr = input$action, {
    # Reference: https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive
    my_filters <- list()
    # Filter only if user selects something.
    if(!(input$input_select_subset_variable == "none") && !is.null(input$input_select_subset_variable)) {
      my_filters[[data_source_labels()[[2]][[input$input_select_subset_variable]]]] <- c(input$select_subset_operation, input$select_subset_variable_level)
    }
    # Default data source (in case data_source() fails).
    my_data <- bhteds_data
    my_labels <- var_lab_list
    # User defined data source.
    if(is.data.frame(data_source())) {
      my_data <- data_source()
      my_labels <- data_source_labels()[[1]]
    } 
    # Define reference county(ies). 
    my_reference_county <- input$input_select_ref_county
    if("none" %in% input$input_select_ref_county) { my_reference_county = NULL }
    # Retrieve variable names from selected variable labels.
    my_selected_vars <- c()
    for(i in input$input_select_variables) {
      my_selected_vars <- append(data_source_labels()[[2]][[i]], my_selected_vars)
    }
    # If user selects "all variables" in the dataset.
    if("all variables" %in% input$input_select_variables) {
      my_selected_vars <- data_source_labels()[[3]]$variable[!data_source_labels()[[3]]$variable %in% c("county")]
    }
    # Reference: https://shiny.rstudio.com/articles/isolation.html
    isolate(visualize_demographics( # Call bh_teds.R source file function.
      data = my_data,
      labels_data = my_labels,
      county_filter = input$input_select_county,
      ref_county_filter = my_reference_county,
      subset_filter = my_filters, 
      fiscal_year = "FY2022 Q1-Q3",
      stats_vars = my_selected_vars,
      ref_stat_type = input$select_ref_county_stat))
  })
  
  # Dynamically determine render output height.
  dynamic_render_height <- function() {
    output_plottable <- create_plot_table()
    return(480 * (length(output_plottable) / 2))   
  }
  
  # Format plots + tables for export (download) and rendering (in app).
  format_grobs <- function() {
    output_plottable <- create_plot_table() # Get plots + tables.
    my_list <- vector("list", length(output_plottable)) # Define list length.
    # Post-process plots.
    for(i in seq(from = 1, to = length(output_plottable), by = 2)) {
      my_list[[i]] <- output_plottable[[i]] + scale_x_discrete(labels = function(x) str_wrap(x, width = 12))
    }
    # Post-process tables.
    for(j in seq(from = 2, to = length(output_plottable), by = 2)) {
      my_list[[j]] <- gridExtra::tableGrob(output_plottable[[j]], theme = my_grid.table_theme(table = output_plottable[[j]]))
    }
    # Define export layout of plots + tables. 
    my_grobs_pdf <- gridExtra::marrangeGrob(
      grobs = my_list, 
      nrow = 2, ncol = 4) # Adjust rows/columns of objects per page of PDF output.
    return(list(my_pdf_format = my_grobs_pdf, my_render_format = my_list))
  }
  
  # Render ggplot and tableGrob().
  output$ggplot <- renderPlot(
    # Reference: https://github.com/rstudio/shiny/issues/2373
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      gridExtra::grid.arrange(grobs = format_grobs()[[2]], ncol = 2) # Display plots + tables.
    }, width = 1920, height = dynamic_render_height) # Dynamic render output height.
  
  # Download rendered plots + tables (Note: Won't handle downloads properly in R-Studio (use external browser).
  output$download_output <- downloadHandler(
    # Reference: https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app 
    filename = paste0(input$file_name, ".pdf"),
    content = function(filename) {
      ggsave(filename = filename, 
             plot = format_grobs()[[1]], 
             device = "pdf", 
             units = "in", width = 50, height = 24, dpi = 300, 
             limitsize = FALSE)
    })
  
  # Dynamically acquire levels within selected variable for subsetting data.
  observe({
    my_choices <- c("none")
    if(!(input$input_select_subset_variable == "none") && !is.null(input$input_select_subset_variable)) {
      my_choices <- unique(data_source()[data_source_labels()[[2]][[input$input_select_subset_variable]]])
    }
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId =  "select_subset_variable_level",
                      choices = my_choices)
  })
}
#####################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)