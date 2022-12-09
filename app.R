#####################################################################################################
# IBH Analytics LLC
# BH-TEDS Project R Shiny App Prototype
# Author: Joseph Jinn
# E-mail: jinnjo@mail.gvsu.edu
#####################################################################################################

# TODO: Expand upon user authenticaiton-permissions in conjunction with SQL server. 
# TODO: Switch to using appendTab() basedon user privileges. 

# TODO: Setup BED-TEDS project directory to connect directly to GitHub repository for direct version control. 
# TODO: Setup .gitignore file so only R-code files are in the repository. 

# TODO: create a "universal" theme to apply to all plots (ie. theme_bw())
# TODO: Add tooltips to other dashboard tabs to guide user in usage. 

# TODO: Fix image size restriction when selecting too many variables (could fail to plot).
# TODO: https://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input/56551558#56551558 

# Note: https://shiny.rstudio.com/articles/profiling.html (performance profiling to identify bottlenecks)

# Reference: https://deanattali.com/blog/advanced-shiny-tips/ 
# Reference: https://shinyapps.dreamrs.fr/shinyWidgets/  
# Reference: https://sscc.wisc.edu/shiny/users/jstruck2/layouts/  

# Reference: https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html  
# Reference: https://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input/56551558#56551558 (IMPLEMENT THIS)

#####################################################################################################
# References for deploying R-Shiny on Linux CentOS7 using Microsoft Azure Virtual Machine
#####################################################################################################

# Note: entire process could be time consuming if VM has limited resources (CPU/RAM) allocated (e.g. free Azure account). 

# To use specific version of R in Linux CentOS7 CLI: sudo -i /opt/R/4.2.2/bin/R

# FIXME: Tidyverse package fails to install due to lack of availability of certain dependencies (can install individual components).

# Note: need to install ODBC drivers on Linux distro before installing the relevant R-package. 

# Install BitVise client for access via SSH: 
# https://www.bitvise.com/ssh-client-download

# If can't install ODBC drivers, refer to below: 
# https://support.posit.co/hc/en-us/articles/360052331933-ODBC-v1-2-3-on-CentOS-RHEL-6-7-#:~:text=fatal%20error%3A%20codecvt%3A%20No%20such,provide%20a%20sufficient%20GCC%20version.

# In case of issue with installing R-package "sodium": 
# https://stackoverflow.com/questions/61454457/problem-installing-sodium-package-in-r-on-an-ubuntu-system 

# General process for deploying R-Shiny on R-Server on CentOS7: 
# https://canovasjm.netlify.app/2020/01/08/deploy-you-own-shiny-server-on-azure/

# General process for installing R, R-Studio, and packages on Linux: 
# https://linoxide.com/install-r-rstudio-centos-7/

# Official Posit documentation on installing R on Linux distros: 
# https://docs.posit.co/resources/install-r/#optional-install-recommended-packages

# Official Posit documentation on installing R-server on Linux distros: 
# https://posit.co/download/rstudio-server/

# Official Posit documentation for installing R-Shiny server on Linux distros: 
# https://posit.co/download/shiny-server/

# StackOverflow post on updating to different versions of R: 
# https://stackoverflow.com/questions/61646933/how-to-update-to-specific-r-version-on-linux-red-hat-centos-keeping-the-previ

# StackOverflow post on using personal libraries in R on Linux: 
# https://stackoverflow.com/questions/62206931/how-to-solve-personal-library-problem-in-r

#####################################################################################################
# Required packages. 
#####################################################################################################

library(shiny) # Shiny apps.
library(shinyWidgets) # Additional Shiny input widgets. 
library(shinycssloaders) # Waiting/loading animated icons.
library(shinyBS) # Add tooltips to GUI elements.
library(shinyauthr) # User authentication features. 
library(shinyjs) # Enable additional javascript functionality in R. 

library(dplyr) # Data manipulation.
library(tidyr)
library(ggplot2) # Plotting.

library(foreign) # Read SPSS files.
library(forcats) # For fct_rev(). 
library(stringr) # String manipulation functions.

library(DBI) # Server access backend. 
library(odbc) # Server access driver. 
library(dbplyr) # SQL queries using dplyr syntax. 

source("bh_teds.R") # Data visualization function.

c("shiny", "shinyWidgets", "shinycssloaders", "shinyBS", "shinyauthr", "shinyjs", "dplyr", "dbplyr", "tidyr", 
  "ggplot2", "foreign", "forcats", "stringr", "odbc")

#####################################################################################################
# Define global variables.
#####################################################################################################

bhteds_admissions_labels <- readRDS("data/bhteds_admissions_variable_labels_full.RDS") # Data labels
bhteds_discharge_labels <- readRDS("data/bhteds_discharge_variable_labels_full.RDS") # Data labels

enable_database_access <- 0 # Use database or static data source? 
debug_database_access <- 0 
debug_user_authentication <- 0 # Enable/disable user authentication debug messages. 
debug_filter <- 0; # Enable/disable filtering debug messages.

#####################################################################################################
# Microsoft SQL Server Database Acess. 
#####################################################################################################

if(enable_database_access) {
  tryCatch(
    {
      # Connect to SQL Server database.
      con <- dbConnect(odbc(),
                       Driver = "SQL Server",
                       Server = "ANOVA",
                       Database = "BH-TEDS",
                       UID = "Sorce",
                       # PWD = rstudioapi::askForPassword("Database password"),
                       # Port = 1433,
                       Trusted_Connection = "yes")
      # Read admissions and discharge tables from the database.
      admissions_table <- tbl(src = con, "admissions")
      discharges_table <- tbl(src = con, "discharges")
      # Retrieve BH-TEDS tables.
      admissions_table_df <- admissions_table %>% collect() %>% as.data.frame() %>% select(everything())
      discharges_table_df <- discharges_table %>% collect() %>% as.data.frame() %>% select(everything())
      
      bhteds_admissions_data_full <- admissions_table_df
      bhteds_discharge_data_full <- discharges_table_df
    },
    error = function(e) {
      warning("Failed to retrieve data from SQL Server: defaulting to local data source")
      bhteds_admissions_data_full <- readRDS("data/bhteds_admissions_data_full.RDS")
      bhteds_discharge_data_full <- readRDS("data/bhteds_discharge_data_full.RDS")
    }
  )
} else {
  bhteds_admissions_data_full <- readRDS("data/bhteds_admissions_data_full.RDS")
  bhteds_discharge_data_full <- readRDS("data/bhteds_discharge_data_full.RDS")
}

#####################################################################################################
# User Authentication. 
# TODO: This should be a table in the database. 
#####################################################################################################

user_list <- data.frame(
  user = c("admin", "allegan"), 
  password = sapply(c("admin_3412", "allegan_3412"), sodium::password_store), 
  permissions = c("admin", "allegan"), 
  name = c("admin", "allegan")
)

#####################################################################################################
# Define GUI.
#####################################################################################################
ui <- fluidPage(
  id = "my_page", 
  # Application title
  titlePanel("BH-TEDS Project Prototype App"),
  
  tabsetPanel(
    id = "my_tabset_panel", 
    #####################################################################################################
    tabPanel(
      id = "BD-TEDS Login-Logout", 
      title = "BH-TEDS Login-Logout", 
      # User authentication components. 
      loginUI(id = "login"), # Log-in UI. 
      logoutUI(id = "logout")
    ),
    tabPanel(
      id = "BH-TEDS Demographic Visualizations", 
      title = "BH-TEDS Demographic Visualizations",
      sidebarLayout(
        sidebarPanel(
          popify(
            content = NULL,
            el = (fileInput(
              inputId = "select_file", 
              label = "Upload SPSS Data (.sav file):", 
              multiple = FALSE, 
              accept = c(".sav"), 
              width = "400px")), 
            title = "Select a SPSS data file (.sav) to upload to the app.", 
            placement = "top", 
            trigger = "hover"),
          
          bsPopover(
            id = "select_admissions_or_discharge", 
            content = NULL,
            title = "Select to load either BH-TEDS Admissions or Discharge Dataset", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_admissions_or_discharge"),
          
          bsPopover(
            id = "select_fiscal_year_demographics", 
            content = NULL,
            title = "Select Fiscal Year(s) of interest to filter the BH-TEDS data by.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          bsPopover(
            id = "select_fiscal_quarter_demographics", 
            content = NULL, 
            title = "Select Fiscal Quarter(s) of interest to filter the BH-TEDS data by.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_fiscal_year_demographics"),
          uiOutput(outputId = "select_fiscal_quarter_demographics"),
          
          bsPopover(
            id = "select_variables", 
            content = NULL,
            title = "Select variables of interest from the BH-TEDS data to generate plots and tables for.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          bsPopover(
            id = "select_county", 
            content = NULL, 
            title = "Select county of interest to filter the BH-TEDS data by.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_variables"),
          uiOutput(outputId = "select_county"),
          
          bsPopover(
            id = "select_ref_county", 
            content = NULL, 
            title = "Select one or more reference county(ies) to compare against the selected county above.", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          bsPopover(
            id = "select_ref_county_stat", 
            content = NULL, 
            title = "Select the type of aggregate statistic to compute for the reference county(ies).", 
            placement = "top", 
            trigger = "hover", options = list(container = "body")),
          
          uiOutput(outputId = "select_ref_county"),
          
          selectInput(
            inputId = "select_ref_county_stat", 
            label = "Reference County(ies) Stat Type:", 
            width = "400px", 
            choices = c("mean", "median", "sum"),
            selected = "median", 
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
          
          selectInput(
            # Reference: https://stackoverflow.com/questions/34929206/selectinput-that-is-dependent-on-another-selectinput
            inputId = "select_subset_variable_level", 
            label = "Subset by this level within the variable:", 
            width = "400px", 
            choices = c("Dynamic Update: User selection"), # Level(s) within user-selected variable.
            selected = "Male", 
            multiple = FALSE),
          
          popify(
            # FIXME: Can't seem to generate tooltip for actionButton() 
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
          
          downloadButton(outputId = "download_output", label = "Export to PDF")
        ),
        mainPanel(
          withSpinner( # Animated load/wait icon for render output.
            plotOutput(outputId = "ggplot", width = "auto", height = "auto")) # Render ggplot + tableGrob().
        )
      )
    ),
    #####################################################################################################
    tabPanel(
      id = "BH-TEDS Admissions", 
      title = "BH-TEDS Admissions",
      wellPanel( # First row.
        fluidRow(
          column(
            width = 1, 
            wellPanel( 
              fluidRow(
                uiOutput(outputId = "select_fiscal_year_admissions")
              )
            )
          ),
          column(
            width = 1, 
            wellPanel(
              fluidRow(
                uiOutput(outputId = "select_fiscal_quarter_admissions")
              )
            )
          ),
          column(
            width = 3, 
            wellPanel(
              "Priority Population",
              id = "Priority Population", 
              fluidRow(
                fluidRow(
                  column(
                    width = 12, 
                    uiOutput(outputId = "mdoc_flag_admissions")
                  )
                ), 
                fluidRow(
                  column(
                    width = 6, 
                    uiOutput(outputId = "iv_flag_admissions")
                  ), 
                  column(
                    width = 6, 
                    uiOutput(outputId = "pregnant_admissions")
                  ), 
                )
              )
            )
          ),
          column(
            width = 5, 
            wellPanel(
              "Primary Substance of Abuse", 
              id = "Primary Substance of Abuse", 
              fluidRow(
                column(
                  width = 4, 
                  uiOutput(outputId = "primary_substance_admissions")
                ), 
                column(
                  width = 4, 
                  uiOutput(outputId = "last_30_days_admissions")
                ), 
                column(
                  width = 4, 
                  uiOutput(outputId = "dx_severity_admissions")
                ), 
              )
            )
          ),
          column(
            width = 2, 
            wellPanel(
              "Demographics",
              id = "Demographics", 
              fluidRow(
                column(
                  width = 6,
                  uiOutput(outputId = "adult_child_admissions")
                ), 
                column(
                  width = 6, 
                  uiOutput(outputId = "gender_admissions")
                )
              ),
              fluidRow(
                uiOutput(outputId = "admissions_county")
              ),
              fluidRow(
                uiOutput(outputId = "admissions_reference_county")
              )
            )
          )
        )
      ), 
      wellPanel(
        "Drug Flags",
        id = "Drug Flags", # 2nd row. 
        fluidRow(
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "alcohol_admissions")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "methamphetamine_admissions")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "marijuana_admissions")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "heroin_admissions")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "other_opiates_admissions")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "any_opioids_admissions")
            )
          )
        )
      ),
      wellPanel( # Third row. 
        fluidRow(
          column(
            width = 4, 
            wellPanel(
              uiOutput(outputId = "top_providers_admissions")
            )
          ),
          column(
            width = 4, 
            wellPanel(
              uiOutput(outputId = "service_setting_admissions")
            )
          ),
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "integrated_treatment_admissions")
            )
          ),
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "mat_admissions")
            )
          )
        )
      ), 
      wellPanel( # Fourth row. 
        fluidRow(
          column(
            width = 7, 
            plotOutput(outputId = "admissions_plot", width = "auto", height = "auto")
            
          ), 
          column(
            width = 5, 
            plotOutput(outputId = "days_to_treatment_plot", width = "auto", height = "auto")
          )
        )
      )
    ),
    #####################################################################################################
    tabPanel(
      id = "BH-TEDS Discharge", 
      title = "BH-TEDS Discharge", 
      wellPanel( # First row.
        fluidRow(
          column(
            width = 1, 
            wellPanel( 
              fluidRow(
                uiOutput(outputId = "select_fiscal_year_discharge")
              )
            )
          ),
          column(
            width = 1, 
            wellPanel(
              fluidRow(
                uiOutput(outputId = "select_fiscal_quarter_discharge")
              )
            )
          ),
          column(
            width = 3, 
            wellPanel(
              "Priority Population",
              id = "Priority Population", 
              fluidRow(
                fluidRow(
                  column(
                    width = 12, 
                    uiOutput(outputId = "mdoc_flag_discharge")
                  )
                ), 
                fluidRow(
                  column(
                    width = 6, 
                    uiOutput(outputId = "iv_flag_discharge")
                  ), 
                  column(
                    width = 6, 
                    uiOutput(outputId = "pregnant_discharge")
                  ), 
                )
              )
            )
          ),
          column(
            width = 5, 
            wellPanel(
              "Primary Substance of Abuse", 
              id = "Primary Substance of Abuse",
              fluidRow(
                column(
                  width = 4, 
                  uiOutput(outputId = "primary_substance_discharge")
                ), 
                column(
                  width = 4, 
                  uiOutput(outputId = "last_30_days_discharge")
                ), 
                column(
                  width = 4, 
                  uiOutput(outputId = "dx_severity_discharge")
                ), 
              )
            )
          ),
          column(
            width = 2, 
            wellPanel(
              "Demographics", 
              id = "Demographics", 
              fluidRow(
                column(
                  width = 6,
                  uiOutput(outputId = "adult_child_discharge")
                ), 
                column(
                  width = 6, 
                  uiOutput(outputId = "gender_discharge")
                )
              ),
              fluidRow(
                uiOutput(outputId = "discharge_county")
              ),
              fluidRow(
                uiOutput(outputId = "discharge_reference_county")
              )
            )
          )
        )
      ), 
      wellPanel(
        "Drug Flags", 
        id = "Drug Flags", # 2nd row. 
        fluidRow(
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "alcohol_discharge")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "methamphetamine_discharge")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "marijuana_discharge")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "heroin_discharge")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "other_opiates_discharge")
            )
          ), 
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "any_opioids_discharge")
            )
          )
        )
      ),
      wellPanel( # Third row. 
        fluidRow(
          column(
            width = 4, 
            wellPanel(
              uiOutput(outputId = "top_providers_discharge")
            )
          ),
          column(
            width = 4, 
            wellPanel(
              uiOutput(outputId = "service_setting_discharge")
            )
          ),
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "integrated_treatment_discharge")
            )
          ),
          column(
            width = 2, 
            wellPanel(
              uiOutput(outputId = "mat_discharge")
            )
          )
        )
      ), 
      wellPanel( # Fourth row. 
        fluidRow(
          column(
            width = 7, 
            plotOutput(outputId = "discharges_plot", width = "auto", height = "auto")
            
          ), 
          column(
            width = 5, 
            plotOutput(outputId = "discharge_days_to_treatment_plot", width = "auto", height = "auto")
          )
        )
      ),
      wellPanel( # Fifth row. 
        fluidRow(
          column(
            width = 6, 
            plotOutput(outputId = "discharge_reason_plot", width = "auto", height = "auto")
            
          ), 
          column(
            width = 6, 
            plotOutput(outputId = "discharge_change_in_use_plot", width = "auto", height = "auto")
          )
        )
      )
      #####################################################################################################
    )
  )
)
#####################################################################################################
# Define Server.
#####################################################################################################

server <- function(input, output, session) {
  
  #####################################################################################################
  # User login and logout. 
  #####################################################################################################
  
  credentials <- loginServer(
    # Reference: https://datastorm-open.github.io/shinymanager/  
    # Reference: https://towardsdatascience.com/r-shiny-authentication-incl-demo-app-a599b86c54f7 
    id = "login",
    data = user_list,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  logout_init <- logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Show UI based on user authentication and permissions.  
  observe({
    # Reference: https://shiny.rstudio.com/reference/shiny/1.4.0/showtab  
    # Reference: https://github.com/PaulC91/shinyauthr
    hideTab(inputId = "my_tabset_panel", target = "BH-TEDS Demographic Visualizations")
    hideTab(inputId = "my_tabset_panel", target = "BH-TEDS Admissions")
    hideTab(inputId = "my_tabset_panel", target = "BH-TEDS Discharge")
    
    if (credentials()$user_auth == TRUE){
      showTab(inputId = "my_tabset_panel", target = "BH-TEDS Admissions")
      showTab(inputId = "my_tabset_panel", target = "BH-TEDS Discharge")
      if (credentials()$info$permission == "allegan") {
        # Insert condition here. 
      } else if(credentials()$info$permission == "admin") {
        showTab(inputId = "my_tabset_panel", target = "BH-TEDS Demographic Visualizations")
      }
    }
    if(debug_user_authentication == 1) {
      print(paste0("User Authorized?: ", credentials()$user_auth))
      print(paste0("\nUser Info:\n", glimpse((credentials()$info))))
    }
  })
  # Update UI elements once user switches to relevant tab panel. 
  observeEvent(eventExpr = input$my_tabset_panel, {
    # Reference: https://stackoverflow.com/questions/58402873/shiny-is-there-a-way-to-trigger-an-observeevent-by-switching-between-tabspane 
    if (credentials()$user_auth == TRUE){
      if (credentials()$info$permission == "allegan") {
        my_choices <- c("Allegan")
        updateSelectInput(session = session,
                          inputId =  "input_admissions_county",
                          choices = my_choices)
        updateSelectInput(session = session,
                          inputId =  "input_discharge_county",
                          choices = my_choices)
        
      } else if(credentials()$info$permission == "admin") {
        updateSelectInput(session = session,
                          inputId =  "input_admissions_county",
                          choices = sort(c(as.character(unique(filter_preprocess_admissions_data()$county)))))
        updateSelectInput(session = session,
                          inputId =  "input_discharge_county",
                          choices = sort(c(as.character(unique(filter_preprocess_admissions_data()$county)))))
      }
    }
  })
  
  #####################################################################################################
  # Admissions Tab
  #####################################################################################################
  
  # Preprocess data before user filtering. 
  filter_preprocess_admissions_data <- reactive({
    # Reference: https://stackoverflow.com/questions/1169248/test-if-a-vector-contains-a-given-element
    req(credentials()$user_auth) 
    if(credentials()$info$permission == "allegan") {
      temp <- bhteds_admissions_data_full
    } else if(credentials()$info$permission == "admin") {
      temp <- bhteds_admissions_data_full
    }
    temp <- temp %>% mutate(adultchild = ifelse(ageyears >= 18, "Adult", "Child"))
    return(temp)
  })
  
  # Determine top 90% of providers for selected county.   
  filter_admissions_top_providers <- reactive({
    req(credentials()$user_auth) 
    my_county_filter <- input$input_admissions_county
    temp <- filter_preprocess_admissions_data()
    
    if(!is.null(my_county_filter)) {
      if(my_county_filter != "none"){
        temp <- temp %>% filter(county %in% my_county_filter)
        if(debug_filter == 1) { print(paste0("County Filter:", my_county_filter, ":", nrow(temp)))  }
      }
    }
    program.list <- temp %>% 
      # filter(county == "Allegan") %>%
      mutate(countT= n()) %>%
      group_by(program, .add=TRUE) %>% 
      mutate(n = n(),
             per=round(100*(n/countT),2)) %>% 
      filter(row_number() == 1) %>% 
      arrange(desc(n)) %>% 
      ungroup() %>% 
      mutate(cum_sum = cumsum(per)) %>% 
      filter(cum_sum < 90) %>%
      #  select(program, n, cum_sum)
      select(program)
    
    program.list <- as.list(as.data.frame(t(program.list)))
    
    temp <- filter_preprocess_admissions_data() %>%
      mutate(programRecode = ifelse(!(as.character(program) %in% program.list),
                                    "Other", as.character(program)),
             programRecode = gsub('[0-9]+', '', programRecode))
    return(temp)
  })
  
  observe({
    print(paste0("Total Admissions Rows after User Filtering (excluding county filter): ", nrow(filter_admissions_data())))
  })
  
  # Dynamically filters data based on use selections. 
  filter_admissions_data <- reactive({
    req(credentials()$user_auth) 
    temp <- filter_admissions_top_providers()
    # Fiscal year/quarters.
    my_fiscal_year_filters <- input$input_select_fiscal_year_admissions
    my_fiscal_quarter_filters <- input$input_select_fiscal_quarter_admissions
    my_fiscal_year_quarter_filters <- input$input_select_fiscal_year_quarter_admissions
    # Priority populations. 
    my_iv_flag_filters <- input$input_iv_flag_admissions
    my_pregnant_filters <- input$input_pregnant_admissions
    my_mdoc_flag_filters <- input$input_mdoc_flag_admissions
    # Primary substance of abuse. 
    my_primary_substance_filters <- input$input_primary_substance_admissions
    my_last_30_days_filters <- input$input_last_30_days_admissions
    my_dx_severity_filters <- input$input_dx_severity_admissions
    # Demographics. 
    my_adult_child_filters <- input$input_adult_child_admissions
    my_gender_filters <- input$input_gender_admissions
    # Drug flags. 
    my_alcohol_filters <- input$input_alcohol_admissions
    my_methamphetamine_filters <- input$input_methamphetamine_admissions
    my_marijuana_filters <- input$input_marijuana_admissions
    my_heroin_filters <- input$input_heroin_admissions
    my_other_opiates_filters <- input$input_other_opiates_admissions
    my_any_opioids_filters <- input$input_any_opioids_admissions
    # Third row. 
    my_service_setting_filters <- input$input_service_setting_admissions
    my_integrated_treatment_filters <- input$input_integrated_treatment_admissions
    my_mat_filters <- input$input_mat_admissions
    
    # Fiscal years and quarters.
    if(!is.null(my_fiscal_year_filters)) {
      temp <- temp %>% filter(FY %in% my_fiscal_year_filters)
      if(debug_filter == 1) { print(paste("Fiscal Year:", my_fiscal_year_filters, ":", nrow(temp), collapse = " ")) }
    }
    if(!is.null(my_fiscal_quarter_filters)) {
      temp <- temp %>% filter(Qtr %in% my_fiscal_quarter_filters)
      if(debug_filter == 1) { print(paste("Fiscal Quarter:", my_fiscal_quarter_filters, ":", nrow(temp), collapse = " ")) }
    }
    # if(!is.null(my_fiscal_year_quarter_filters)) {
    #   temp <- temp %>% filter(FYQtr %in% my_fiscal_year_quarter_filters)
    #   if(debug_filter == 1) { print(paste("Fiscal Year-Quarter:", my_fiscal_year_quarter_filters, ":", nrow(temp), sep = " ")) }
    # }
    # Priority Population. 
    if(!is.null(my_iv_flag_filters)) {
      # TODO: double-check this is correct. 
      if(any(my_iv_flag_filters == "IV drug use")) {
        temp <- temp %>% filter((psarte %in% "Injection" | ssarte %in% "Injection" | tsarte %in% "Injection") & (!psause %in% c("No Use", "Not Applicable")))
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      } else if(any(my_iv_flag_filters == "No IV drug use")) {
        temp <- temp %>% filter(!psarte %in% "Injection" & !ssarte %in% "Injection" & !tsarte %in% "Injection")
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_pregnant_filters)) {
      if(my_pregnant_filters == "Yes") {
        temp <- temp %>% filter(pregnant %in% "Yes")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else if(my_pregnant_filters == "No") {
        temp <- temp %>% filter(pregnant %in% "No")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else if(my_pregnant_filters == "NA") {
        temp <- temp %>% filter(pregnant %in% "NA")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_mdoc_flag_filters)) {
      if(my_mdoc_flag_filters == "MDOC Involvement") {
        temp <- temp %>% filter(corrstat %in% c("In Prison", "In Jail", "Parole"))
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      } else if(my_mdoc_flag_filters == "No MDOC Involvement") {
        temp <- temp %>% filter(!corrstat %in% c("In Prison", "In Jail", "Parole"))
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      }
    }
    # Primary Substance of Abuse. 
    if(!is.null(my_primary_substance_filters)) {
      temp <- temp %>% filter(psacode %in% my_primary_substance_filters)
      if(debug_filter == 1) { print(paste("Primary Substance:", my_primary_substance_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_last_30_days_filters)) {
      temp <- temp %>% filter(psause %in% my_last_30_days_filters)
      if(debug_filter == 1) { print(paste("Last 30 Days Use:", my_last_30_days_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_dx_severity_filters)) {
      # Note: Filter does not account for NA values in variable 
      temp <- temp %>% filter(SUD_DxGroup1 %in% my_dx_severity_filters)
      if(debug_filter == 1) { print(paste0("DX Severity:", my_dx_severity_filters, ":", nrow(temp))) }
    }
    # Demographics. 
    if(!is.null(my_adult_child_filters)) {
      if(any(my_adult_child_filters == "Adult")) {
        temp <- temp %>% filter(adultchild %in% c("Adult"))
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      } else if(any(my_adult_child_filters == "Child")) {
        temp <- temp %>% filter(adultchild %in% c("Child"))
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_gender_filters)) {
      if(any(my_gender_filters == "Male")) {
        temp <- temp %>% filter(gender %in% c("Male"))
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      } else if(any(my_gender_filters == "Female")) {
        temp <- temp %>% filter(gender %in% c("Female"))
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      }
    }
    # Drug Flags. 
    if(!is.null(my_alcohol_filters)) {
      if(any(my_alcohol_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Alcohol") | ssacode %in% c("Alcohol") | tsacode %in% c("Alcohol"))
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      } else if(any(my_alcohol_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Alcohol") & !ssacode %in% c("Alcohol") & !tsacode %in% c("Alcohol"))
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_methamphetamine_filters)) {
      if(any(my_methamphetamine_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Methamphetamine") | ssacode %in% c("Methamphetamine") | tsacode %in% c("Methamphetamine"))
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      } else if(any(my_methamphetamine_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Methamphetamine") & !ssacode %in% c("Methamphetamine") & !tsacode %in% c("Methamphetamine"))
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_marijuana_filters)) {
      if(any(my_marijuana_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Marijuana") | ssacode %in% c("Marijuana") | tsacode %in% c("Marijuana"))
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      } else if(any(my_marijuana_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Marijuana") & !ssacode %in% c("Marijuana") & !tsacode %in% c("Marijuana"))
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_heroin_filters)) {
      if(any(my_heroin_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Heroin") | ssacode %in% c("Heroin") | tsacode %in% c("Heroin"))
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      } else if(any(my_heroin_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Heroin") & !ssacode %in% c("Heroin") & !tsacode %in% c("Heroin"))
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_other_opiates_filters)) {
      if(any(my_other_opiates_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Other Opiates") | ssacode %in% c("Other Opiates") | tsacode %in% c("Other Opiates"))
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      } else if(any(my_other_opiates_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Other Opiates") & !ssacode %in% c("Other Opiates") & !tsacode %in% c("Other Opiates"))
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_any_opioids_filters)) {
      if(any(my_any_opioids_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Heroin", "Other Opiates") | ssacode %in% c("Heroin", "Other Opiates") | tsacode %in% c("Heroin", "Other Opiates"))
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      } else if(any(my_any_opioids_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Heroin", "Other Opiates") & !ssacode %in% c("Heroin", "Other Opiates") & !tsacode %in% c("Heroin", "Other Opiates"))
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      }
    }
    # Third row. 
    my_top_providers_filters <- input$input_top_providers_admissions
    if(!is.null(my_top_providers_filters)) {
      temp <- temp %>% filter(programRecode %in% my_top_providers_filters)
      if(debug_filter == 1) { print(paste("Top Providers:", my_top_providers_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_service_setting_filters)) {
      temp <- temp %>% filter(srvset %in% my_service_setting_filters)
      if(debug_filter == 1) { print(paste("Service Setting:", my_service_setting_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_integrated_treatment_filters)) {
      temp <- temp %>% filter(inttx %in% my_integrated_treatment_filters)
      if(debug_filter == 1) { print(paste("Integrated Treatment:", my_integrated_treatment_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_mat_filters)) {
      if(any(my_mat_filters == "Yes")) {
        temp <- temp %>% filter(opioid %in% c("Yes"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else if(any(my_mat_filters == "No")) {
        temp <- temp %>% filter(!opioid %in% c("No"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else if(any(my_mat_filters == "NA")) {
        temp <- temp %>% filter(!opioid %in% c("NA"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      }
    }
    return(temp)
  })
  
  # Fiscal year/quarter. 
  output$select_fiscal_year_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_select_fiscal_year_admissions", 
                         label = "Fiscal Year", 
                         choices = c("2020", "2021", "2022", "2023"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$select_fiscal_quarter_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_select_fiscal_quarter_admissions", 
                         label = "Fiscal Quarter", 
                         choices = c("Q1", "Q2", "Q3", "Q4"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  # output$select_fiscal_year_quarter_admissions <- renderUI({
  #   checkboxGroupButtons(inputId = "input_select_fiscal_year_quarter_admissions", 
  #                        label = "Fiscal Year-Quarter", 
  #                        choices = sort(as.character(unique(filter_preprocess_data()$FYQtr))), 
  #                        selected = NULL, 
  #                        size = "sm", 
  #                        direction = "vertical")
  # })
  # Priority populations. 
  output$iv_flag_admissions <- renderUI({
    radioGroupButtons(inputId = "input_iv_flag_admissions", 
                      label = "IV Flag (< 30 days)", 
                      choices = c("IV drug use", "No IV drug use", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "horizontal")
  })
  output$pregnant_admissions <- renderUI({
    radioGroupButtons(inputId = "input_pregnant_admissions", 
                      label = "Pregnant", 
                      choices = c("Yes", "No", "NA", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "horizontal")
  })
  output$mdoc_flag_admissions <- renderUI({
    radioGroupButtons(inputId = "input_mdoc_flag_admissions", 
                      label = "MDOC Flag (Prison, Jail, Parole)", 
                      choices = c("MDOC Involvement", "No MDOC Involvement", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Primary substance of abuse. 
  output$primary_substance_admissions <- renderUI({
    pickerInput(inputId = "input_primary_substance_admissions", 
                label = "Primary Substance", 
                choices = sort(as.character(unique(filter_preprocess_admissions_data()$psacode))), 
                selected = NULL, 
                multiple = TRUE)
  })
  output$last_30_days_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_last_30_days_admissions", 
                         label = "Use in Last 30 Days", 
                         choices = sort(as.character(unique(filter_preprocess_admissions_data()$psause))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$dx_severity_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_dx_severity_admissions",
                         label = "Dx Severity", 
                         choices = c("Mild", "Moderate/Severe", "Withdrawal", "Remission", "Other"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  # Demographics. 
  output$adult_child_admissions <- renderUI({
    radioGroupButtons(inputId = "input_adult_child_admissions", 
                      label = "Adult or Child", 
                      choices = c("Adult", "Child", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$gender_admissions <- renderUI({
    radioGroupButtons(inputId = "input_gender_admissions", 
                      label = "Gender", 
                      choices = c("Female", "Male", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$admissions_county <- renderUI({
    selectInput(inputId = "input_admissions_county", 
                label = "Select County Filter", 
                choices = sort(c(as.character(unique(filter_preprocess_admissions_data()$county)), "none")), 
                selected = "none", 
                multiple = FALSE)
  })
  output$admissions_reference_county <- renderUI({
    pickerInput(inputId = "input_admissions_reference_county", 
                label = "Select Reference County", 
                choices = sort(c(as.character(unique(filter_preprocess_admissions_data()$county)), "none")), 
                selected = "none", 
                multiple = TRUE)
  })
  # Drug flags. 
  output$alcohol_admissions <- renderUI({
    radioGroupButtons(inputId = "input_alcohol_admissions", 
                      label = "Alcohol", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$methamphetamine_admissions <- renderUI({
    radioGroupButtons(inputId = "input_methamphetamine_admissions", 
                      label = "Methamphetamine", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$marijuana_admissions <- renderUI({
    radioGroupButtons(inputId = "input_marijuana_admissions", 
                      label = "Marijuana", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$heroin_admissions <- renderUI({
    radioGroupButtons(inputId = "input_heroin_admissions", 
                      label = "Heroin", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$other_opiates_admissions <- renderUI({
    radioGroupButtons(inputId = "input_other_opiates_admissions", 
                      label = "Other Opiates", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$any_opioids_admissions <- renderUI({
    radioGroupButtons(inputId = "input_any_opioids_admissions", 
                      label = "Any Opioids", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Third row. 
  output$top_providers_admissions <- renderUI({
    multiInput(inputId = "input_top_providers_admissions",
               label = "Top Providers (90% of Admissions)", 
               choices = sort(as.character(unique(filter_admissions_top_providers()$programRecode))), 
               selected = NULL)
  })
  output$service_setting_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_service_setting_admissions", 
                         label = "Service Setting", 
                         choices = sort(as.character(unique(filter_preprocess_admissions_data()$srvset))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "horizontal")
  })
  output$integrated_treatment_admissions <- renderUI({
    checkboxGroupButtons(inputId = "input_integrated_treatment_admissions", 
                         label = "Integrated Treatment", 
                         choices = sort(as.character(unique(filter_preprocess_admissions_data()$inttx))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$mat_admissions <- renderUI({
    radioGroupButtons(inputId = "input_mat_admissions", 
                      label = "MAT", 
                      choices = c("Yes", "No", "NA", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Render plots. 
  output$admissions_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      my_county_filter <- input$input_admissions_county
      temp <- filter_admissions_data()
      
      if(!is.null(my_county_filter)) {
        if(my_county_filter != "none"){
          temp <- temp %>% filter(county %in% my_county_filter)
          if(debug_filter == 1) { print(paste0("County Filter:", my_county_filter, ":", nrow(temp)))  }
        }
      }
      
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_admissions_data()) != 0) {
            my_stats <- temp %>% count(FYQtr) %>%
              mutate_if(is.factor, as.character) %>% arrange(FYQtr)
            
            ggplot(data = my_stats, aes(x = FYQtr, y = n, fill = FYQtr)) + geom_bar(stat = "identity") + 
              geom_text(aes(label = n), vjust = -0.2, colour = "black") +
              labs(title = "ACCMHS SUD Admissions") + 
              xlab("Fiscal Year + Quarter") + 
              ylab("Number of BH-TEDS Admissions") + 
              scale_fill_hue(l = 50, c = 50)
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        },
        error = function(e) {
          warning("ggplot2 failed to render admissions count plot!")
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in inability to render plot!!!'))
          xlab(NULL) 
        }
      )
    }, width = 854, height = 480)
  
  output$days_to_treatment_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      # Set defaults so ggplot2 doesn't crash. 
      my_county_filter <- input$input_admissions_county
      my_reference_county_filter <- input$input_admissions_reference_county
      
      if(is.null(my_county_filter) || my_county_filter == "none") {
        my_county_filter <- "Allegan"
      }
      if(is.null(my_reference_county_filter) || my_reference_county_filter %in% "none") {
        my_reference_county_filter <- c("Barry", "Oakland")
      }
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_admissions_data()) != 0) {
            my_state_stats <- filter_admissions_data() %>% 
              group_by(FYQtr) %>% summarise(mean_state = round(mean(daystx), digits = 2))
            
            my_county_stats <- filter_admissions_data() %>% 
              filter(county %in% my_county_filter) %>% 
              group_by(FYQtr) %>% summarise(mean_county = round(mean(daystx), digits = 2))
            
            my_ref_county_stats <- filter_admissions_data() %>% 
              filter(county %in% my_reference_county_filter) %>% 
              group_by(FYQtr) %>% summarise(mean_ref_county = round(mean(daystx), digits = 2))
            
            my_stats <- merge(my_state_stats, my_county_stats, by = "FYQtr", all = TRUE)
            my_stats <- merge(my_stats, my_ref_county_stats, by = "FYQtr", all = TRUE)
            my_stats <- my_stats %>% replace(is.na(.), 0)
            
            my_stats2 <- my_stats %>% pivot_longer(cols = c("mean_state", "mean_county", "mean_ref_county"), names_to = "mean_type", values_to = "mean")
            
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(FYQtr, mean_type), .fns = factor))
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(mean), .fns = as.double))
            my_stats2 <- my_stats2 %>% mutate(mean_type = factor(mean_type, levels = c("mean_state", "mean_county", "mean_ref_county")))
            
            ggplot(data = my_stats2, aes(x = FYQtr, y = mean, fill = fct_rev(mean_type))) + 
              geom_col(position = "dodge") + 
              geom_text(aes(label = mean), vjust = 0.3, hjust = 1.5, colour = "black", position = position_dodge(width = 1)) + 
              scale_y_discrete(breaks = NULL, expand = c(0, 2)) +
              scale_fill_discrete(breaks = c("mean_state", "mean_county", "mean_ref_county"), labels = c("State", "County", "Ref. County(ies)")) + 
              labs(title = "Days to Treatment", 
                   subtitle = paste0("State: MI", "\nCounty: ", my_county_filter, "\nRef. County(ies): ", paste(my_reference_county_filter, collapse = ","))) + 
              guides(fill = guide_legend(title = "Mean of: ")) + 
              xlab("Fiscal Year + Quarter") + 
              ylab("Average Days") + coord_flip()
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        }, 
        error = function(e) {
          warning("ggplot2 failed to render days to treatment plot!")
          # Reference: https://stackoverflow.com/questions/12518387/can-i-create-an-empty-ggplot2-plot-in-r
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in plot rendering error!!!')) +
            xlab(NULL) #optional, but safer in case another theme is applied later
        }
      )
    }, width = 480, height = 854)
  
  #####################################################################################################
  # Discharge Tab
  #####################################################################################################
  
  # Preprocess data before user filtering. 
  filter_preprocess_discharge_data <- reactive({
    req(credentials()$user_auth) 
    if(credentials()$info$permission == "allegan") {
      temp <- bhteds_discharge_data_full
    } else if(credentials()$info$permission == "admin") {
      temp <- bhteds_discharge_data_full
    }
    temp <- temp %>% mutate(adultchild = ifelse(ageyears >= 18, "Adult", "Child"))
    return(temp)
  })
  
  # Determine top 90% of providers for selected county.   
  filter_discharge_top_providers <- reactive({
    req(credentials()$user_auth) 
    my_county_filter <- input$input_discharge_county
    temp <- filter_preprocess_discharge_data()
    
    if(!is.null(my_county_filter)) {
      if(my_county_filter != "none"){
        temp <- temp %>% filter(county %in% my_county_filter)
        if(debug_filter == 1) { print(paste0("County Filter:", my_county_filter, ":", nrow(temp)))  }
      }
    }
    program.list <- temp %>% 
      # filter(county == "Allegan") %>%
      mutate(countT= n()) %>%
      group_by(program, .add=TRUE) %>% 
      mutate(n = n(),
             per=round(100*(n/countT),2)) %>% 
      filter(row_number() == 1) %>% 
      arrange(desc(n)) %>% 
      ungroup() %>% 
      mutate(cum_sum = cumsum(per)) %>% 
      filter(cum_sum < 90) %>%
      #  select(program, n, cum_sum)
      select(program)
    
    program.list <- as.list(as.data.frame(t(program.list)))
    
    temp <- filter_preprocess_discharge_data() %>%
      mutate(programRecode = ifelse(!(as.character(program) %in% program.list),
                                    "Other", as.character(program)),
             programRecode = gsub('[0-9]+', '', programRecode))
    return(temp)
  })
  
  observe({
    print(paste0("Total Discharge Rows after User Filtering (excluding county filter): ", nrow(filter_discharge_data())))
  })
  
  # Dynamically filters data based on use selections. 
  filter_discharge_data <- reactive({
    req(credentials()$user_auth) 
    temp <- filter_discharge_top_providers()
    # Fiscal year/quarters.
    my_fiscal_year_filters <- input$input_select_fiscal_year_discharge
    my_fiscal_quarter_filters <- input$input_select_fiscal_quarter_discharge
    my_fiscal_year_quarter_filters <- input$input_select_fiscal_year_quarter_discharge
    # Priority populations. 
    my_iv_flag_filters <- input$input_iv_flag_discharge
    my_pregnant_filters <- input$input_pregnant_discharge
    my_mdoc_flag_filters <- input$input_mdoc_flag_discharge
    # Primary substance of abuse. 
    my_primary_substance_filters <- input$input_primary_substance_discharge
    my_last_30_days_filters <- input$input_last_30_days_discharge
    my_dx_severity_filters <- input$input_dx_severity_discharge
    # Demographics. 
    my_adult_child_filters <- input$input_adult_child_discharge
    my_gender_filters <- input$input_gender_discharge
    # Drug flags. 
    my_alcohol_filters <- input$input_alcohol_discharge
    my_methamphetamine_filters <- input$input_methamphetamine_discharge
    my_marijuana_filters <- input$input_marijuana_discharge
    my_heroin_filters <- input$input_heroin_discharge
    my_other_opiates_filters <- input$input_other_opiates_discharge
    my_any_opioids_filters <- input$input_any_opioids_discharge
    # Third row. 
    my_service_setting_filters <- input$input_service_setting_discharge
    my_integrated_treatment_filters <- input$input_integrated_treatment_discharge
    my_mat_filters <- input$input_mat_discharge
    
    # Fiscal years and quarters.
    if(!is.null(my_fiscal_year_filters)) {
      temp <- temp %>% filter(FY %in% my_fiscal_year_filters)
      if(debug_filter == 1) { print(paste("Fiscal Year:", my_fiscal_year_filters, ":", nrow(temp), collapse = " ")) }
    }
    if(!is.null(my_fiscal_quarter_filters)) {
      temp <- temp %>% filter(Qtr %in% my_fiscal_quarter_filters)
      if(debug_filter == 1) { print(paste("Fiscal Quarter:", my_fiscal_quarter_filters, ":", nrow(temp), collapse = " ")) }
    }
    # if(!is.null(my_fiscal_year_quarter_filters)) {
    #   temp <- temp %>% filter(FYQtr %in% my_fiscal_year_quarter_filters)
    #   if(debug_filter == 1) { print(paste("Fiscal Year-Quarter:", my_fiscal_year_quarter_filters, ":", nrow(temp), sep = " ")) }
    # }
    # Priority Population. 
    if(!is.null(my_iv_flag_filters)) {
      # TODO: double-check this is correct. 
      if(any(my_iv_flag_filters == "IV drug use")) {
        temp <- temp %>% filter((psarte %in% "Injection" | ssarte %in% "Injection" | tsarte %in% "Injection") & (!psause %in% c("No Use", "Not Applicable")))
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      } else if(any(my_iv_flag_filters == "No IV drug use")) {
        temp <- temp %>% filter(!psarte %in% "Injection" & !ssarte %in% "Injection" & !tsarte %in% "Injection")
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("IV Flag:", my_iv_flag_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_pregnant_filters)) {
      if(my_pregnant_filters == "Yes") {
        temp <- temp %>% filter(pregnant %in% "Yes")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else if(my_pregnant_filters == "No") {
        temp <- temp %>% filter(pregnant %in% "No")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else if(my_pregnant_filters == "NA") {
        temp <- temp %>% filter(pregnant %in% "NA")
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Pregnant:", my_pregnant_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_mdoc_flag_filters)) {
      if(my_mdoc_flag_filters == "MDOC Involvement") {
        temp <- temp %>% filter(corstat %in% c("In Prison", "In Jail", "Parole"))
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      } else if(my_mdoc_flag_filters == "No MDOC Involvement") {
        temp <- temp %>% filter(!corstat %in% c("In Prison", "In Jail", "Parole"))
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("MDOC Flag:", my_mdoc_flag_filters, ":", nrow(temp))) }
      }
    }
    # Primary Substance of Abuse. 
    if(!is.null(my_primary_substance_filters)) {
      temp <- temp %>% filter(psacode %in% my_primary_substance_filters)
      if(debug_filter == 1) { print(paste("Primary Substance:", my_primary_substance_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_last_30_days_filters)) {
      temp <- temp %>% filter(psause %in% my_last_30_days_filters)
      if(debug_filter == 1) { print(paste("Last 30 Days Use:", my_last_30_days_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_dx_severity_filters)) {
      # Note: Filter does not account for NA values in variable 
      temp <- temp %>% filter(SUD_DxGroup1 %in% my_dx_severity_filters)
      if(debug_filter == 1) { print(paste0("DX Severity:", my_dx_severity_filters, ":", nrow(temp))) }
    }
    # Demographics. 
    if(!is.null(my_adult_child_filters)) {
      if(any(my_adult_child_filters == "Adult")) {
        temp <- temp %>% filter(adultchild %in% c("Adult"))
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      } else if(any(my_adult_child_filters == "Child")) {
        temp <- temp %>% filter(adultchild %in% c("Child"))
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Adult/Child:", my_adult_child_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_gender_filters)) {
      if(any(my_gender_filters == "Male")) {
        temp <- temp %>% filter(gender %in% c("Male"))
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      } else if(any(my_gender_filters == "Female")) {
        temp <- temp %>% filter(gender %in% c("Female"))
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Gender:", my_gender_filters, ":", nrow(temp))) }
      }
    }
    # Drug Flags. 
    if(!is.null(my_alcohol_filters)) {
      if(any(my_alcohol_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Alcohol") | ssacode %in% c("Alcohol") | tsacode %in% c("Alcohol"))
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      } else if(any(my_alcohol_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Alcohol") & !ssacode %in% c("Alcohol") & !tsacode %in% c("Alcohol"))
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Alcohol Flag:", my_alcohol_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_methamphetamine_filters)) {
      if(any(my_methamphetamine_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Methamphetamine") | ssacode %in% c("Methamphetamine") | tsacode %in% c("Methamphetamine"))
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      } else if(any(my_methamphetamine_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Methamphetamine") & !ssacode %in% c("Methamphetamine") & !tsacode %in% c("Methamphetamine"))
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Methamphetamine Flag:", my_methamphetamine_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_marijuana_filters)) {
      if(any(my_marijuana_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Marijuana") | ssacode %in% c("Marijuana") | tsacode %in% c("Marijuana"))
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      } else if(any(my_marijuana_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Marijuana") & !ssacode %in% c("Marijuana") & !tsacode %in% c("Marijuana"))
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Marijuana Flag:", my_marijuana_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_heroin_filters)) {
      if(any(my_heroin_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Heroin") | ssacode %in% c("Heroin") | tsacode %in% c("Heroin"))
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      } else if(any(my_heroin_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Heroin") & !ssacode %in% c("Heroin") & !tsacode %in% c("Heroin"))
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Heroin Flag:", my_heroin_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_other_opiates_filters)) {
      if(any(my_other_opiates_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Other Opiates") | ssacode %in% c("Other Opiates") | tsacode %in% c("Other Opiates"))
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      } else if(any(my_other_opiates_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Other Opiates") & !ssacode %in% c("Other Opiates") & !tsacode %in% c("Other Opiates"))
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Other Opiates Flag:", my_other_opiates_filters, ":", nrow(temp))) }
      }
    }
    if(!is.null(my_any_opioids_filters)) {
      if(any(my_any_opioids_filters == "Yes")) {
        temp <- temp %>% filter(psacode %in% c("Heroin", "Other Opiates") | ssacode %in% c("Heroin", "Other Opiates") | tsacode %in% c("Heroin", "Other Opiates"))
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      } else if(any(my_any_opioids_filters == "No")) {
        temp <- temp %>% filter(!psacode %in% c("Heroin", "Other Opiates") & !ssacode %in% c("Heroin", "Other Opiates") & !tsacode %in% c("Heroin", "Other Opiates"))
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("Any Opioid Flag:", my_any_opioids_filters, ":", nrow(temp))) }
      }
    }
    # Third row. 
    my_top_providers_filters <- input$input_top_providers_discharge
    if(!is.null(my_top_providers_filters)) {
      temp <- temp %>% filter(programRecode %in% my_top_providers_filters)
      if(debug_filter == 1) { print(paste("Top Providers:", my_top_providers_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_service_setting_filters)) {
      temp <- temp %>% filter(srvset %in% my_service_setting_filters)
      if(debug_filter == 1) { print(paste("Service Setting:", my_service_setting_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_integrated_treatment_filters)) {
      temp <- temp %>% filter(inttx %in% my_integrated_treatment_filters)
      if(debug_filter == 1) { print(paste("Integrated Treatment:", my_integrated_treatment_filters, ":", nrow(temp), sep = " ")) }
    }
    if(!is.null(my_mat_filters)) {
      if(any(my_mat_filters == "Yes")) {
        temp <- temp %>% filter(opioid %in% c("Yes"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else if(any(my_mat_filters == "No")) {
        temp <- temp %>% filter(!opioid %in% c("No"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else if(any(my_mat_filters == "NA")) {
        temp <- temp %>% filter(!opioid %in% c("NA"))
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      } else {
        if(debug_filter == 1) { print(paste0("MAT Flag:", my_mat_filters, ":", nrow(temp))) }
      }
    }
    return(temp)
  })
  
  # Fiscal year/quarter. 
  output$select_fiscal_year_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_select_fiscal_year_discharge", 
                         label = "Fiscal Year", 
                         choices = c("2020", "2021", "2022", "2023"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$select_fiscal_quarter_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_select_fiscal_quarter_discharge", 
                         label = "Fiscal Quarter", 
                         choices = c("Q1", "Q2", "Q3", "Q4"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  # output$select_fiscal_year_quarter_discharge <- renderUI({
  #   checkboxGroupButtons(inputId = "input_select_fiscal_year_quarter_discharge", 
  #                        label = "Fiscal Year-Quarter", 
  #                        choices = sort(as.character(unique(filter_preprocess_data()$FYQtr))), 
  #                        selected = NULL, 
  #                        size = "sm", 
  #                        direction = "vertical")
  # })
  output$iv_flag_discharge <- renderUI({
    radioGroupButtons(inputId = "input_iv_flag_discharge", 
                      label = "IV Flag (< 30 days)", 
                      choices = c("IV drug use", "No IV drug use", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "horizontal")
  })
  output$pregnant_discharge <- renderUI({
    radioGroupButtons(inputId = "input_pregnant_discharge", 
                      label = "Pregnant", 
                      choices = c("Yes", "No", "NA", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "horizontal")
  })
  output$mdoc_flag_discharge <- renderUI({
    radioGroupButtons(inputId = "input_mdoc_flag_discharge", 
                      label = "MDOC Flag (Prison, Jail, Parole)", 
                      choices = c("MDOC Involvement", "No MDOC Involvement", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Primary substance of abuse. 
  output$primary_substance_discharge <- renderUI({
    pickerInput(inputId = "input_primary_substance_discharge", 
                label = "Primary Substance", 
                choices = sort(as.character(unique(filter_preprocess_discharge_data()$psacode))), 
                selected = NULL, 
                multiple = TRUE)
  })
  output$last_30_days_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_last_30_days_discharge", 
                         label = "Use in Last 30 Days", 
                         choices = sort(as.character(unique(filter_preprocess_discharge_data()$psause))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$dx_severity_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_dx_severity_discharge",
                         label = "Dx Severity", 
                         choices = c("Mild", "Moderate/Severe", "Withdrawal", "Remission", "Other"), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  # Demographics. 
  output$adult_child_discharge <- renderUI({
    radioGroupButtons(inputId = "input_adult_child_discharge", 
                      label = "Adult or Child", 
                      choices = c("Adult", "Child", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$gender_discharge <- renderUI({
    radioGroupButtons(inputId = "input_gender_discharge", 
                      label = "Gender", 
                      choices = c("Female", "Male", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$discharge_county <- renderUI({
    selectInput(inputId = "input_discharge_county", 
                label = "Select County Filter", 
                choices = sort(c(as.character(unique(filter_preprocess_discharge_data()$county)), "none")), 
                selected = "none", 
                multiple = FALSE)
  })
  output$discharge_reference_county <- renderUI({
    pickerInput(inputId = "input_discharge_reference_county", 
                label = "Select Reference County", 
                choices = sort(c(as.character(unique(filter_preprocess_discharge_data()$county)), "none")), 
                selected = "none", 
                multiple = TRUE)
  })
  # Drug flags. 
  output$alcohol_discharge <- renderUI({
    radioGroupButtons(inputId = "input_alcohol_discharge", 
                      label = "Alcohol", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$methamphetamine_discharge <- renderUI({
    radioGroupButtons(inputId = "input_methamphetamine_discharge", 
                      label = "Methamphetamine", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$marijuana_discharge <- renderUI({
    radioGroupButtons(inputId = "input_marijuana_discharge", 
                      label = "Marijuana", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$heroin_discharge <- renderUI({
    radioGroupButtons(inputId = "input_heroin_discharge", 
                      label = "Heroin", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$other_opiates_discharge <- renderUI({
    radioGroupButtons(inputId = "input_other_opiates_discharge", 
                      label = "Other Opiates", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  output$any_opioids_discharge <- renderUI({
    radioGroupButtons(inputId = "input_any_opioids_discharge", 
                      label = "Any Opioids", 
                      choices = c("Yes", "No", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Third row. 
  output$top_providers_discharge <- renderUI({
    multiInput(inputId = "input_top_providers_discharge",
               label = "Top Providers (90% of Admissions)", 
               choices = sort(as.character(unique(filter_discharge_top_providers()$programRecode))), 
               selected = NULL)
  })
  output$service_setting_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_service_setting_discharge", 
                         label = "Service Setting", 
                         choices = sort(as.character(unique(filter_preprocess_discharge_data()$srvset))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "horizontal")
  })
  output$integrated_treatment_discharge <- renderUI({
    checkboxGroupButtons(inputId = "input_integrated_treatment_discharge", 
                         label = "Integrated Treatment", 
                         choices = sort(as.character(unique(filter_preprocess_discharge_data()$inttx))), 
                         selected = NULL, 
                         size = "sm", 
                         direction = "vertical")
  })
  output$mat_discharge <- renderUI({
    radioGroupButtons(inputId = "input_mat_discharge", 
                      label = "MAT", 
                      choices = c("Yes", "No", "NA", "No Filter"), 
                      selected = "No Filter", 
                      size = "sm", 
                      direction = "vertical")
  })
  # Render plots. 
  output$discharges_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      my_county_filter <- input$input_discharge_county
      temp <- filter_discharge_data()
      
      if(!is.null(my_county_filter)) {
        if(my_county_filter != "none"){
          temp <- temp %>% filter(county %in% my_county_filter)
          if(debug_filter == 1) { print(paste0("County Filter:", my_county_filter, ":", nrow(temp)))  }
        }
      }
      
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_discharge_data()) != 0) {
            my_stats <- temp %>% count(FYQtr) %>%
              mutate_if(is.factor, as.character) %>% arrange(FYQtr)
            
            ggplot(data = my_stats, aes(x = FYQtr, y = n, fill = FYQtr)) + geom_bar(stat = "identity") + 
              geom_text(aes(label = n), vjust = -0.2, colour = "black") +
              labs(title = "ACCMHS SUD Discharges") + 
              xlab("Fiscal Year + Quarter") + 
              ylab("Number of BH-TEDS Discharges") + 
              scale_fill_hue(l = 50, c = 50)
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        },
        error = function(e) {
          warning("ggplot2 failed to render discharge count plot!")
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in inability to render plot!!!'))
          xlab(NULL) 
        }
      )
    }, width = 854, height = 480)
  
  output$discharge_days_to_treatment_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      # Set defaults so ggplot2 doesn't crash. 
      my_county_filter <- input$input_discharge_county
      my_reference_county_filter <- input$input_discharge_reference_county
      
      if(is.null(my_county_filter) || my_county_filter == "none") {
        my_county_filter <- "Allegan"
      }
      if(is.null(my_reference_county_filter) || my_reference_county_filter %in% "none") {
        my_reference_county_filter <- c("Barry", "Oakland")
      }
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_discharge_data()) != 0) {
            my_state_stats <- filter_discharge_data() %>% 
              group_by(FYQtr) %>% summarise(mean_state = round(mean(lngthsty), digits = 2))
            
            my_county_stats <- filter_discharge_data() %>% 
              filter(county %in% my_county_filter) %>% 
              group_by(FYQtr) %>% summarise(mean_county = round(mean(lngthsty), digits = 2))
            
            my_ref_county_stats <- filter_discharge_data() %>% 
              filter(county %in% my_reference_county_filter) %>% 
              group_by(FYQtr) %>% summarise(mean_ref_county = round(mean(lngthsty), digits = 2))
            
            my_stats <- merge(my_state_stats, my_county_stats, by = "FYQtr", all = TRUE)
            my_stats <- merge(my_stats, my_ref_county_stats, by = "FYQtr", all = TRUE)
            my_stats <- my_stats %>% replace(is.na(.), 0)
            
            my_stats2 <- my_stats %>% pivot_longer(cols = c("mean_state", "mean_county", "mean_ref_county"), names_to = "mean_type", values_to = "mean")
            
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(FYQtr, mean_type), .fns = factor))
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(mean), .fns = as.double))
            my_stats2 <- my_stats2 %>% mutate(mean_type = factor(mean_type, levels = c("mean_state", "mean_county", "mean_ref_county")))
            
            ggplot(data = my_stats2, aes(x = FYQtr, y = mean, fill = fct_rev(mean_type))) + 
              geom_col(position = "dodge") + 
              geom_text(aes(label = mean), vjust = 0.3, hjust = 1.5, colour = "black", position = position_dodge(width = 1)) + 
              scale_y_discrete(breaks = NULL, expand = c(0, 2)) +
              scale_fill_discrete(breaks = c("mean_state", "mean_county", "mean_ref_county"), labels = c("State", "County", "Ref. County(ies)")) + 
              labs(title = "Length of Treatment", 
                   subtitle = paste0("State: MI", "\nCounty: ", my_county_filter, "\nRef. County(ies): ", paste(my_reference_county_filter, collapse = ","))) + 
              guides(fill = guide_legend(title = "Mean of: ")) + 
              xlab("Fiscal Year + Quarter") + 
              ylab("Average Days in Treatment") + coord_flip()
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        }, 
        error = function(e) {
          warning("ggplot2 failed to render discharge length of treatment plot!")
          # Reference: https://stackoverflow.com/questions/12518387/can-i-create-an-empty-ggplot2-plot-in-r
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in plot rendering error!!!')) +
            xlab(NULL) #optional, but safer in case another theme is applied later
        }
      )
    }, width = 480, height = 854)
  
  output$discharge_reason_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      # Set defaults so ggplot2 doesn't crash. 
      my_county_filter <- input$input_discharge_county
      my_reference_county_filter <- input$input_discharge_reference_county
      
      if(is.null(my_county_filter) || my_county_filter == "none") {
        my_county_filter <- "Allegan"
      }
      if(is.null(my_reference_county_filter) || my_reference_county_filter %in% "none") {
        my_reference_county_filter <- c("Barry", "Oakland")
      }
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_discharge_data()) != 0) {
            my_state_stats <- filter_discharge_data() %>% 
              count(reason) %>% mutate(percent_state = round(n / nrow(filter_discharge_data()), 2))
            
            my_filtered_data <- filter_discharge_data() %>% filter(county %in% my_county_filter)
            my_county_stats <- my_filtered_data %>% 
              count(reason) %>% mutate(percent_county = round(n / nrow(my_filtered_data), 2))
            
            my_filtered_data <- filter_discharge_data() %>% filter(county %in% my_reference_county_filter)
            my_ref_county_stats <- my_filtered_data %>% 
              count(reason) %>% mutate(percent_ref_county = round(n / nrow(my_filtered_data), 2))
            
            my_stats <- merge(my_state_stats, my_county_stats, by = "reason", all = TRUE)
            my_stats <- merge(my_stats, my_ref_county_stats, by = "reason", all = TRUE)
            my_stats <- my_stats %>% mutate(reason = replace_na(reason, "NA"))
            my_stats <- my_stats %>% replace(is.na(.), 0) %>% select(reason, percent_state, percent_county, percent_ref_county)
            
            my_stats2 <- my_stats %>% pivot_longer(cols = c("percent_state", "percent_county", "percent_ref_county"), names_to = "percent_type", values_to = "percent")
            
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(reason, percent_type), .fns = factor))
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(percent), .fns = as.double))
            my_stats2 <- my_stats2 %>% mutate(percent_type = factor(percent_type, levels = c("percent_state", "percent_county", "percent_ref_county")))
            
            ggplot(data = my_stats2, aes(x = reason, y = percent, fill = fct_rev(percent_type))) + 
              geom_col(position = "dodge") + 
              geom_text(aes(label = paste0(percent * 100, "%")), vjust = 0.3, hjust = 1.5, colour = "black", position = position_dodge(width = 1)) + 
              scale_y_discrete(breaks = NULL, expand = c(0, 0.15)) +
              scale_fill_discrete(breaks = c("percent_state", "percent_county", "percent_ref_county"), labels = c("State", "County", "Ref. County(ies)")) + 
              labs(title = "Discharge Reason", 
                   subtitle = paste0("State: MI", "\nCounty: ", my_county_filter, "\nRef. County(ies): ", paste(my_reference_county_filter, collapse = ","))) + 
              guides(fill = guide_legend(title = "Percent of: ")) + 
              xlab("Type of Reason") + 
              ylab("Percentage") + coord_flip()
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        }, 
        error = function(e) {
          warning("ggplot2 failed to render discharge reason plot!")
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in plot rendering error!!!')) +
            xlab(NULL) #optional, but safer in case another theme is applied later
        }
      )
    }, width = 480, height = 854)
  
  output$discharge_change_in_use_plot <- renderPlot(
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      # Set defaults so ggplot2 doesn't crash. 
      my_county_filter <- input$input_discharge_county
      my_reference_county_filter <- input$input_discharge_reference_county
      
      if(is.null(my_county_filter) || my_county_filter == "none") {
        my_county_filter <- "Allegan"
      }
      if(is.null(my_reference_county_filter) || my_reference_county_filter %in% "none") {
        my_reference_county_filter <- c("Barry", "Oakland")
      }
      tryCatch( #Fail gracefully if over-filtered into empty dataframe. 
        {
          if(nrow(filter_discharge_data()) != 0) {
            my_state_stats <- filter_discharge_data() %>% 
              count(psause_Change2) %>% mutate(percent_state = round(n / nrow(filter_discharge_data()), 2))
            
            my_filtered_data <- filter_discharge_data() %>% filter(county %in% my_county_filter)
            my_county_stats <- my_filtered_data %>% 
              count(psause_Change2) %>% mutate(percent_county = round(n / nrow(my_filtered_data), 2))
            
            my_filtered_data <- filter_discharge_data() %>% filter(county %in% my_reference_county_filter)
            my_ref_county_stats <- my_filtered_data %>% 
              count(psause_Change2) %>% mutate(percent_ref_county = round(n / nrow(my_filtered_data), 2))
            
            my_stats <- merge(my_state_stats, my_county_stats, by = "psause_Change2", all = TRUE)
            my_stats <- merge(my_stats, my_ref_county_stats, by = "psause_Change2", all = TRUE)
            my_stats <- my_stats %>% mutate(psause_Change2 = replace_na(psause_Change2, "NA"))
            my_stats <- my_stats %>% replace(is.na(.), 0) %>% select(psause_Change2, percent_state, percent_county, percent_ref_county)
            
            my_stats2 <- my_stats %>% pivot_longer(cols = c("percent_state", "percent_county", "percent_ref_county"), names_to = "percent_type", values_to = "percent")
            
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(psause_Change2, percent_type), .fns = factor))
            my_stats2 <- my_stats2 %>% mutate(across(.cols = c(percent), .fns = as.double))
            my_stats2 <- my_stats2 %>% mutate(percent_type = factor(percent_type, levels = c("percent_state", "percent_county", "percent_ref_county")))
            
            ggplot(data = my_stats2, aes(x = psause_Change2, y = percent, fill = fct_rev(percent_type))) + 
              geom_col(position = "dodge") + 
              geom_text(aes(label = paste0(percent * 100, "%")), vjust = 0.3, hjust = 1.5, colour = "black", position = position_dodge(width = 1)) + 
              scale_y_discrete(breaks = NULL, expand = c(0, 0.15)) +
              scale_fill_discrete(breaks = c("percent_state", "percent_county", "percent_ref_county"), labels = c("State", "County", "Ref. County(ies)")) + 
              labs(title = "Change in Use (Primary Substance of Abuse)", 
                   subtitle = paste0("State: MI", "\nCounty: ", my_county_filter, "\nRef. County(ies): ", paste(my_reference_county_filter, collapse = ","))) + 
              guides(fill = guide_legend(title = "Percent of: ")) + 
              xlab("Type of Change") + 
              ylab("Percentage") + coord_flip()
          } else {
            ggplot() +
              theme_void() +
              geom_text(aes(0,0,label='Filtering resulted in empty dataframe!!!')) +
              xlab(NULL)
          }
        }, 
        error = function(e) {
          warning("ggplot2 failed to render discharge change in use plot!")
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label='Filtering resulted in plot rendering error!!!')) +
            xlab(NULL) #optional, but safer in case another theme is applied later
        }
      )
    }, width = 480, height = 854)
  
  #####################################################################################################
  # Demographics Visualizations Tab
  #####################################################################################################
  
  options(shiny.maxRequestSize=30*1024^2) # change max file upload size limit.
  
  # Update data source if user uploads a file.
  data_source <- reactive({
    # Reference: https://shiny.rstudio.com/gallery/file-upload.html
    tryCatch(
      {
        if(!is.null(input$select_file$datapath)) {
          df <- read.spss(
            file = input$select_file$datapath,  
            use.value.labels = TRUE, 
            to.data.frame = TRUE,
            trim.factor.names = TRUE, 
            trim_values = TRUE, 
            use.missings = TRUE, 
            add.undeclared.levels = "append")
          return(df) # Use uploaded dataset.
        } else {
          if(is.null(input$input_select_admissions_or_discharge) || input$input_select_admissions_or_discharge == "admissions") {
            return(bhteds_admissions_data_full)
          } else {
            return(bhteds_discharge_data_full)
          }
        }
      },
      error = function(e) {
        # stop(safeError(e)) # Show user error message
        warning("Error importing data: defaulting to Admissions FY 2020-2022 Q1-Q4")
        return(bhteds_admissions_data_full)
      }
    )
  })
  
  # Create variable labels for SPSS imported data. 
  create_variable_labels <- function(spss_dataframe = NULL) {
    as.data.frame(attributes(spss_dataframe)$variable.labels) %>% 
      # rename(variable_label = `attributes(spss_dataframe)$variable.labels`) %>% 
      mutate(variable = row.names(as.data.frame(attributes(spss_dataframe)$variable.labels)), 
             variable_label = ifelse(`attributes(spss_dataframe)$variable.labels` == "", 
                                     paste0(variable, ":No Label found!"), `attributes(spss_dataframe)$variable.labels`)
      )
  }
  
  # Update data source variable labels if user uploads a file. 
  data_source_labels <- reactive({
    # Reference: https://stackoverflow.com/questions/29378493/update-on-file-upload-shiny-r
    if(!is.null(attributes(data_source())$variable.labels)) { # Extract only if data exists.
      my_variable_labels <- create_variable_labels(spss_dataframe = data_source())
      my_variable_labels <- my_variable_labels %>% filter(!my_variable_labels$variable %in% c("daystx", "income", "educat"))
    }
    else {
      if(is.null(input$input_select_admissions_or_discharge) || input$input_select_admissions_or_discharge == "admissions") {
        my_variable_labels <- bhteds_admissions_labels
      } else {
        my_variable_labels <- bhteds_discharge_labels
      }
    }
    # Convert variable labels to list format.
    variable_labels_list <- list()
    for(i in 1:nrow(my_variable_labels)) {
      variable_labels_list[my_variable_labels[i, 1]] <- my_variable_labels[i, 2]
    }
    # Create labels for variables without one (or modify them).
    variable_labels_list["anyopioid"] <- "Medication Assisted Opioid Treatment (any opioid)"
    variable_labels_list["daystx2"] <- "Days to Treatment Group"
    variable_labels_list["ageyr10"] <- "Age Group"
    variable_labels_list["adultchild"] <- "Adult/Child"
    variable_labels_list["educatGrp"] <- "Education"
    variable_labels_list["income2"] <- "Income Group"
    
    variable_labels_list_reversed <- list()
    for(i in 1:nrow(my_variable_labels)) {
      variable_labels_list_reversed[my_variable_labels[i, 2]] <- my_variable_labels[i, 1]
    }
    return(list(labels = variable_labels_list, labels_reversed = variable_labels_list_reversed, 
                labels_df = my_variable_labels))
  })
  
  output$select_admissions_or_discharge <- renderUI({
    pickerInput(inputId = "input_select_admissions_or_discharge", 
                label = "Dataset", 
                choices = c("admissions", "discharge"), 
                selected = "admissions", 
                multiple = FALSE)
  })
  
  output$select_fiscal_year_demographics <- renderUI({
    pickerInput(inputId = "input_select_fiscal_year_demographics", 
                label = "Fiscal Year", 
                choices = c("2020", "2021", "2022", "2023"), 
                selected = NULL, 
                multiple = TRUE)
  })
  output$select_fiscal_quarter_demographics <- renderUI({
    pickerInput(inputId = "input_select_fiscal_quarter_demographics", 
                label = "Fiscal Quarter", 
                choices = c("Q1", "Q2", "Q3", "Q4"), 
                selected = NULL, 
                multiple = TRUE)
  })
  
  output$select_variables <- renderUI({
    pickerInput(inputId = "input_select_variables",
                label = "Data Variables:",
                # width = "400px",
                choices = sort(c(data_source_labels()[[3]]$variable_label, "all variables")), # BH-TEDS variables in dataset.
                selected = "Integrated Tx",
                multiple = TRUE)
  })
  
  output$select_county <- renderUI({
    selectInput(inputId = "input_select_county",
                label = "County Filter:",
                width = "400px",
                choices = sort(as.character(unique(data_source()$county))), # BH-TEDS counties in dataset.
                selected = "Allegan",
                multiple = FALSE)
  })
  
  output$select_ref_county <- renderUI({
    pickerInput(inputId = "input_select_ref_county",
                label = "Reference County(ies) Filter(s):",
                # width = "400px",
                choices = sort(c(as.character(unique(data_source()$county)), "None")), # BH-TEDS counties in dataset.
                selected = "None",
                multiple = TRUE)
  })
  
  output$select_subset_variable <- renderUI({
    selectInput(inputId = "input_select_subset_variable",
                label = "Subset BH-TEDS data by:",
                width = "400px",
                choices = sort(c(data_source_labels()[[3]]$variable_label, "None")), # BH-TEDS variables in dataset.
                selected = "None",
                multiple = FALSE)
  })
  
  # Generate plots + tables (upon user clicking action button).
  create_plot_table <- eventReactive(eventExpr = input$action, {
    # Reference: https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive
    my_filters <- list()
    # Filter only if user selects something.
    if(!(input$input_select_subset_variable == "None") && !is.null(input$input_select_subset_variable)) {
      my_filters[[data_source_labels()[[2]][[input$input_select_subset_variable]]]] <- c(input$select_subset_operation, input$select_subset_variable_level)
    }
    # User defined data source.
    my_data <- data_source()
    my_labels <- data_source_labels()[[1]]
    
    # Fiscal years and quarters filtering.
    my_fiscal_year_filters <- input$input_select_fiscal_year_demographics
    my_fiscal_quarter_filters <- input$input_select_fiscal_quarter_demographics
    
    if(!is.null(my_fiscal_year_filters)) {
      my_data <- my_data %>% filter(FY %in% my_fiscal_year_filters)
    }
    if(!is.null(my_fiscal_quarter_filters)) {
      my_data <- my_data %>% filter(Qtr %in% my_fiscal_quarter_filters)
    }
    if(nrow(my_data) == 0) {
      show_alert(title = "Error: ", text = "Filtering/Subsetting resulted in no data!", type = "error")
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
    if(sapply(my_data %>% select(all_of(my_selected_vars)), is.numeric) == TRUE) {
      show_alert(title = "Warning: ", text = "Unbinned quantitative variable(s) selected!\nPossible rendering issues!", type = "warning")
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
    tryCatch(
      {
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
      }, 
      error = function(e) {
        warning("format_grobs() function failed!")
        return(NULL)
      }
    )
  }
  
  # Render ggplot and tableGrob().
  output$ggplot <- renderPlot(
    # Reference: https://github.com/rstudio/shiny/issues/2373
    execOnResize = FALSE, { # Don't recompute image when resizing browser window.
      req(credentials()$user_auth) # Render only if user authenticated.
      tryCatch(
        {
          gridExtra::grid.arrange(grobs = format_grobs()[[2]], ncol = 2) # Display plots + tables.
        }, 
        error = function(e) {
          warning("Failed to render plot! (likely due to failure in format_grobs() function)")
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label= paste0("Failed to render plot!\n(due to user filtering/subset choices)")), size = 20, color = "red") +
            xlab(NULL) #optional, but safer in case another theme is applied later
        }
      )
    }, width = 1920, height = dynamic_render_height) # Dynamic render output height.
  
  # Download rendered plots + tables (Note: Won't handle downloads properly in R-Studio (use external browser).
  output$download_output <- downloadHandler(
    # Note: Appears to execute once during initial app startup. 
    # Reference: https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app 
    req(credentials()$user_auth), # Render only if user authenticated.
    filename = paste0(input$file_name, ".pdf"), 
    tryCatch(
      {
        content = function(filename) {
          ggsave(filename = filename, 
                 plot = format_grobs()[[1]], 
                 device = "pdf", 
                 units = "in", width = 50, height = 24, dpi = 300, 
                 limitsize = FALSE)
        }
      }, 
      error = function(e) {
        warning("Failed to export/download rendered plot!")
      }
    )
  )
  
  # Disable download button if nothing to export. 
  observe({
    req(credentials()$user_auth)
    if(is.null(format_grobs())) {
      shinyjs::hide(id = "download_output")
    } else {
      shinyjs::show(id = "download_output")
    }
  })
  
  # Dynamically acquire levels within selected variable for subsetting data.
  observe({
    my_choices <- c("None")
    if(!(input$input_select_subset_variable == "None") && !is.null(input$input_select_subset_variable)) {
      my_choices <- unique(data_source()[data_source_labels()[[2]][[input$input_select_subset_variable]]])
      my_choices <- sort(my_choices[, 1])
    }
    updateSelectInput(session = getDefaultReactiveDomain(),
                      inputId =  "select_subset_variable_level",
                      choices = my_choices)
  })
}
#####################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)