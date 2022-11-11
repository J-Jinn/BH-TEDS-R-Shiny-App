
options(dplyr.summarise.inform = FALSE)

# Reference: https://stackoverflow.com/questions/58200240/mutate-a-column-selected-by-a-string-converted-to-symbol
# Reference: https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
# Reference: https://stackoverflow.com/questions/50639903/paste-variable-name-in-mutate-dplyr
# Reference: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti  
# Reference: https://rstats-tips.net/2020/07/31/get-rid-of-info-of-dplyr-when-grouping-summarise-regrouping-output-by-species-override-with-groups-argument/  

# Reference: https://r-pkgs.org/ 

#' @title BHTEDS Project Data Visualizations 
#'
#' @description Generates aggregate statistics for Michigan Behavioral Health Treatment Episode Data Set 
#'              and produces bar charts + summary tables to visualize statistics.
#' 
#' @param data Dataframe containing BHTEDS dataset.
#' 
#' @param labels_data List containing variable names with associated label.
#'        I.e. `var_lab_list["income2"] <- "Income Group"`.
#'        Used to generate title and x-axis labels for plots. 
#' 
#' @param county_filter String naming Michigan county to generate stats for (i.e. "Allegan").
#' @param ref_county_filter String naming Michigan county|ies| to compare against (i.e. "Barry").
#'        Will take the mean of multiple counties.
#' @param subset_filter List in the format of `my_filters[["var"]] <- c("relational_operator", "value")`.
#'        Subsets dataframe based on specified criteria.
#'        Subsetting could result in empty dataframe.
#'        Currently supports relational operators (!=, ==, >, <, >=, <=).
#'        Currently only supports specifying one value per relation.
#'        Perhaps easier to filter datframe beforehand before passing to function.
#' 
#' @param fiscal_year String naming part of title label for ggplot. 
#' @param stats_vars Vector of BHTEDS variable names to generate visualizations for.
#' @param ref_stat_type currently "mean" or "sum". 
#' @param ...    To be implemented.  
#' 
#' @details `ref_county_filter` is optional.
#'          `subset_filter` is optional.
#'          All other parameters are required.
#'          
#'                      
#' @author Joseph Jinn, \email{jinnjo@mail.gvsu.edu}
#' @references 
#' @concept 
#' @seealso 
#' @return List containing all plots and tables generated.
#'         
#' @examples
#' \dontrun{
#'
#' # Specify variables to generate plot + tables for.
#' my_vars <- c(
#' "inttx", "srvset", "prevtrmt", "psacode", "ssacode", "tsacode", "opioid", "anyopioid", "daystx2", "ageyr10", "adultchild", "gender", "gendid", "pregnant",
#' "race", "ethnic", "educatGrp", "mrtlstat", "empstat", "income2", "veteran", "livargmt", "corrstat")
#' 
#' # Create labels for variables without one (or modify them). 
#' var_lab_list["anyopioid"] <- "Medication Assisted Opioid Treatment (any opioid)"
#' var_lab_list["daystx2"] <- "Days to Treatment Group"
#' var_lab_list["ageyr10"] <- "Age Group"
#' var_lab_list["adultchild"] <- "Adult/Child"
#' var_lab_list["educatGrp"] <- "Education"
#' var_lab_list["income2"] <- "Income Group"
#' 
#' # Create filters to subset data. 
#' my_filters <- list()
#' my_filters[["gender"]] <- c("==", "Female")
#' 
#' # Call function. 
#' results <- visualize_demographics(
#'   data = lab0, 
#'   labels_data = var_lab_list, 
#'   county_filter = "Allegan", 
#'   ref_county_filter = c("Barry", "Oakland"), 
#'   subset_filter = my_filters, 
#'   fiscal_year = "FY2022 Q1-Q3", 
#'   stats_vars = my_vars)
#' 
#' names(results)
#' 
#' # Modify returned plots (ggplots) + tables (dataframes)
#' plot1 <- results[[1]]
#' plot1
#' 
#' table1 <- results[[2]]
#' table1 %>% kableExtra::kable()
#'
#' plot2 <- results[[3]] + scale_x_discrete(labels = function(x) str_wrap(x, width = 12))
#' plot2
#' 
#' table1 <- results[[4]]
#' table1 %>% kableExtra::kable()
#' }
#' 
#' @importFrom
#' @export

visualize_demographics <- function(data = NULL, labels_data = NULL, county_filter = NULL, ref_county_filter = NULL, subset_filter = NULL, 
                                   fiscal_year = NULL, stats_vars = NULL, ref_stat_type = "mean") {
  
  if(is.null(data) || is.null(labels_data) || is.null(county_filter) || is.null(stats_vars)) { stop("Required argument(s) missing!\n") }
  if(is.null(ref_stat_type)) { ref_stat_type = "" }
  if(!is.null(ref_county_filter) && !(ref_stat_type %in% c("mean", "median", "sum"))) { warning("Unrecognized ref_stat_type: defaulting to 'sum'\n") }
  
  # Filter data based on user-specified criteria (limited functionality, for now). 
  my_filter_string <- ""; counter <- 1;
  for(f in names(subset_filter)) { # for each subset filter
    my_filter_string <- paste0(my_filter_string, "(", names(subset_filter)[counter], subset_filter[[f]][1], subset_filter[[f]][2], ")")
    if(!is.null(data[[f]]) && length(subset_filter[[f]]) == 2) { # if column exists
      if(subset_filter[[f]][2] == "NA") {
        if(subset_filter[[f]][1] == "!=") { my_string <- paste0("!is.na(", names(subset_filter[f]), ")") 
        } else if(subset_filter[[f]][1] == "==") { my_string <- paste0("is.na(", names(subset_filter[f]), ")") }
      } else if(data[[f]] %>% class() %in% c("character", "factor", "logical")) {
        if(subset_filter[[f]][1] %in% c("!=", "==", ">", "<", ">=", "<=")) 
        { my_string <- paste0(names(subset_filter[f]), subset_filter[[f]][1], "\"", subset_filter[[f]][2], "\"") }
      } else if(data[[f]] %>% class() %in% c("numeric", "integer")) {
        if(subset_filter[[f]][1] %in% c("!=", "==", ">", "<", ">=", "<=")) 
        { my_string <- paste0(names(subset_filter[f]), subset_filter[[f]][1], subset_filter[[f]][2]) }
      } else { warning("Error: variable of unsupported data type\n") }
      if(!subset_filter[[f]][1] %in% c("!=", "==", ">", "<", ">=", "<=")) { warning("Only (!=, ==, >, <, >=, <=) operators supported\n")}
      my_expression <- rlang::parse_expr(my_string) %>% as.vector()
      data <- data %>% filter(!!my_expression)
    } else { warning("Error: variable (column) not found or improper filter specification\n")}
    counter = counter + 1
  }
  my_list <- list()
  for(i in 1:length(stats_vars)) { # Iterate through all variables of interest. 
    if(!(stats_vars[i] %in% names(labels_data))) { warning(paste0("Var: ", stats_vars[i], " not found\n")); next; } # Skip if variable not found. 
    
    stats_vars_sym <- sym(stats_vars[i]) # Packages expect to evaluate symbols/objects, not strings. 
    ############################################################################################################
    # Conditional structure to handle additional data manipulation on per-variable basis.
    # This can also be done outside of the function to reduce function code (provide modified data = dataframe). 
    ############################################################################################################
    if(stats_vars_sym == "anyopioid") {
      anyOpioid <- c("Heroin", "Other Opiates")
      my_state_data <- data %>% filter((psacode %in% anyOpioid) | (ssacode %in% anyOpioid) | (tsacode %in% anyOpioid))
      my_county_data <- my_state_data %>% filter(county == county_filter)
      stats_vars_sym <- sym("opioid")
    }
    else if(stats_vars_sym == "psacode" || stats_vars_sym == "ssacode" || stats_vars_sym == "tsacode") { 
      my_temp_data <- data %>% filter(county == county_filter)
      my_temp_data <- my_temp_data %>% group_by(!!stats_vars_sym) %>% summarise(n = n()) %>% 
        mutate(percent = (n / nrow(my_temp_data)) * 1.0) %>% arrange(desc(percent)) %>% 
        mutate(cumulative_percent = cumsum(percent)) %>% filter(percent >= 0.01) %>% 
        mutate(!!stats_vars_sym := as.factor(as.character(!!stats_vars_sym)))
      
      drug.list <- levels(my_temp_data[[stats_vars_sym]])
      
      my_state_data <- data %>%
        mutate(var1 = ifelse(!(as.character(!!stats_vars_sym) %in% drug.list),
                             "Other", as.character(!!stats_vars_sym)))
      my_state_data$var1 <- factor(my_state_data$var1, levels = c(drug.list, 'Other'))
      my_county_data <- my_state_data %>% filter(county == county_filter)
      
      county_stats <- my_county_data %>% count(var1) %>% mutate(percent = (n / nrow(my_county_data)) * 1.0)
      state_stats <- my_state_data %>% count(var1) %>% mutate(percent = (n / nrow(my_state_data)) * 1.0)
      
      county_stats$var1 <- factor(county_stats$var1, levels = c(drug.list, 'Other'))
      state_stats$var1 <- factor(state_stats$var1, levels = c(drug.list, 'Other'))
      stats_vars_sym <- sym("var1")
    }
    else if(stats_vars_sym == "daystx2") {
      daystx2.list <- c('Within 7 days', 'Within 14 days', 'Within 30 days', 'More than 30 days')
      my_state_data <- data %>% 
        mutate(daystx2 = ifelse(daystx < 8, "Within 7 days", 
                                ifelse(daystx < 15, "Within 14 days",
                                       ifelse(daystx < 31, "Within 30 days", "More than 30 days"))))
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "ageyr10") {
      my_state_data <- data %>% filter(!(ageyr10 %in% c( 'under 14'))) #'65+'
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "adultchild") {
      my_state_data <- data %>% mutate(adultchild = ifelse(ageyears >= 18, "Adult", "Child"))
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "pregnant") {
      my_state_data <- data %>% filter(pregnant %in% c("Yes", "No")) %>% filter(gender == "Female")
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "ethnic") {
      my_state_data <- data %>% filter(ethnic != 97)
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "educatGrp") {
      educ.list <- c('Grade 12 or GED', ' Vocational School', ' 1 Year of College', ' 2 Years of College', 
                     '3 Years of College', '4 Years of College or BS BA', 'Grad School')
      fun <- function(z) {
        z[!(z %in% educ.list)] <- "Less than Grade 12 or GED"
        return(z)
      }
      my_state_data <- data
      my_state_data$educatGrp <- fct_relabel(factor(my_state_data$educat), fun)
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "income2") {
      income.list <- c('Less than \n $10,000', '$10,000 to \n $19,999', '$20,000 to \n $29,999', 
                       '$30,000 to \n $39,999', '$40,000 to \n $49,999', '$50,000 \n or more')
      my_state_data <- data %>% 
        mutate(income2 = ifelse(income < 10000, "Less than \n $10,000", 
                                ifelse(income < 20000, "$10,000 to \n $19,999",
                                       ifelse(income < 30000, "$20,000 to \n $29,999",
                                              ifelse(income < 40000, "$30,000 to \n $39,999",
                                                     ifelse(income < 50000, "$40,000 to \n $49,999", "$50,000 \n or more"))))),
               income3 = ifelse(income < 10000, "Less than $10,000", 
                                ifelse(income < 20000, "$10,000 to $19,999",
                                       ifelse(income < 30000, "$20,000 to $29,999",
                                              ifelse(income < 40000, "$30,000 to $39,999",
                                                     ifelse(income < 50000, "$40,000 to $49,999", "$50,000 or more"))))),)
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    else if(stats_vars_sym == "corrstat") {
      corr.list <- c('In Prison', 'In Jail', 'Parole', 'Probation', 'None')
      my_state_data <- data %>% mutate(var1 = as.factor(ifelse(!(corrstat %in% corr.list), "Other", corrstat)))
      levels(my_state_data$var1) <- list("In Prison" = "1",  "In Jail" = "2", "Parole" = "3", "Probation" = "4",  
                                         "None" = "11", "Other" = "Other")
      my_county_data <- my_state_data %>% filter(county == county_filter)
      stats_vars_sym <- sym("var1")
    } 
    else {
      my_state_data <- data
      my_county_data <- my_state_data %>% filter(county == county_filter)
    }
    ############################################################################################################
    ############################################################################################################
    if(nrow(my_state_data) == 0 || nrow(my_county_data) == 0) { # Error checking for filtering data.
      my_list[[paste0(stats_vars[i], " plot")]] <- paste0("filtering resulted in no data for ", stats_vars[i])
      my_list[[paste0(stats_vars[i], " table")]] <- paste0("filtering resulted in no data for ", stats_vars[i])
      next
    }
    # Compute required aggregate statistics.
    county_stats <- my_county_data %>% count(!!stats_vars_sym) %>% mutate(percent = (n / nrow(my_county_data)) * 1.0) %>% 
      mutate_if(is.factor, as.character) %>% arrange(!!stats_vars_sym)
    state_stats <- my_state_data %>% count(!!stats_vars_sym) %>% mutate(percent = (n / nrow(my_state_data)) * 1.0) %>% 
      mutate_if(is.factor, as.character) %>% arrange(!!stats_vars_sym)
    
    county_table_stats <- my_county_data %>% 
      count(!!stats_vars_sym) %>% mutate(!!paste0(county_filter, " (n)") := n, 
                                         !!paste0(county_filter, " (%)") := 
                                           paste0(round((n / nrow(my_county_data)) * 100.0, digits = 1), "%")) %>% select(-n) 
    state_table_stats <- my_state_data %>% 
      count(!!stats_vars_sym) %>% mutate("MI (n)" = n, 
                                         "MI (%)" = paste0(round((n / nrow(my_state_data)) * 100.0, digits = 1), "%")) %>% select(-n)
    my_table <- full_join(x = county_table_stats, y = state_table_stats, by = setNames(as.character(stats_vars_sym), as.character(stats_vars_sym)))
    
    if(!is.null(ref_county_filter)) # Compute reference county stats. 
    {
      my_reference_data <- my_state_data %>% filter(county %in% ref_county_filter) 
      # TODO: Optimize redundant code for computing aggregate reference stats.
      if(ref_stat_type == "mean") {
        # Compute mean of reference county(ies) for plot. 
        reference_stats <- my_reference_data %>% group_by(!!stats_vars_sym, county) %>% summarise(n = n(), percent = (n / nrow(my_reference_data)) * 1.0)
        reference_stats <- reference_stats %>% group_by(!!stats_vars_sym) %>% summarise(n = sum(n), percent = mean(percent))
        # Compute mean of reference county(ies) for table. 
        reference_table_stats <- my_reference_data %>% group_by(!!stats_vars_sym, county) %>% 
          summarise(n = n(), percent = (n / nrow(my_reference_data)) * 100.0)
        reference_table_stats <- reference_table_stats %>% group_by(!!stats_vars_sym) %>% 
          summarise("Reference County(ies) (n)" := sum(n), 
                    "Reference County(ies) (%)" := paste0(round(mean(percent), digits = 1), "%"))
      } else if(ref_stat_type == "median") {
        # Compute median of reference county(ies) for plot. 
        reference_stats <- my_reference_data %>% group_by(!!stats_vars_sym, county) %>% summarise(n = n(), percent = (n / nrow(my_reference_data)) * 1.0)
        reference_stats <- reference_stats %>% group_by(!!stats_vars_sym) %>% summarise(n = sum(n), percent = median(percent))
        # Compute median of reference county(ies) for table. 
        reference_table_stats <- my_reference_data %>% group_by(!!stats_vars_sym, county) %>% 
          summarise(n = n(), percent = (n / nrow(my_reference_data)) * 100.0)
        reference_table_stats <- reference_table_stats %>% group_by(!!stats_vars_sym) %>% 
          summarise("Reference County(ies) (n)" := sum(n), 
                    "Reference County(ies) (%)" := paste0(round(median(percent), digits = 1), "%"))
      } else {
        # Compute combined total of  reference county(ies) for plot. 
        reference_stats <- my_reference_data %>% count(!!stats_vars_sym) %>% 
          mutate(percent = (n / nrow(my_reference_data)) * 1.0)
        # Compute combined total of reference county(ies) for table. 
        reference_table_stats <- my_reference_data %>% 
          count(!!stats_vars_sym) %>% 
          # Comment/uncommnent ONE of the mutate() below to identify/de-identify reference county(ies) in table.  
          # mutate(!!paste0(paste0(ref_county_filter, collapse = " "), " (n)") := n, 
          #        !!paste0(paste0(ref_county_filter, collapse = " "), " (%)" ) := 
          #          paste0(round((n / nrow(my_reference_data)) * 100.0, digits = 2), "%")) %>% select(-n) 
          mutate("Reference County(ies) (n)" = n, 
                 "Reference County(ies) (%)" = paste0(round((n / nrow(my_reference_data)) * 100.0, digits = 1), "%")) %>% select(-n)
      }
      my_table <- full_join(x = my_table, y = reference_table_stats, by = setNames(as.character(stats_vars_sym), as.character(stats_vars_sym)))
      
      # TODO: Eliminate necessity of redundant code below. 
      my_table <- my_table %>% 
        mutate_if(is.factor, as.character) %>% arrange(!!stats_vars_sym) %>% rename(!!paste0(labels_data[stats_vars[i]]) := 1) %>% 
        mutate_all(~ replace(., is.na(.), 0)) %>% mutate(across(c(3, 5, 7), ~ replace(., . == 0, "0.0%")))
    } else {
      my_table <- my_table %>% 
        mutate_if(is.factor, as.character) %>% arrange(!!stats_vars_sym) %>% rename(!!paste0(labels_data[stats_vars[i]]) := 1) %>% 
        mutate_all(~ replace(., is.na(.), 0)) %>% mutate(across(c(3, 5), ~ replace(., . == 0, "0.0%")))
    }
    # TODO: Optimize ggplot specific code. 
    my_plot <- ggplot() + # Generate base plot. 
      geom_bar(data = state_stats, mapping = aes(x = !!stats_vars_sym, y = percent, fill = as.factor("MI")), # MI columns.
               stat = "identity", width = .8) +
      geom_bar(data = county_stats, mapping = aes(x = !!stats_vars_sym, y = percent, fill = as.factor("County")), # County columns. 
               stat = "identity", width = .45, color = "black") +
      # Plot adjustments (text, labels, etc.). 
      scale_fill_manual(name  = "Legend", values = c("MI"  = "gray", "County" = "#1d5290")) +
      guides(color = guide_legend(override.aes = list(size = 7))) +
      ylab("Relative frequency") +
      xlab(labels_data[[stats_vars[i]]]) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%"),limits = c(0,1)) + # Convert proportions to percentages for y-axis.
      ggtitle(paste0(labels_data[[stats_vars[i]]], "\n", fiscal_year, " BH TEDS - ", county_filter, " County")) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    if(!is.null(ref_county_filter)) { # Add reference county stats to plot.
      my_label = paste("Sum of:\n", length(ref_county_filter), "County(ies)") # Default to sum label. 
      if(ref_stat_type %in% c("mean", "median")) {my_label = paste(ref_stat_type, "of:\n", length(ref_county_filter), "County(ies)")}
      my_plot <- my_plot + 
        geom_point(data = reference_stats, # Add reference county(ies) marker. 
                   mapping = aes(x = !!stats_vars_sym, y = percent, color = "ref_color"), 
                   size = 20, shape = "-", alpha = 0.90) + 
        scale_color_manual(name = "Reference Legend", breaks = c("ref_color"), # Add/adjust custom legend. 
                           values = c("ref_color" = "orange"), label = my_label) + 
        theme(legend.key = element_rect(fill = scales::alpha("white", 0.5), colour = "white"), # Adjust legend theme. 
              legend.key.size = unit(0.5, "cm")) +  
        guides(color = guide_legend(override.aes = list(size = 15, alpha = 0.75))) # Adjust legend symbol/element. 
      # Uncomment/comment ONE of the two lines below to identify/de-identify reference counties in plto. 
      # scale_color_discrete(labels = paste("Aggregate:\n", paste(paste0(ref_county_filter, "\n"), collapse = "", sep = ""), collapse = "", sep = ""))
    }
    if(length(names(subset_filter)) > 0) { # Indicate data filtered by what.
      my_plot <- my_plot + 
        labs(caption = paste0("Data Filtered By:", my_filter_string))
    }
    # Add text last so always on "top" layer of plot. 
    my_plot <- my_plot + 
      geom_text(data = county_stats, aes(x = !!stats_vars_sym, y = percent, label = paste0("(n = ", n, ")")), # Sample size. 
                position = position_dodge(width = 1),
                vjust = -0.5, size = 3, fontface = "bold") +
      geom_text(data = county_stats, aes(x = !!stats_vars_sym, y = percent, label = paste0(round(percent * 100.0, digits = 1), "%")), # relative freq. 
                position = position_dodge(width = 1),
                vjust = -2.0, size = 3, fontface = "bold") 
    
    my_list[[paste0(stats_vars[i], " plot")]] <- my_plot # Return all plots. 
    my_list[[paste0(stats_vars[i], " table")]] <- my_table # Return all tables. 
  }
  return(my_list)
}

# Reference: https://stackoverflow.com/questions/31776557/how-to-adjust-the-font-size-of-tablegrob  
# Reference: https://stackoverflow.com/questions/35208526/how-to-keep-column-names-when-using-as-data-frame 

#' @title BHTEDS Project Data Visualizations Helper Function
#'
#' @description Generates a modified theme for the tableGrob() object that visualizes BHTEDS aggregate statistics. 
#' 
#' @param table Dataframe containing a returned aggregate statistics table from the visualize_demographics() function. 
#' 
#' @param ...    To be implemented.  
#' 
#' @details Assumes 7 columns in the aggregate statistics dataframe/table. 
#'          
#'                      
#' @author Joseph Jinn, \email{jinnjo@mail.gvsu.edu}
#' @references 
#' @concept 
#' @return gridExtra::ttheme_default() object. 
#' 
#' @seealso \code{\link{visualize_demographics}}
#' 
#' @section
#'         
#' @examples
#' \dontrun{
#'
#` my_grid.table_theme(table = results[["srvset table"]])
#' }
#' 
#' @importFrom
#' @export
#' 
my_grid.table_theme <- function(table = NULL) {
  # Full matrix to define colors by columns (otherwise row-wise operation)
  my_cell_fill_colors <- matrix("white", nrow = nrow(table), ncol = ncol(table))
  my_cell_fill_colors[, 1] <- "white"
  my_cell_fill_colors[, 2:3] <- "#ACB9CA"
  my_cell_fill_colors[, 4:5] <- "#EDEDED"
  # Matrix to define column header colors. 
  my_column_header_fill_colors <- matrix("red", nrow = 1, ncol = ncol(table))
  my_column_header_fill_colors[, 1] <- "white"
  my_column_header_fill_colors[, 2:3] <- "#ACB9CA"
  my_column_header_fill_colors[, 4:5] <- "#EDEDED"
  
  if(ncol(table) > 5) { # Add columns only if reference county(ies) present. 
    my_cell_fill_colors[, 6:7] <- "#FFE36D"
    my_column_header_fill_colors[, 6:7] <- "#FFE36D" 
  }
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.8, # Table cell text size (excludes column headers). 
                               alpha=c(1.0), # Table cell text alpha/transparency. 
                               col="black"), # Table cell text color.
                
                bg_params=list(cex = 1.0, # Does nothing?
                               alpha=c(1.0), # Table cell fill color alpha/transparency.
                               col="black", # Table cell/grid line color.
                               fill=my_cell_fill_colors)), # Table cell fill color. 
    
    colhead = list(fg_params=list(cex = 1.0,  # Table column headers text size. 
                                  fontface=c("bold"), # Adjust font style.
                                  alpha=c(1.0), # Table column headers text alpha/transparency. 
                                  col="black"), # Table column headers text color.
                   
                   bg_params=list(cex=1.0, # Does nothing? 
                                  alpha=c(1.0), # Table column headers cell/grid line alpha/transparency. 
                                  col="black", # Table column headers cell/grid line color.
                                  fill=my_column_header_fill_colors)), # Table column headers cell fill color.
    
    rowhead = list(fg_params=list(cex = 0.7,  # Row ID number label text size. 
                                  alpha=c(0.0), # Row ID number label text alpha/transparency.
                                  col="black"), # Row ID number label text color.
                   
                   bg_params=list(cex=1.0, # Does nothing?
                                  alpha=c(0.0), # Row ID number label cell alpha/transparency.
                                  col="black", # Row ID number label cell/grid line color.
                                  fill="white")) # Row ID number label cell fill color.
  ) 
  return(mytheme)
}
