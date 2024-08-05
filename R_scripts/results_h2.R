# Function to set the working directory dynamically
set_working_directory <- function() {
  current_path <- dirname(normalizePath(sys.frame(1)$ofile))
  setwd(current_path)
}

# Call the function to set the working directory dynamically
set_working_directory()

# Load necessary data and functions
source("load_data.R")
source("ols.R")
source("coxph.R")
source("compare_coefficients.R")

# Function to check if a package is installed, install it if not, and load it
load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Load required packages
load_package("knitr")
load_package("kableExtra")
load_package("survival")
load_package("dplyr")
load_package("htmltools")

# Define the file paths
files <- list(
  "/Users/martin/Documents/__thesis/__analysis/bgg/eventnet_data/bgg_two_mode_novelty_COND_SIZE_DHE.csv",
  "/Users/martin/Documents/__thesis/__analysis/bgg/eventnet_data/bgg_two_mode_rating_COND_SIZE_DHE.csv"
)

# Function to calculate significance stars (properly vectorized)
calc_significance_stars <- function(pvalues) {
  sapply(pvalues, function(pvalue) {
    if (is.na(pvalue)) return("")
    if (pvalue < 0.001) return("***")
    else if (pvalue < 0.01) return("**")
    else if (pvalue < 0.05) return("*")
    else if (pvalue < 0.1) return("+")
    else return("")
  })
}

# Function to round and format coefficients with significance stars
round_and_format <- function(coefficient, pvalue) {
  rounded_coef <- round(as.numeric(coefficient), 2)
  significance_star <- calc_significance_stars(pvalue)
  paste0(format(rounded_coef, nsmall = 2), significance_star)
}

# Process each file and combine results
comparison_results_list <- lapply(files, function(file) {
  # Prepare the data
  coxph_input <- prepare_data(file, min_eventsize = 2, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE)
  ols_input <- prepare_data(file, min_eventsize = 2, observed=TRUE, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE)
  
  # Run the models
  coxph_model <- run_coxph(coxph_input)
  ols_model <- run_ols(ols_input)
  
  # Compare the coefficients and their significance levels
  comparison_result <- compare_coefficients(coxph_model, ols_model)
  
  # Round coefficients and add significance stars
  # comparison_result <- comparison_result %>%
  #   mutate(CoxPH_Coefficient = mapply(round_and_format, CoxPH_Coefficient, CoxPH_PValue),
  #          OLS_Coefficient = mapply(round_and_format, OLS_Coefficient, OLS_PValue),
  #          Same_Direction = ifelse(CoxPH_PValue < 0.1 & OLS_PValue < 0.1, 
  #                                  ifelse(Same_Direction, "<b>TRUE</b>", "<b>FALSE</b>"), 
  #                                  as.character(Same_Direction)))
  
  return(comparison_result)
})

# Combine the results side by side
combined_results <- Reduce(function(x, y) {
  merge(x, y, by = "Variable", suffixes = c("_novelty", "_usefulness"))
}, comparison_results_list)

# Extract the desired columns for the CSV
csv_data <- combined_results
# Define the output CSV file path
output_csv_file <- "../results/h2.csv"

# Save the extracted data to a CSV file
write.csv(csv_data, file = output_csv_file, row.names = FALSE)

# Print a message indicating the file has been saved
cat("CSV file has been saved to", output_csv_file, "\n")

# # Define styles for significant differences
# styles <- ifelse(grepl("<b>", combined_results$Same_Direction_novelty) | grepl("<b>", combined_results$Same_Direction_usefulness),
#                  "background-color: white; color: black;", "")

# # Prepare the table for output
# table_data <- combined_results %>%
#   select(Variable, 
#          CoxPH_Coefficient_novelty, CoxPH_Coefficient_usefulness, 
#          OLS_Coefficient_novelty, OLS_Coefficient_usefulness, 
#          Same_Direction_novelty, Same_Direction_usefulness) %>%
#   distinct()

# # Filter for significant rows
# significant_rows <- combined_results %>%
#   filter(grepl("<b>", Same_Direction_novelty) | grepl("<b>", Same_Direction_usefulness)) %>%
#   select(Variable, 
#          CoxPH_Coefficient_novelty, CoxPH_Coefficient_usefulness, 
#          OLS_Coefficient_novelty, OLS_Coefficient_usefulness, 
#          Same_Direction_novelty, Same_Direction_usefulness) %>%
#   distinct()

# # Function to generate HTML or LaTeX output
# generate_output <- function(output_format = "html", include_significant_only = FALSE) {
#   data_to_use <- if (include_significant_only) significant_rows else table_data
  
#   if (output_format == "html") {
#     html_table <- kable(data_to_use, format = "html", table.attr = "class='table table-striped'", escape = FALSE, row.names = FALSE, col.names = c("Variable", "Novelty", "Usefulness", "Novelty", "Usefulness", "Novelty", "Usefulness")) %>%
#       kable_styling(full_width = F) %>%
#       add_header_above(c(" " = 1, "CoxPH Coefficient" = 2, "OLS Coefficient" = 2, "Same Direction" = 2)) %>%
#       row_spec(1:nrow(data_to_use), extra_css = styles)
    
#     return(html_table)
#   } else if (output_format == "latex") {
#     latex_table <- kable(data_to_use, format = "latex", booktabs = TRUE, escape = TRUE, row.names = FALSE, col.names = c("Variable", "CoxPH Novelty", "CoxPH Usefulness", "OLS Novelty", "OLS Usefulness", "Same Direction Novelty", "Same Direction Usefulness")) %>%
#       #kable_styling(latex_options = c("striped", "hold_position")) %>%
#       add_header_above(c(" " = 1, "CoxPH Coefficient" = 2, "OLS Coefficient" = 2, "Same Direction" = 2))
    
#     return(latex_table)
#   } else {
#     stop("Unsupported output format. Please choose either 'html' or 'latex'.")
#   }
# }

# # Generate and print the output in the desired format, with option to include only significant rows
# output_format <- "latex"  # Change to "latex" for LaTeX output
# output_file <- "h2.tex"  # Specify the output file path if needed, e.g., "output.html"
# include_significant_only <- TRUE  # Change to FALSE to include all rows

# output <- generate_output(output_format, include_significant_only)

# # Print or save the output
# if (interactive() && output_format == "html") {
#   # Display HTML output in the viewer
#   print(output)
# } else if (!is.null(output_file)) {
#   writeLines(output, output_file)
# } else {
#   # Ensure the output is printed in non-interactive environments
#   cat(output, sep = "\n")
# }