library(dplyr)
library(knitr)
library(kableExtra)

# Function to set the working directory dynamically
set_working_directory <- function() {
  current_path <- dirname(normalizePath(sys.frame(1)$ofile))
  setwd(current_path)
}

# Call the function to set the working directory dynamically
set_working_directory()

# Load necessary data and functions
source("load_data.R")

# Define the file paths
files <- list(
  "../eventnet_data/bgg_two_mode_novelty_COND_SIZE_DHE.csv",
  "../eventnet_data/bgg_two_mode_rating_COND_SIZE_DHE.csv"
)

# Function to calculate significance stars
calc_significance_stars <- function(pvalues) {
  sapply(pvalues, function(pvalue) {
    if (is.na(pvalue)) return("")
    if (pvalue < 0.001) return("***")
    else if (pvalue < 0.01) return("**")
    else if (pvalue < 0.05) return("*")
    else if (pvalue < 0.1) return(".")
    else return("")
  })
}

# Function to process each file and return the results
process_file <- function(file) {
  a <- prepare_data(file, min_eventsize = 2, observed = TRUE, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE)
  data <- a$bgg_events %>% select(all_of(c(a$ivs, "WEIGHT")))

  # Select numeric covariates
  numeric_covariates <- names(data)[sapply(data, is.numeric) & names(data) != "WEIGHT"]

  # Split the data into top 25% and bottom 25% quantiles based on WEIGHT
  top_25 <- data %>% filter(WEIGHT >= quantile(WEIGHT, 0.75, na.rm = TRUE))
  bottom_25 <- data %>% filter(WEIGHT <= quantile(WEIGHT, 0.25, na.rm = TRUE))

  # Perform Shapiro-Wilk test for normality
  normality_tests <- lapply(numeric_covariates, function(covar) {
    list(
      top = if (length(unique(top_25[[covar]])) > 1) shapiro.test(top_25[[covar]]) else list(p.value = NA),
      bottom = if (length(unique(bottom_25[[covar]])) > 1) shapiro.test(bottom_25[[covar]]) else list(p.value = NA)
    )
  })

  names(normality_tests) <- numeric_covariates

  # Summarize the results for top 25% and bottom 25% groups
  top_25_summary <- top_25 %>%
    summarise(across(all_of(numeric_covariates), mean))

  bottom_25_summary <- bottom_25 %>%
    summarise(across(all_of(numeric_covariates), mean))

  # Perform appropriate tests for each covariate
  test_results <- lapply(numeric_covariates, function(covar) {
    is_normal <- !is.na(normality_tests[[covar]]$top$p.value) && !is.na(normality_tests[[covar]]$bottom$p.value) && 
                 normality_tests[[covar]]$top$p.value > 0.05 && normality_tests[[covar]]$bottom$p.value > 0.05
    test <- if (is_normal) {
      t.test(top_25[[covar]], bottom_25[[covar]])
    } else {
      wilcox.test(top_25[[covar]], bottom_25[[covar]])
    }
    list(test = test, is_normal = is_normal)
  })

  names(test_results) <- numeric_covariates

  # Combine the results into a formatted table
  summary_table <- data.frame(
    Covariate = numeric_covariates,
    Top_25 = round(unlist(top_25_summary), 2),
    Bottom_25 = round(unlist(bottom_25_summary), 2),
    Difference = round(unlist(top_25_summary) - unlist(bottom_25_summary), 2),
    p_value = sapply(test_results, function(res) res$test$p.value)
  )

  summary_table$Significance <- calc_significance_stars(summary_table$p_value)
  summary_table$Difference <- paste0(summary_table$Difference, summary_table$Significance)
  summary_table <- summary_table %>% select(-p_value, -Significance)

  summary_table %>% arrange(desc(abs(as.numeric(gsub("[^0-9.-]", "", Difference)))))
}

# Process each file and combine results
comparison_results_list <- lapply(files, process_file)

# Combine the results side by side
combined_results <- merge(comparison_results_list[[1]], comparison_results_list[[2]], by = "Covariate", suffixes = c("_novelty", "_usefulness"))

csv_data <- combined_results
# Define the output CSV file path
output_csv_file <- "../results/h3.csv"

# Save the extracted data to a CSV file
write.csv(csv_data, file = output_csv_file, row.names = FALSE)

# Print a message indicating the file has been saved
cat("CSV file has been saved to", output_csv_file, "\n")

# # Filter to include only covariates with at least one significant difference
# filtered_results <- combined_results %>%
#   filter(grepl("[*]", Difference_novelty) | grepl("[*]", Difference_usefulness))

# # Sort by the sum of the magnitudes of the differences
# filtered_results <- filtered_results %>%
#   mutate(Sum_Magnitudes = abs(as.numeric(gsub("[^0-9.-]", "", Difference_novelty))) + abs(as.numeric(gsub("[^0-9.-]", "", Difference_usefulness)))) %>%
#   arrange(desc(Sum_Magnitudes)) %>%
#   select(-Sum_Magnitudes)

# # Prepare the table for output
# table_data <- filtered_results %>%
#   select(Covariate,
#          Top_25_novelty, Bottom_25_novelty, Difference_novelty,
#          Top_25_usefulness, Bottom_25_usefulness, Difference_usefulness) %>%
#   distinct()

# # Generate HTML output
# html_table <- kable(table_data, format = "html", table.attr = "class='table table-striped'", escape = FALSE, row.names = FALSE, col.names = c(
#   "Covariate", "Top 25%", "Bottom 25%", "Difference", "Top 25%", "Bottom 25%", "Difference"
# )) %>%
#   kable_styling(full_width = F) %>%
#   add_header_above(c(" " = 1, "Novelty" = 3, "Usefulness" = 3))

# # Print the HTML table
# print(html_table)