# Load necessary libraries
library(kableExtra)
library(knitr)

# Function to set the working directory dynamically
set_working_directory <- function() {
  # This only works if the script is run using source()
  current_path <- dirname(normalizePath(sys.frame(1)$ofile))
  setwd(current_path)
}

# Call the function to set the working directory dynamically
set_working_directory()

# Source the required scripts
source("load_data.R")
source("ols.R")
source("coxph.R")

# Load the data
file <- "../eventnet_data/bgg_two_mode_novelty_COND_SIZE_DHE.csv"
a <- prepare_data(file, min_eventsize = 2, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE)
data <- a$bgg_events[a$ivs]

# Calculate the correlation matrix
correlations <- cor(data, use = "complete.obs")

# Round the correlation values to 2 decimal places
correlations <- round(correlations, 2)

# Extract the upper triangle of the correlation matrix
upper_tri <- correlations
upper_tri[lower.tri(upper_tri, diag = TRUE)] <- NA

# Convert to a data frame and replace NA values with an empty string
upper_tri_df <- as.data.frame(upper_tri)
upper_tri_df[is.na(upper_tri_df)] <- ""

# Add row numbers
row_numbers <- 1:nrow(upper_tri_df)
upper_tri_df <- cbind(row_numbers, upper_tri_df)
colnames(upper_tri_df)[1] <- ""

# Repeat row numbers for columns
colnames(upper_tri_df)[-1] <- row_numbers

# Create and save the LaTeX table using kableExtra
kable(upper_tri_df, format = "latex", booktabs = TRUE, caption = "Correlation Matrix (Upper Triangle)", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  save_kable("../results/correlation_matrix.tex")

print(upper_tri_df)