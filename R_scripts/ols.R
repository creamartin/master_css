library(car)
library(stargazer)

run_ols <- function(input, controls = NULL, print_vifs = FALSE, fout = NULL) {
  data <- input$bgg_events
  ivs <- input$ivs

  # Combine ivs and controls into one list of predictors
  predictors <- c(ivs, controls)

  # Create the formula string
  formula_str <- paste("WEIGHT ~", paste(predictors, collapse = " + "))

  # Create the formula
  formula <- as.formula(formula_str)

  # Run the OLS regression
  ols_model <- lm(formula, data = data)

  if (print_vifs) {
    # Calculate VIF
    vif_values <- vif(ols_model)

    # Create a data frame for VIF values
    vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values, row.names = NULL)

    # Calculate correlation matrix
    corr_matrix <- cor(data[predictors], use = "complete.obs")

    # Identify the most correlated variable for each variable
    most_correlated_var <- apply(corr_matrix, 1, function(row) {
      max_corr_index <- which.max(abs(row[-which.max(abs(row))]))
      max_corr_var <- names(row)[max_corr_index]
      max_corr <- row[max_corr_index]
      return(c(max_corr_var, max_corr))
    })

    most_correlated_df <- as.data.frame(t(most_correlated_var))
    colnames(most_correlated_df) <- c("Most_Correlated_Var", "Correlation")
    most_correlated_df <- data.frame(Variable = rownames(most_correlated_df), most_correlated_df, stringsAsFactors = FALSE)
    most_correlated_df$Correlation <- as.numeric(most_correlated_df$Correlation)

    # Merge VIF values with most correlated variable
    vif_df <- merge(vif_df, most_correlated_df, by = "Variable")

    # Sort the table by VIF
    vif_df <- vif_df[order(-vif_df$VIF), ]

    # Print the table using stargazer
    if (is.null(fout)) {
      stargazer(vif_df, type = "text", summary = FALSE, digits = 2, align = TRUE, rownames = FALSE)
    } else {
      stargazer(vif_df, type = "latex", summary = FALSE, digits = 2, align = TRUE, rownames = FALSE, out = fout)
    }
  } else {
    return(ols_model)
  }
}