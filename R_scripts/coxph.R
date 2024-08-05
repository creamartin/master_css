library(survival)
library(car)
library(stargazer)

run_coxph <- function(input, print_vifs = FALSE, fout = NULL) {
  data <- input$bgg_events
  ivs <- input$ivs

  data$stratum <- paste(data$TIME, data$team_size, data$mechanic_size, sep = ":")

  formula_str <- sprintf(
    "Surv(time = rep(0, dim(data)[1]), event = data$IS_OBSERVED) ~ %s + strata(stratum)",
    paste(ivs, collapse = " + ")
  )

  # Fit the Cox proportional hazards model
  cox_model <- coxph(as.formula(formula_str), data = data, robust = TRUE)

  if (print_vifs) {
    # Fit a model without the strata term for VIF calculation
    formula_str_vif <- sprintf(
      "Surv(time = rep(0, dim(data)[1]), event = data$IS_OBSERVED) ~ %s",
      paste(ivs, collapse = " + ")
    )
    cox_model_vif <- coxph(as.formula(formula_str_vif), data = data)

    # Calculate VIF
    vif_values <- vif(cox_model_vif)

    # Create a data frame for VIF values
    vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

    # Calculate correlation matrix
    corr_matrix <- cor(data[ivs], use = "complete.obs")

    # Identify the most correlated variable for each variable
    most_correlated_var <- apply(corr_matrix, 1, function(row) {
      max_corr_var <- names(which.max(row[-which.max(abs(row))]))
      max_corr <- max(row[-which.max(abs(row))])
      return(c(max_corr_var, max_corr))
    })

    most_correlated_df <- t(most_correlated_var)
    colnames(most_correlated_df) <- c("Most_Correlated_Var", "Correlation")
    most_correlated_df <- data.frame(most_correlated_df, stringsAsFactors = FALSE)

    # Merge VIF values with most correlated variable
    vif_df <- cbind(vif_df, most_correlated_df[rownames(vif_df), ])

    # Convert Correlation column to numeric
    vif_df$Correlation <- as.numeric(vif_df$Correlation)

    # Sort the table by VIF
    vif_df <- vif_df[order(-vif_df$VIF), ]

    # Print the table using stargazer
    if (is.null(fout)) {
      stargazer(vif_df, type = "text", summary = FALSE, digits = 2, align = TRUE, rownames = FALSE)
    } else {
      stargazer(vif_df, type = "latex", summary = FALSE, digits = 2, align = TRUE, rownames = FALSE, out = fout)
    }
  }

  return(cox_model)
}
