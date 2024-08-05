# Function to compare the coefficients and their significance levels
compare_coefficients <- function(coxph_model, ols_model) {
  coxph_summary <- summary(coxph_model)
  ols_summary <- summary(ols_model)
  
  coxph_coefs <- coef(coxph_model)
  ols_coefs <- coef(ols_model)
  
  coxph_pvalues <- coxph_summary$coefficients[, "Pr(>|z|)"]
  ols_pvalues <- ols_summary$coefficients[, "Pr(>|t|)"]
  
  # Align the coefficients based on the predictor variables
  common_vars <- intersect(names(coxph_coefs), names(ols_coefs))
  
  coxph_coefs <- coxph_coefs[common_vars]
  ols_coefs <- ols_coefs[common_vars]
  coxph_pvalues <- coxph_pvalues[common_vars]
  ols_pvalues <- ols_pvalues[common_vars]
  
  # Compare the signs of the coefficients
  comparison <- sign(coxph_coefs) == sign(ols_coefs)
  
  result <- data.frame(
    Variable = common_vars,
    CoxPH_Coefficient = round(coxph_coefs, 3),
    CoxPH_PValue = coxph_pvalues,
    CoxPH_Significance = sapply(coxph_pvalues, calc_significance_stars),
    OLS_Coefficient = round(ols_coefs, 3),
    OLS_PValue = ols_pvalues,
    OLS_Significance = sapply(ols_pvalues, calc_significance_stars),
    Same_Direction = comparison,
    Magnitude_Sum = abs(coxph_coefs) + abs(ols_coefs)
  )
  
  return(result)
}