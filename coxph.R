library(survival)
library(texreg)

# adapt
setwd(".")

run_coxph <- function(file_path, use_rhom = TRUE) {
  bgg.events <- read.csv(file_path)
  num_variables = 16
  exogenous_variables <- c(
    "event.size",
    "avg.gender",
    "ratio.gender",
    "avg.tenure",
    "ratio.tenure"
  )
  rhem_variables <- c(
    "closure",
    "exact.repetition"
  )
  for (i in 1:num_variables) {
    rhem_variables <- c(rhem_variables, paste0("sub.rep.", i))
  }
  if (use_rhom) {
    # Calculate prior.success.exact
    bgg.events$prior.success.exact <- ifelse(bgg.events$exact.repetition > 0,
      bgg.events$cum.performance.exact / bgg.events$exact.repetition,
      0
    )
    # Calculate prior.success for sub.rep.cum columns from 1 to 9
    for (i in 1:num_variables) {
      prior_col <- paste0("prior.success.", i)
      cum_col <- paste0("cum.performance.", i)
      sub_rep_col <- paste0("sub.rep.cum.", i)

      bgg.events[[prior_col]] <- ifelse(bgg.events[[sub_rep_col]] > 0,
        bgg.events[[cum_col]] / bgg.events[[sub_rep_col]],
        0
      )
    }
    rhom_variables <- c(
      "prior.success.exact",
      "sdev.raw.performance.order.1"
    )    
    for (i in 1:num_variables) {
      rhom_variables <- c(rhom_variables, paste0("prior.success.", i))
    }
    ivs <- c(exogenous_variables, rhem_variables, rhom_variables)
  } else {
    ivs <- c(exogenous_variables, rhem_variables)
  }


  formula.str <- sprintf(
    "Surv(time = rep(0, dim(bgg.events)[1]), event = bgg.events$IS_OBSERVED) ~ %s + strata(TIME)",
    paste(ivs, collapse = " + ")
  )
  return(coxph(as.formula(formula.str), data = bgg.events))
}

model.rating.avg <- run_coxph("RHOM_output/bgg_avg_rating_RHOM.csv")
model.rating.bayes <- run_coxph("RHOM_output/bgg_bayes_rating_RHOM.csv")
model.novelty.avg <- run_coxph("RHOM_output/bgg_novelty_avg_RHOM.csv")
model.novelty.norm <- run_coxph("RHOM_output/bgg_novelty_norm_RHOM.csv")
model.rhem <- run_coxph("RHOM_output/bgg_avg_rating_RHOM.csv", use_rhom = FALSE)
# model.votes <- run_coxph("RHOM_output/bgg_num_votes_RHOM.csv")

models <- list(
  model.rating.avg,
  model.rating.bayes,
  model.novelty.avg,
  model.novelty.norm,
  model.rhem
  # model.votes,
)
names <- c(
  "model.rating.avg",
  "model.rating.bayes",
  "model.novelty.avg",
  "model.novelty.norm",
  "model.rhem"
  # "model.votes"
)
print(
  screenreg(
    file = "coxph.csv",
    models,
    single.row = TRUE,
    digits = 2, stars = c(0.001, 0.01, 0.05, 0.1),
    custom.model.names = names,
    include.rsquared = FALSE,
    include.maxrs = FALSE,
    include.missings = TRUE,
    include.zph = FALSE
  )
)

