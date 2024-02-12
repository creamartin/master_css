## uncomment if packages have to be installed
# install.packages("survival")
# install.packages("texreg")

## package for survial analysis
library(survival)

## package for nice formatting of parameter tables
library(texreg)

# adapt
setwd(".")

fit_ols <- function(file_path) {
  bgg.events <- read.csv(file_path)
  bgg.events <- bgg.events[bgg.events$event.size > 1,]
  bgg.events <- bgg.events[bgg.events$IS_OBSERVED == 1,]
  # all explanatory variables are zero in this data snippet since it has only one single point in time (2007).
  # Thus, there will be no findings on this snippet - but the code should work also for larger data.

  bgg.events$prior.success.exact <- 0
  bgg.events[bgg.events$exact.repetition > 0, "prior.success.exact"] <-
    (bgg.events[bgg.events$exact.repetition > 0, "cum.performance.exact"] /
      bgg.events[bgg.events$exact.repetition > 0, "exact.repetition"])

  bgg.events$prior.success.1 <- 0
  bgg.events[bgg.events$sub.rep.cum.1 > 0, "prior.success.1"] <-
    (bgg.events[bgg.events$sub.rep.cum.1 > 0, "cum.performance.1"] /
      bgg.events[bgg.events$sub.rep.cum.1 > 0, "sub.rep.cum.1"])

  bgg.events$prior.success.2 <- 0
  bgg.events[bgg.events$sub.rep.cum.2 > 0, "prior.success.2"] <-
    (bgg.events[bgg.events$sub.rep.cum.2 > 0, "cum.performance.2"] /
      bgg.events[bgg.events$sub.rep.cum.2 > 0, "sub.rep.cum.2"])

  bgg.events$prior.success.3 <- 0
  bgg.events[bgg.events$sub.rep.cum.3 > 0, "prior.success.3"] <-
    (bgg.events[bgg.events$sub.rep.cum.3 > 0, "cum.performance.3"] /
      bgg.events[bgg.events$sub.rep.cum.3 > 0, "sub.rep.cum.3"])

  bgg.events$prior.success.4 <- 0
  bgg.events[bgg.events$sub.rep.cum.4 > 0, "prior.success.4"] <-
    (bgg.events[bgg.events$sub.rep.cum.4 > 0, "cum.performance.4"] /
      bgg.events[bgg.events$sub.rep.cum.4 > 0, "sub.rep.cum.4"])

  bgg.events$prior.success.5 <- 0
  bgg.events[bgg.events$sub.rep.cum.5 > 0, "prior.success.5"] <-
    (bgg.events[bgg.events$sub.rep.cum.5 > 0, "cum.performance.5"] /
      bgg.events[bgg.events$sub.rep.cum.5 > 0, "sub.rep.cum.5"])

  bgg.events$prior.success.6 <- 0
  bgg.events[bgg.events$sub.rep.cum.6 > 0, "prior.success.6"] <-
    (bgg.events[bgg.events$sub.rep.cum.6 > 0, "cum.performance.6"] /
      bgg.events[bgg.events$sub.rep.cum.6 > 0, "sub.rep.cum.6"])

  bgg.events$prior.success.7 <- 0
  bgg.events[bgg.events$sub.rep.cum.7 > 0, "prior.success.7"] <-
    (bgg.events[bgg.events$sub.rep.cum.7 > 0, "cum.performance.7"] /
      bgg.events[bgg.events$sub.rep.cum.7 > 0, "sub.rep.cum.7"])
  summary(bgg.events)

  bgg.events$prior.success.8 <- 0
  bgg.events[bgg.events$sub.rep.cum.8 > 0, "prior.success.8"] <-
    (bgg.events[bgg.events$sub.rep.cum.8 > 0, "cum.performance.8"] /
      bgg.events[bgg.events$sub.rep.cum.8 > 0, "sub.rep.cum.8"])

  bgg.events$prior.success.9 <- 0
  bgg.events[bgg.events$sub.rep.cum.9 > 0, "prior.success.9"] <-
    (bgg.events[bgg.events$sub.rep.cum.9 > 0, "cum.performance.9"] /
      bgg.events[bgg.events$sub.rep.cum.9 > 0, "sub.rep.cum.9"])

  # do some rescaling - if you want to do that; alternative: scale(), log1p(), ...
  bgg.events[, c(12:ncol(bgg.events))] <- sqrt(bgg.events[, c(12:ncol(bgg.events))])

  return(lm(WEIGHT ~
      # exogenous
      + event.size
      + avg.gender
      + ratio.gender
      + avg.tenure
      + ratio.tenure
      # endogenous rhem
      + closure
      + sub.rep.1
      + sub.rep.2
      + sub.rep.3
      + sub.rep.4
      + sub.rep.5
      + sub.rep.6
      + sub.rep.7
      + sub.rep.8
      + sub.rep.9
      + exact.repetition
      # endogenous rhom
      + prior.success.exact
      + sdev.raw.performance.order.1
      + prior.success.1
      + prior.success.2
      + prior.success.3
      + prior.success.4
      + prior.success.5
      + prior.success.6
      + prior.success.7
      + prior.success.8
      + prior.success.9, data = bgg.events))
}

fit_ols_rhem <- function(file_path) {
  bgg.events <- read.csv(file_path)
  # all explanatory variables are zero in this data snippet since it has only one single point in time (2007).
  # Thus, there will be no findings on this snippet - but the code should work also for larger data.

  bgg.events$prior.success.exact <- 0
  bgg.events[bgg.events$exact.repetition > 0, "prior.success.exact"] <-
    (bgg.events[bgg.events$exact.repetition > 0, "cum.performance.exact"] /
      bgg.events[bgg.events$exact.repetition > 0, "exact.repetition"])

  bgg.events$prior.success.1 <- 0
  bgg.events[bgg.events$sub.rep.cum.1 > 0, "prior.success.1"] <-
    (bgg.events[bgg.events$sub.rep.cum.1 > 0, "cum.performance.1"] /
      bgg.events[bgg.events$sub.rep.cum.1 > 0, "sub.rep.cum.1"])

  bgg.events$prior.success.2 <- 0
  bgg.events[bgg.events$sub.rep.cum.2 > 0, "prior.success.2"] <-
    (bgg.events[bgg.events$sub.rep.cum.2 > 0, "cum.performance.2"] /
      bgg.events[bgg.events$sub.rep.cum.2 > 0, "sub.rep.cum.2"])

  bgg.events$prior.success.3 <- 0
  bgg.events[bgg.events$sub.rep.cum.3 > 0, "prior.success.3"] <-
    (bgg.events[bgg.events$sub.rep.cum.3 > 0, "cum.performance.3"] /
      bgg.events[bgg.events$sub.rep.cum.3 > 0, "sub.rep.cum.3"])

  bgg.events$prior.success.4 <- 0
  bgg.events[bgg.events$sub.rep.cum.4 > 0, "prior.success.4"] <-
    (bgg.events[bgg.events$sub.rep.cum.4 > 0, "cum.performance.4"] /
      bgg.events[bgg.events$sub.rep.cum.4 > 0, "sub.rep.cum.4"])

  bgg.events$prior.success.5 <- 0
  bgg.events[bgg.events$sub.rep.cum.5 > 0, "prior.success.5"] <-
    (bgg.events[bgg.events$sub.rep.cum.5 > 0, "cum.performance.5"] /
      bgg.events[bgg.events$sub.rep.cum.5 > 0, "sub.rep.cum.5"])

  bgg.events$prior.success.6 <- 0
  bgg.events[bgg.events$sub.rep.cum.6 > 0, "prior.success.6"] <-
    (bgg.events[bgg.events$sub.rep.cum.6 > 0, "cum.performance.6"] /
      bgg.events[bgg.events$sub.rep.cum.6 > 0, "sub.rep.cum.6"])

  bgg.events$prior.success.7 <- 0
  bgg.events[bgg.events$sub.rep.cum.7 > 0, "prior.success.7"] <-
    (bgg.events[bgg.events$sub.rep.cum.7 > 0, "cum.performance.7"] /
      bgg.events[bgg.events$sub.rep.cum.7 > 0, "sub.rep.cum.7"])
  summary(bgg.events)

  bgg.events$prior.success.8 <- 0
  bgg.events[bgg.events$sub.rep.cum.8 > 0, "prior.success.8"] <-
    (bgg.events[bgg.events$sub.rep.cum.8 > 0, "cum.performance.8"] /
      bgg.events[bgg.events$sub.rep.cum.8 > 0, "sub.rep.cum.8"])

  bgg.events$prior.success.9 <- 0
  bgg.events[bgg.events$sub.rep.cum.9 > 0, "prior.success.9"] <-
    (bgg.events[bgg.events$sub.rep.cum.9 > 0, "cum.performance.9"] /
      bgg.events[bgg.events$sub.rep.cum.9 > 0, "sub.rep.cum.9"])

  # do some rescaling - if you want to do that; alternative: scale(), log1p(), ...
  bgg.events[, c(12:ncol(bgg.events))] <- sqrt(bgg.events[, c(12:ncol(bgg.events))])

  return(lm(WEIGHT ~
      # exogenous
      + event.size
      + avg.gender
      + ratio.gender
      + avg.tenure
      + ratio.tenure
      # endogenous rhem
      + closure
      + sub.rep.1
      + sub.rep.2
      + sub.rep.3
      + sub.rep.4
      + sub.rep.5
      + sub.rep.6
      + sub.rep.7
      + sub.rep.8
      + sub.rep.9
      + exact.repetition,
       data = bgg.events))
}

model.rating.avg <- fit_ols("RHOM_output/bgg_avg_rating_RHOM.csv")
model.rating.bayes <- fit_ols("RHOM_output/bgg_bayes_rating_RHOM.csv")
model.novelty.avg <- fit_ols("RHOM_output/bgg_novelty_avg_RHOM.csv")
model.novelty.norm <- fit_ols("RHOM_output/bgg_novelty_norm_RHOM.csv")
#model.rhem <- fit_ols_rhem("RHOM_output/bgg_avg_rating_RHOM.csv")
#model.votes <- fit_ols("RHOM_output/bgg_num_votes_RHOM.csv")

models <- list(
  model.rating.avg,
  model.rating.bayes,
  model.novelty.avg,
  model.novelty.norm
  #model.rhem
  #model.votes,
)
names <- c(
  "model.rating.avg",
  "model.rating.bayes",
  "model.novelty.avg",
  "model.novelty.norm"
  #"model.rhem"
  #"model.votes"
)
print(
  screenreg(
    models,
    single.row = TRUE,
    digits = 2, stars = c(0.001, 0.01, 0.05, 0.1),
    custom.model.names = names,
    include.rsquared = FALSE,
    include.maxrs = FALSE,
    include.missings = FALSE,
    include.zph = FALSE
  )
)