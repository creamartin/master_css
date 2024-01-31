## uncomment if packages have to be installed
#install.packages("survival")
#install.packages("texreg")

## package for survial analysis
library(survival)

## package for nice formatting of parameter tables
library(texreg)

## set the working directory (adapt the directory path)
setwd("./RHOM_data")

bgg.events <- read.csv("bgg_min_RHOM_avg_rating.csv")
# all explanatory variables are zero in this data snippet since it has only one single point in time (2007).
# Thus, there will be no findings on this snippet - but the code should work also for larger data.
summary(bgg.events)

# the following three blocks compute average performance of sets of coauthors of size 1, 2, and 3
bgg.events$prior.success.1 <- 0

bgg.events[bgg.events$sub.rep.cum.1 > 0, "prior.success.1"] <-
  (bgg.events[bgg.events$sub.rep.cum.1 > 0, "cum.ratings.1"] /
     bgg.events[bgg.events$sub.rep.cum.1 > 0, "sub.rep.cum.1"])

bgg.events$prior.success.2 <- 0
bgg.events[bgg.events$sub.rep.cum.2 > 0, "prior.success.2"] <-
  (bgg.events[bgg.events$sub.rep.cum.2 > 0, "cum.ratings.2"] /
     bgg.events[bgg.events$sub.rep.cum.2 > 0, "sub.rep.cum.2"])

bgg.events$prior.success.3 <- 0
bgg.events[bgg.events$sub.rep.cum.3 > 0, "prior.success.3"] <-
  (bgg.events[bgg.events$sub.rep.cum.3 > 0, "cum.ratings.3"] /
     bgg.events[bgg.events$sub.rep.cum.3 > 0, "sub.rep.cum.3"])

bgg.events$prior.success.4 <- 0
bgg.events[bgg.events$sub.rep.cum.4 > 0, "prior.success.4"] <-
  (bgg.events[bgg.events$sub.rep.cum.4 > 0, "cum.ratings.4"] /
     bgg.events[bgg.events$sub.rep.cum.4 > 0, "sub.rep.cum.4"])

bgg.events$prior.success.5 <- 0
bgg.events[bgg.events$sub.rep.cum.5 > 0, "prior.success.5"] <-
  (bgg.events[bgg.events$sub.rep.cum.5 > 0, "cum.ratings.5"] /
     bgg.events[bgg.events$sub.rep.cum.5 > 0, "sub.rep.cum.5"])

bgg.events$prior.success.6 <- 0
bgg.events[bgg.events$sub.rep.cum.6 > 0, "prior.success.6"] <-
  (bgg.events[bgg.events$sub.rep.cum.6 > 0, "cum.ratings.6"] /
     bgg.events[bgg.events$sub.rep.cum.6 > 0, "sub.rep.cum.6"])

bgg.events$prior.success.7 <- 0
bgg.events[bgg.events$sub.rep.cum.7 > 0, "prior.success.7"] <-
  (bgg.events[bgg.events$sub.rep.cum.7 > 0, "cum.ratings.7"] /
     bgg.events[bgg.events$sub.rep.cum.7 > 0, "sub.rep.cum.7"])
summary(bgg.events)

bgg.events$prior.success.8 <- 0
bgg.events[bgg.events$sub.rep.cum.8 > 0, "prior.success.8"] <-
  (bgg.events[bgg.events$sub.rep.cum.8 > 0, "cum.ratings.8"] /
     bgg.events[bgg.events$sub.rep.cum.8 > 0, "sub.rep.cum.8"])

bgg.events$prior.success.9 <- 0
bgg.events[bgg.events$sub.rep.cum.9 > 0, "prior.success.9"] <-
  (bgg.events[bgg.events$sub.rep.cum.9 > 0, "cum.ratings.9"] /
     bgg.events[bgg.events$sub.rep.cum.9 > 0, "sub.rep.cum.9"])

# Fit the model
model.avg.rating <- lm(WEIGHT ~     + event.size
    + exact.repetition
    + sub.rep.1
    + sub.rep.2
    + sub.rep.3
    + sub.rep.4
    + sub.rep.5
    + sub.rep.6
    + sub.rep.7
    + sub.rep.8
    + sub.rep.9
    + closure
    + prior.success.1
    + prior.success.2
    + prior.success.3
    + prior.success.4
    + prior.success.5
    + prior.success.6
    + prior.success.7
    + prior.success.8
    + prior.success.9, data = bgg.events)

#############################################################

bgg.events <- read.csv("bgg_min_RHOM_bayes_rating.csv")
# all explanatory variables are zero in this data snippet since it has only one single point in time (2007).
# Thus, there will be no findings on this snippet - but the code should work also for larger data.
summary(bgg.events)

# the following three blocks compute average performance of sets of coauthors of size 1, 2, and 3
bgg.events$prior.success.1 <- 0

bgg.events[bgg.events$sub.rep.cum.1 > 0, "prior.success.1"] <-
  (bgg.events[bgg.events$sub.rep.cum.1 > 0, "cum.ratings.1"] /
     bgg.events[bgg.events$sub.rep.cum.1 > 0, "sub.rep.cum.1"])

bgg.events$prior.success.2 <- 0
bgg.events[bgg.events$sub.rep.cum.2 > 0, "prior.success.2"] <-
  (bgg.events[bgg.events$sub.rep.cum.2 > 0, "cum.ratings.2"] /
     bgg.events[bgg.events$sub.rep.cum.2 > 0, "sub.rep.cum.2"])

bgg.events$prior.success.3 <- 0
bgg.events[bgg.events$sub.rep.cum.3 > 0, "prior.success.3"] <-
  (bgg.events[bgg.events$sub.rep.cum.3 > 0, "cum.ratings.3"] /
     bgg.events[bgg.events$sub.rep.cum.3 > 0, "sub.rep.cum.3"])

bgg.events$prior.success.4 <- 0
bgg.events[bgg.events$sub.rep.cum.4 > 0, "prior.success.4"] <-
  (bgg.events[bgg.events$sub.rep.cum.4 > 0, "cum.ratings.4"] /
     bgg.events[bgg.events$sub.rep.cum.4 > 0, "sub.rep.cum.4"])

bgg.events$prior.success.5 <- 0
bgg.events[bgg.events$sub.rep.cum.5 > 0, "prior.success.5"] <-
  (bgg.events[bgg.events$sub.rep.cum.5 > 0, "cum.ratings.5"] /
     bgg.events[bgg.events$sub.rep.cum.5 > 0, "sub.rep.cum.5"])

bgg.events$prior.success.6 <- 0
bgg.events[bgg.events$sub.rep.cum.6 > 0, "prior.success.6"] <-
  (bgg.events[bgg.events$sub.rep.cum.6 > 0, "cum.ratings.6"] /
     bgg.events[bgg.events$sub.rep.cum.6 > 0, "sub.rep.cum.6"])

bgg.events$prior.success.7 <- 0
bgg.events[bgg.events$sub.rep.cum.7 > 0, "prior.success.7"] <-
  (bgg.events[bgg.events$sub.rep.cum.7 > 0, "cum.ratings.7"] /
     bgg.events[bgg.events$sub.rep.cum.7 > 0, "sub.rep.cum.7"])
summary(bgg.events)

bgg.events$prior.success.8 <- 0
bgg.events[bgg.events$sub.rep.cum.8 > 0, "prior.success.8"] <-
  (bgg.events[bgg.events$sub.rep.cum.8 > 0, "cum.ratings.8"] /
     bgg.events[bgg.events$sub.rep.cum.8 > 0, "sub.rep.cum.8"])

bgg.events$prior.success.9 <- 0
bgg.events[bgg.events$sub.rep.cum.9 > 0, "prior.success.9"] <-
  (bgg.events[bgg.events$sub.rep.cum.9 > 0, "cum.ratings.9"] /
     bgg.events[bgg.events$sub.rep.cum.9 > 0, "sub.rep.cum.9"])

# Fit the model
model.bayes.rating <- lm(WEIGHT ~     + event.size
    + exact.repetition
    + sub.rep.1
    + sub.rep.2
    + sub.rep.3
    + sub.rep.4
    + sub.rep.5
    + sub.rep.6
    + sub.rep.7
    + sub.rep.8
    + sub.rep.9
    + closure
    + prior.success.1
    + prior.success.2
    + prior.success.3
    + prior.success.4
    + prior.success.5
    + prior.success.6
    + prior.success.7
    + prior.success.8
    + prior.success.9, data = bgg.events)

#############################################################

bgg.events <- read.csv("bgg_min_RHOM_novelty_avg.csv")
# all explanatory variables are zero in this data snippet since it has only one single point in time (2007).
# Thus, there will be no findings on this snippet - but the code should work also for larger data.
summary(bgg.events)

# the following three blocks compute average performance of sets of coauthors of size 1, 2, and 3
bgg.events$prior.success.1 <- 0

bgg.events[bgg.events$sub.rep.cum.1 > 0, "prior.success.1"] <-
  (bgg.events[bgg.events$sub.rep.cum.1 > 0, "cum.ratings.1"] /
     bgg.events[bgg.events$sub.rep.cum.1 > 0, "sub.rep.cum.1"])

bgg.events$prior.success.2 <- 0
bgg.events[bgg.events$sub.rep.cum.2 > 0, "prior.success.2"] <-
  (bgg.events[bgg.events$sub.rep.cum.2 > 0, "cum.ratings.2"] /
     bgg.events[bgg.events$sub.rep.cum.2 > 0, "sub.rep.cum.2"])

bgg.events$prior.success.3 <- 0
bgg.events[bgg.events$sub.rep.cum.3 > 0, "prior.success.3"] <-
  (bgg.events[bgg.events$sub.rep.cum.3 > 0, "cum.ratings.3"] /
     bgg.events[bgg.events$sub.rep.cum.3 > 0, "sub.rep.cum.3"])

bgg.events$prior.success.4 <- 0
bgg.events[bgg.events$sub.rep.cum.4 > 0, "prior.success.4"] <-
  (bgg.events[bgg.events$sub.rep.cum.4 > 0, "cum.ratings.4"] /
     bgg.events[bgg.events$sub.rep.cum.4 > 0, "sub.rep.cum.4"])

bgg.events$prior.success.5 <- 0
bgg.events[bgg.events$sub.rep.cum.5 > 0, "prior.success.5"] <-
  (bgg.events[bgg.events$sub.rep.cum.5 > 0, "cum.ratings.5"] /
     bgg.events[bgg.events$sub.rep.cum.5 > 0, "sub.rep.cum.5"])

bgg.events$prior.success.6 <- 0
bgg.events[bgg.events$sub.rep.cum.6 > 0, "prior.success.6"] <-
  (bgg.events[bgg.events$sub.rep.cum.6 > 0, "cum.ratings.6"] /
     bgg.events[bgg.events$sub.rep.cum.6 > 0, "sub.rep.cum.6"])

bgg.events$prior.success.7 <- 0
bgg.events[bgg.events$sub.rep.cum.7 > 0, "prior.success.7"] <-
  (bgg.events[bgg.events$sub.rep.cum.7 > 0, "cum.ratings.7"] /
     bgg.events[bgg.events$sub.rep.cum.7 > 0, "sub.rep.cum.7"])
summary(bgg.events)

bgg.events$prior.success.8 <- 0
bgg.events[bgg.events$sub.rep.cum.8 > 0, "prior.success.8"] <-
  (bgg.events[bgg.events$sub.rep.cum.8 > 0, "cum.ratings.8"] /
     bgg.events[bgg.events$sub.rep.cum.8 > 0, "sub.rep.cum.8"])

bgg.events$prior.success.9 <- 0
bgg.events[bgg.events$sub.rep.cum.9 > 0, "prior.success.9"] <-
  (bgg.events[bgg.events$sub.rep.cum.9 > 0, "cum.ratings.9"] /
     bgg.events[bgg.events$sub.rep.cum.9 > 0, "sub.rep.cum.9"])

# Fit the model
model.novelty.avg <- lm(WEIGHT ~     + event.size
    + exact.repetition
    + sub.rep.1
    + sub.rep.2
    + sub.rep.3
    + sub.rep.4
    + sub.rep.5
    + sub.rep.6
    + sub.rep.7
    + sub.rep.8
    + sub.rep.9
    + closure
    + prior.success.1
    + prior.success.2
    + prior.success.3
    + prior.success.4
    + prior.success.5
    + prior.success.6
    + prior.success.7
    + prior.success.8
    + prior.success.9, data = bgg.events)

#############################################################



print(screenreg(list(model.avg.rating,model.bayes.rating,model.novelty.avg), single.row = TRUE, 
          digits = 2, stars = c(0.001, 0.01,0.05,0.1),
          custom.model.names = c("success:avg-rating","success:bayes-rating","success:novelty"),
          include.rsquared = FALSE, include.maxrs = FALSE, include.missings = FALSE,
          include.zph = FALSE))
