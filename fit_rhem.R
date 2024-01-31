## uncomment if packages have to be installed
#install.packages("survival")
#install.packages("texreg")

## package for survial analysis
library(survival)

## package for nice formatting of parameter tables
library(texreg)

## set the working directory (adapt the directory path)
setwd("./conditional")

## read the events from the conditional-size RHEM
events.cond <- read.csv("bgg_min_EVENTS_CONDITIONAL_SIZE.csv")

## fit the RHEM 
model.cond.size <- coxph(Surv(time = rep(1,dim(events.cond)[1]), event = events.cond$IS_OBSERVED) ~ 
                    event.size
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
                    + sub.rep.10
                    + sub.rep.11
                    + sub.rep.12
                    + sub.rep.13
                    + sub.rep.14
                    + sub.rep.15
                    + sub.rep.16
                    + sub.rep.17
                    + sub.rep.18
                    + sub.rep.19
                    + sub.rep.20
                    + sub.rep.21
                    + sub.rep.22
                    + sub.rep.23
                    + sub.rep.24
                    + sub.rep.25
                    + sub.rep.26
                    + closure
                    + strata(TIME)
                    , data = events.cond)
#print(summary(model.cond.size))

model.unconstr <- model.cond.size
# display model parameters in one table
print(screenreg(list(model.cond.size), single.row = TRUE, 
          digits = 2, stars = c(0.001, 0.01,0.05,0.1),
          custom.model.names = c("conditional-size"),
          include.rsquared = FALSE, include.maxrs = FALSE, include.missings = FALSE,
          include.zph = FALSE))