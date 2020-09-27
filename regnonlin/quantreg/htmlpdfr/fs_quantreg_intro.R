## ----global_options, include = FALSE---------------------------------------------------------------
try(source("../../.Rprofile"))
library('quantreg')


## --------------------------------------------------------------------------------------------------
fit_mean <- lm(mpg ~ disp + hp + factor(am) + factor(vs), data = mtcars)
summary(fit_mean)


## --------------------------------------------------------------------------------------------------
ls_fl_quantiles <- c(0.25, 0.50, 0.75)
fit_quantiles <- rq(mpg ~ disp + hp + factor(am),
               tau = ls_fl_quantiles,
               data = mtcars)
summary(fit_quantiles, se = "boot")


## --------------------------------------------------------------------------------------------------
ls_fl_quantiles <- c(0.25, 0.50)
fit_quantiles <- rq(mpg ~ disp + hp + factor(am),
               tau = ls_fl_quantiles,
               data = mtcars)
anova(fit_quantiles, test = "Wald", joint=TRUE)


## --------------------------------------------------------------------------------------------------
anova(fit_quantiles, test = "Wald", joint=FALSE)


## --------------------------------------------------------------------------------------------------
ls_fl_quantiles <- c(0.25, 0.50, 0.75)
fit_quantiles <- rq(mpg ~ disp + hp + factor(am),
               tau = ls_fl_quantiles,
               data = mtcars)
anova(fit_quantiles, test = "Wald", joint=TRUE)


## --------------------------------------------------------------------------------------------------
anova(fit_quantiles, test = "Wald", joint=FALSE)

