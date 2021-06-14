## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# Random normal Data Vector (not equal outcomes)
set.seed(123)
it_sample_N <- 10000
fl_rnorm_mean <- 100
fl_rnorm_sd <- 6
ar_data_rnorm <- rnorm(it_sample_N, mean=fl_rnorm_mean, sd=fl_rnorm_sd)
# Visualize
par(new=FALSE)
hist(ar_data_rnorm, xlab = 'x Values', ylab = 'Frequencies', main = '')
title(main = 'Continuous Normal Random Variable Draws')
grid()


## ------------------------------------------------------------------------------------------------------
# Use binomial to approximate normal
fl_p_binom <- 1 - fl_rnorm_sd^2/fl_rnorm_mean
fl_n_binom <- round(fl_rnorm_mean^2/(fl_rnorm_mean - fl_rnorm_sd^2))
fl_binom_mean <- fl_n_binom*fl_p_binom
fl_binom_sd <- sqrt(fl_n_binom*fl_p_binom*(1-fl_p_binom))
# Mean and sd
print(paste0('BINOMI mean=', fl_binom_mean))
print(paste0('BINOMI mean=', fl_binom_sd))
# drv = discrete random variable
ar_drv_rbinom_xval <- seq(1, fl_n_binom)
ar_drv_rbinom_prob <- dbinom(ar_drv_rbinom_xval, size=fl_n_binom, prob=fl_p_binom)
# ignore weight at x=0
ar_drv_rbinom_prob <- ar_drv_rbinom_prob/sum(ar_drv_rbinom_prob)


## ------------------------------------------------------------------------------------------------------
# graph
par(new=FALSE)
ar_ylim = c(0,1)
ffi_atkinson_ineq(ar_data_rnorm, -1)
ar_atkinson <- apply(mt_rho, 1, function(row){ffi_atkinson_ineq(ar_data_rnorm, row[1])})
plot(ar_drv_rbinom_xval, ar_drv_rbinom_prob, xlab = 'Binomial x Values', ylab = 'Discrete Probabilitys, P(x)')
title(main = paste0('Binomial Approximate of Normal Random Variable'),
      sub = paste0('binop=', round(fl_p_binom, 2), ';binon=', round(fl_n_binom, 2), ';binomean=', round(fl_binom_mean, 2), ';binomsd=', round(fl_binom_sd, 2), ';normmean=', round(fl_rnorm_mean, 2), ';normsd=', round(fl_rnorm_sd, 2)))
grid()

