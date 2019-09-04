# Convert one by one
f_frac_asymp <- function(x, sca.total.frac=1) {
    sca.subsidy.frac <- sca.total.frac - sca.total.frac/(1+exp(x))
    return(sca.subsidy.frac)
}
# Vector of Fractions from Asymptotic Transform
f_frac_asymp_vec <- function(x) {
    # inputs: x <- c(1, 1, 1), these are the x_hats, the unconstrained transformed
    # outputs: the return output of the function are the original fractionos

    sca.x.counter <- 1
    sca.total.frac <- 1
    vec.subsidy.frac <- numeric(length(x)+1)
    while(sca.x.counter <= length(x)) {
        sca.x.cur <- x[sca.x.counter]
        sca.subsidy.frac <- f_frac_asymp(sca.x.cur, sca.total.frac)
        vec.subsidy.frac[sca.x.counter] <- sca.subsidy.frac
        sca.total.frac <- sca.total.frac - sca.subsidy.frac
        sca.x.counter <- sca.x.counter + 1
    }

    vec.subsidy.frac[length(x)+1] <- sca.total.frac

    return(vec.subsidy.frac)
}

# Fractional Estimands Initializer Function
f_subsidy_frac <- function(n, type, seed=123) {

    if (type == 'rand') {
        set.seed(seed)
        vec.draws <- runif(n)
        vec.prob <- vec.draws/sum(vec.draws)
    }

    if (type == 'unif') {
        vec.prob <- numeric(n) + 1/n
    }

    if (type == 'dexp') {
        bin <- 1/(n)
        vec.quantiles <- seq(bin/2, 1, bin)
        vec.points <- qexp(vec.quantiles)
        vec.prob <- vec.points/sum(vec.points)
        vec.prob <- sort(vec.prob)
    }

    if (type == 'dnorm') {
        bin <- 1/(n)
        vec.quantiles <- seq(bin/2, 1, bin)
        vec.points <- qnorm(vec.quantiles)
        vec.prob <- dnorm(vec.points)/sum(dnorm(vec.points))
    }

    return((vec.prob))
}
# Different Probability Strucgture
cbind(tibble(type='rand', frac=f_subsidy_frac(5, 'rand')),
      tibble(type='unif', frac=f_subsidy_frac(5, 'unif')),
      tibble(type='dexp', frac=f_subsidy_frac(5, 'dexp')),
      tibble(type='dnorm', frac=f_subsidy_frac(5, 'dnorm')))
