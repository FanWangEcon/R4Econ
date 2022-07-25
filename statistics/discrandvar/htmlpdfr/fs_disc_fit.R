## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------
# Construct transition matrix (row sums to 1, 5 by 5)
mt_pi_kids_trans <- matrix(
    data = c(
        0.8929,    0.1045,    0.0026,    0.0000,    0.0000,
        0.0547,    0.6704,    0.2708,    0.0040,    0.0001,
        0.0014,    0.0571,    0.7931,    0.1462,    0.0022,
        0.0000,    0.0009,    0.0748,    0.7885,    0.1358,
        0.0000,    0.0000,    0.0008,    0.0593,    0.9399
    ),
    nrow = 5, ncol = 5,
    byrow = TRUE
)
# check row sum
rowSums(mt_pi_kids_trans)
# Construct the x vector, this is (5 by 1)
# we have x_0, to x_4, for the 5 columns
# x_0 = 0, and x_4 = 4
ar_x <- matrix(c(0, 1, 2, 3, 4), nrow = 5, ncol = 1)


## ----------------------------------------------------------------------------------------------------------------------------
# (5 by 5) times (5 by 1) generates (5 by 1)
# they correspond to 5 different \sum_{i=0}^N P(x_i)x_i
# Note divide by 4 not 5
ar_row_expected_x <- (mt_pi_kids_trans %*% ar_x) / (4)

# estimated thetas have been found
ar_theta_esti <- ar_row_expected_x
print(ar_theta_esti)


## ----------------------------------------------------------------------------------------------------------------------------
# Generate Binomial
mt_dbinom_approx <- t(sapply(
    ar_theta_esti, dbinom,
    x = seq(0, 4), size = 4
))
# Present
print(round(mt_dbinom_approx, 3))


## ----------------------------------------------------------------------------------------------------------------------------
#' Probability group aggregator
ffi_pbinom_group_theta_m <-
    function(fl_chance_success = 0.52,
             it_number_of_trials = 4,
             it_m = 8) {

        # it_n = (N+1)*M-1
        it_n <- it_number_of_trials
        # nbrtrl = number of trials
        it_nbrtrl <- (it_n + 1) * it_m - 1

        # Evaluate probability
        ar_dbinom <- dbinom(
            seq(0, it_nbrtrl),
            size = it_nbrtrl, prob = fl_chance_success
        )
        mt_dbinom <- matrix(
            ar_dbinom,
            nrow = it_n + 1, ncol = it_m,
            byrow = TRUE
        )

        # Sum probabilities each sub-groups
        ar_dbinom_grouped <- rowSums(mt_dbinom)

        return(ar_dbinom_grouped)
    }
# log-likelihood generator
# at the default value, the generated probabilities are similar to observed.
ffi_pbinom_lnlk_theta_m <-
    function(ar_drm_prob = c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022),
             fl_chance_success = 0.52,
             it_m = 8) {

        # it_n = (N+1)*M-1
        it_n <- length(ar_drm_prob) - 1

        # Call the grouping function
        ar_dbinom_grouped <- ffi_pbinom_group_theta_m(
            fl_chance_success = fl_chance_success,
            it_number_of_trials = it_n,
            it_m = it_m
        )

        # Log of group-probabilities
        ar_dbinom_grouped_ln <- log(ar_dbinom_grouped)

        # Log likelihood
        fl_ln_likelihood <- ar_dbinom_grouped_ln %*% ar_drm_prob
        return(fl_ln_likelihood)
    }


## ----------------------------------------------------------------------------------------------------------------------------
# Test the function with different theta values
ar_drm_prob <- c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022)
ar_fl_theta <- seq(0.1, 0.9, length.out = 9)
it_m <- 1
ar_ln_likelihood <- sapply(
    ar_fl_theta,
    ffi_pbinom_lnlk_theta_m,
    ar_drm_prob = ar_drm_prob,
    it_m = it_m
)
# Label and print
mt_theta_likelihood <- cbind(ar_fl_theta, ar_ln_likelihood)
colnames(mt_theta_likelihood) <- c("theta", "log_likelihood")
kable(as_tibble(mt_theta_likelihood) %>%
    mutate(rank = min_rank(desc(log_likelihood))), caption = paste(
    "Log likelihood evaluated at varying",
    "theta values, given some vector",
    "of observed probabilities, and",
    paste0("M=", it_m),
    separator = " "
)) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# Test the function with different theta values
ar_drm_prob <- c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022)
fl_theta <- 0.50
ar_it_m <- seq(1, 10)
ar_ln_likelihood <- sapply(
    ar_it_m,
    ffi_pbinom_lnlk_theta_m,
    ar_drm_prob = ar_drm_prob,
    fl_chance_success = fl_theta
)
# Label and print
mt_m_likelihood <- cbind(ar_it_m, ar_ln_likelihood)
colnames(mt_m_likelihood) <- c("M", "log_likelihood")
kable(as_tibble(mt_m_likelihood) %>%
    mutate(rank = min_rank(desc(log_likelihood))), caption = paste(
    "Log likelihood evaluated at varying",
    "M values, given some vector",
    "of observed probabilities, and",
    paste0("theta=", fl_theta),
    separator = " "
)) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# Evaluate likelihood along a grid and find the
# surrounding points around the maximizing grid point.
ffi_pbinom_lnlk_theta_m_grid <-
    function(ar_drm_prob = c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022),
             it_m = 8,
             it_p_success_grid_len = 10,
             fl_min_p_success = 1e-5,
             fl_max_p_success = 1 - 1e-5) {

        # Theta array rebuilt
        ar_fl_theta <- seq(
            fl_min_p_success,
            fl_max_p_success,
            length.out = it_p_success_grid_len
        )

        # Evaluate likelihood
        ar_ln_likelihood <- sapply(
            ar_fl_theta,
            ffi_pbinom_lnlk_theta_m,
            ar_drm_prob = ar_drm_prob,
            it_m = it_m
        )
        # Find min grid
        it_max_idx <- which.max(ar_ln_likelihood)
        fl_max_val <- ar_ln_likelihood[it_max_idx]

        # Find lower and upper bound
        fl_min_p_succcess_new <- ar_fl_theta[
            max(it_max_idx - 1, 1)
        ]
        fl_max_p_succcess_new <- ar_fl_theta[
            min(it_max_idx + 1, it_p_success_grid_len)
        ]

        # return
        return(list(
            fl_max_val = fl_max_val,
            fl_min_p_succcess_new = fl_min_p_succcess_new,
            fl_max_p_succcess_new = fl_max_p_succcess_new
        ))
    }
# test
ffi_pbinom_lnlk_theta_m_grid()


## ----------------------------------------------------------------------------------------------------------------------------
# Find likelihood maximizing theta, given M
ffi_pbinom_lnlk_estitheta_fixm <-
    function(ar_drm_prob = c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022),
             it_m = 1,
             verbose = FALSE) {

        # Initialize min and max and tolerance criteria
        fl_min_p_success_cur <- 1e-5
        fl_max_p_success_cur <- 1 - 1e-5
        it_p_success_grid_len <- 10
        fl_tol <- 1e-6
        it_max_iter <- 10

        # Initialize initial gaps etc
        fl_gap <- 1e5
        fl_min_ln_like_last <- 1e5
        it_iter <- 0

        # Iteratively loop over grid to find the maximum by zooming in
        while ((fl_gap > fl_tol) && it_iter <= it_max_iter) {

            # Iterator counts up
            it_iter <- it_iter + 1
            if (verbose) print(paste0("it_iter=", it_iter))

            # build array
            ls_find_max <- ffi_pbinom_lnlk_theta_m_grid(
                ar_drm_prob = ar_drm_prob,
                it_m = it_m,
                it_p_success_grid_len = it_p_success_grid_len,
                fl_min_p_success = fl_min_p_success_cur,
                fl_max_p_success = fl_max_p_success_cur
            )

            # Min objective value current
            fl_max_ln_like <- ls_find_max$fl_max_val
            # Find new lower and upper bound
            fl_min_p_success_cur <- ls_find_max$fl_min_p_succcess_new
            fl_max_p_success_cur <- ls_find_max$fl_max_p_succcess_new
            if (verbose) print(paste0("min_p_succ_cur=", fl_min_p_success_cur))
            if (verbose) print(paste0("max_p_succ_cur=", fl_max_p_success_cur))

            # Compare
            fl_gap <- abs(fl_max_ln_like - fl_min_ln_like_last)
            fl_max_ln_like_last <- fl_max_ln_like
            if (verbose) print(paste0("fl_gap=", fl_gap))
        }

        # Find estimate for theta
        fl_p_success_argmax <- (fl_min_p_success_cur + fl_max_p_success_cur) / 2

        # return
        return(list(
            fl_p_success_argmax = fl_p_success_argmax,
            fl_max_ln_like = fl_max_ln_like
        ))
    }
# Test with different M values
ar_drm_prob <- c(0.0014, 0.0571, 0.7931, 0.1462, 0.0022)
ffi_pbinom_lnlk_estitheta_fixm(ar_drm_prob = ar_drm_prob, it_m = 1)
ffi_pbinom_lnlk_estitheta_fixm(ar_drm_prob = ar_drm_prob, it_m = 5)
ffi_pbinom_lnlk_estitheta_fixm(ar_drm_prob = ar_drm_prob, it_m = 9)


## ----------------------------------------------------------------------------------------------------------------------------
# Find likelihood maximizing theta, given M
ffi_pbinom_lnlk_estijnt <-
    function(mt_drm_prob = matrix(
                 data = c(
                     0.8929,    0.1045,    0.0026,    0.0000,    0.0000,
                     0.0547,    0.6704,    0.2708,    0.0040,    0.0001
                 ),
                 nrow = 2, ncol = 5, byrow = TRUE
             ),
             verbose = FALSE) {

        # Initialize min and max and tolerance criteria
        it_drm_set <- dim(mt_drm_prob)[1]
        ar_it_m <- seq(1, 10)

        for (it_i in seq(1, it_drm_set)) {
            ls_estitheta_fixm_res <- sapply(
                ar_it_m,
                ffi_pbinom_lnlk_estitheta_fixm,
                ar_drm_prob = mt_drm_prob[it_i, ]
            )

            mt_estitheta_fixm_res <- t(matrix(
                as.numeric(ls_estitheta_fixm_res),
                nrow = 2, ncol = length(ls_estitheta_fixm_res) / 2
            ))
            mt_estitheta_fixm_res <- cbind(mt_estitheta_fixm_res, ar_it_m)
            colnames(mt_estitheta_fixm_res) <- c("esti_theta", "ln_like", "M")

            tb_estitheta_fixm_res <-
                as_tibble(mt_estitheta_fixm_res) %>%
                mutate(drm_group = it_i)

            if (it_i > 1) {
                tb_estitheta_fixm_res_stack <-
                    bind_rows(
                        tb_estitheta_fixm_res_stack,
                        tb_estitheta_fixm_res
                    )
            } else {
                tb_estitheta_fixm_res_stack <- tb_estitheta_fixm_res
            }
        }

        # Group max
        tb_estitheta_fixm_res_stack <- tb_estitheta_fixm_res_stack %>%
            group_by(drm_group) %>%
            mutate(drm_group_rank = min_rank(desc(ln_like))) %>%
            ungroup()

        # Overall M that maximizes sum of log likelihood
        tb_estitheta_fixm_res_stack <- tb_estitheta_fixm_res_stack %>%
            group_by(M) %>%
            mutate(ln_like_M_sum = sum(ln_like)) %>%
            ungroup() %>%
            mutate(drm_jnt_rank = dense_rank(desc(ln_like_M_sum))) %>%
            arrange(drm_group, M)

        # Return
        return(tb_estitheta_fixm_res_stack)
    }
# Test with full transition matrix
tb_rest_res <- ffi_pbinom_lnlk_estijnt(mt_drm_prob = mt_pi_kids_trans)
tb_rest_res %>% filter(drm_jnt_rank == 1)


## ----------------------------------------------------------------------------------------------------------------------------
# Compute predicted probabilities at maxmizing parameters
mt_dbinom_grouped <-
    apply(
        tb_rest_res %>% filter(drm_jnt_rank == 1),
        1,
        function(row) {
            ffi_pbinom_group_theta_m(
                fl_chance_success = row[["esti_theta"]],
                it_number_of_trials = (dim(mt_pi_kids_trans)[1] - 1),
                it_m = row[["M"]]
            )
        }
    )
# Show predictions
kable(round(t(mt_dbinom_grouped), 3),
    caption = "Predicted transitions given estimates"
) %>% kable_styling_fc()
# Show data
kable(round(mt_pi_kids_trans, 3),
    caption = "Data transitions probalities"
) %>% kable_styling_fc()
# Show data and prediction differences
kable(round(mt_pi_kids_trans - t(mt_dbinom_grouped), 3),
    caption = "Data transitions probalities - predictions"
) %>% kable_styling_fc()

