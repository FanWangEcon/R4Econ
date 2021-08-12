## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# polynomial coefficients
set.seed(123)
ar_coef_poly <- rnorm(4)
# time right hand side matrix
ar_t <- 0:3
ar_power <- 0:3
mt_t_data <- do.call(rbind, lapply(ar_power, function(power) {
  ar_t^power
}))
# Final matrix, each row is an observation, or time.
mt_t_data <- t(mt_t_data)
# General model prediction
ar_y <- mt_t_data %*% matrix(ar_coef_poly, ncol = 1, nrow = 4)
# Prediction and Input time matrix
mt_all_data <- cbind(ar_y, mt_t_data)
st_cap <- paste0(
  "C1=Y, each row is time, t=0, incremental by 1, ",
  "each column a polynomial term from 0th to higher."
)
kable(mt_all_data, caption = st_cap) %>% kable_styling_fc()


## ------------------------------------------------------------------------------------------------------
# The constant term
alpha_0 <- ar_y[1]
# The cubic term
alpha_3 <- as.numeric((t(ar_y) %*% c(-1, +3, -3, +1))/(3*2))
# The quadratic term, difference cubic out, alpha_2_1t3 = alpha_2_2t4
ar_y_hat <- ar_y - alpha_3*ar_t^3
alpha_2_1t3 <- as.numeric((t(ar_y_hat[1:3]) %*% c(1, -2, +1))/(2))
alpha_2_2t4 <- as.numeric((t(ar_y_hat[2:4]) %*% c(1, -2, +1))/(2))
alpha_2 <- alpha_2_1t3
# The linear term, difference cubic out and quadratic
ar_y_hat <- ar_y - alpha_3*ar_t^3 - alpha_2*ar_t^2
alpha_1_1t2 <- as.numeric((t(ar_y_hat[1:2]) %*% c(-1, +1))/(1))
alpha_1_2t3 <- as.numeric((t(ar_y_hat[2:3]) %*% c(-1, +1))/(1))
alpha_1_3t4 <- as.numeric((t(ar_y_hat[3:4]) %*% c(-1, +1))/(1))
alpha_1 <- alpha_1_1t2
# Collect results
ar_names <- c("Constant", "Linear", "Quadratic", "Cubic")
ar_alpha_solved <- c(alpha_0, alpha_1, alpha_2, alpha_3)
mt_alpha <- cbind(ar_names, ar_alpha_solved, ar_coef_poly)
# Display
ar_st_varnames <- c('Coefficient Counter', 'Polynomial Terms', 'Solved Coefficient Given Y', 'Actual DGP Coefficient')
tb_alpha <- as_tibble(mt_alpha) %>%
  rowid_to_column(var = "polynomial_term_coef") %>%
  rename_all(~c(ar_st_varnames))
# Display
st_cap = paste0('Solving for polynomial coefficients.')
kable(tb_alpha, caption = st_cap) %>% kable_styling_fc()

