## ----global_options, include = FALSE------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------
#' Test polynomial fit to random draw x and y points
#'
#' @description Draw random sets of x and y points, fit a polynomial curve through
#' and compare predictions of y and actual y values.
#'
#' @param it_xy_pairs The number of x and y pair points
#' @param it_seed The random seed for drawing values
#' @param it_poly_fit An integer value of the order of polynomial
#' @param fl_mean The mean of the the random normal draw
#' @param fl_sd The standard deviation of the random normal draw
#' @returns
#' \itemize{
#'   \item rs_lm_poly - polynomial fit estimation results
#'   \item df_data_predict - N by 3 where N = \code{it_xy_pairs} and
#' columns are x, y, y-predict, and residual
#'   \item td_table_return - Display version of df_data_predict with title
#' }
#' @import stats, tibble, dplyr
#' @author Fan Wang, \url{http://fanwangecon.github.io}
ffi_lm_quad_fit <- function(it_xy_pairs = 3, it_seed = 123,
                            it_poly_fit = 2, fl_mean = 1, fl_sd = 1,
                            verbose = FALSE) {

  # 1. Generate three pairs of random numbers
  set.seed(it_seed)
  mt_rnorm <- matrix(
    rnorm(it_xy_pairs * 2, mean = fl_mean, sd = fl_sd),
    nrow = it_xy_pairs, ncol = 2
  )
  colnames(mt_rnorm) <- c("x", "y")
  rownames(mt_rnorm) <- paste0("p", seq(1, it_xy_pairs))
  df_rnorm <- as_tibble(mt_rnorm)

  # 2. Quadratic fit using ORTHOGONAL POLYNOMIAL
  # For predictions, lm(y ~ x + I(x^2)) and lm(y ~ poly(x, 2)) are the same,
  # but they have different parameters because x is transformed by poly().
  rs_lm_quad <- stats::lm(y ~ poly(x, it_poly_fit), data = df_rnorm)
  if (verbose) print(stats::summary.lm(rs_lm_quad))

  # 3. Fit prediction
  ar_y_predict <- stats::predict(rs_lm_quad)
  df_data_predict <- cbind(df_rnorm, ar_y_predict) %>%
    mutate(res = ar_y_predict - y)
  if (verbose) print(df_data_predict)

  # 4. show values
  st_poly_order <- "Quadratic"
  if (it_poly_fit != 2) {
    st_poly_order <- paste0(it_poly_fit, "th order")
  }
  td_table_return <- kable(df_data_predict,
    caption = paste0(
      st_poly_order, " Fit of ", it_xy_pairs, " Sets of Random (X,Y) Points"
    )
  ) %>%
    kable_styling_fc()

  return(list(
    rs_lm_quad = rs_lm_quad,
    df_data_predict = df_data_predict,
    td_table_return = td_table_return
  ))
}


## -----------------------------------------------------------------------------------
ls_ffi_lm_quad_fit <-
  ffi_lm_quad_fit(
    it_xy_pairs = 3, it_seed = 123,
    it_poly_fit = 2, fl_mean = 1, fl_sd = 1
  )
ls_ffi_lm_quad_fit$td_table_return


## -----------------------------------------------------------------------------------
ls_ffi_lm_quad_fit <-
  ffi_lm_quad_fit(
    it_xy_pairs = 4, it_seed = 345,
    it_poly_fit = 2, fl_mean = 1, fl_sd = 1
  )
ls_ffi_lm_quad_fit$td_table_return


## -----------------------------------------------------------------------------------
ls_ffi_lm_cubic_fit <-
  ffi_lm_quad_fit(
    it_xy_pairs = 4, it_seed = 345,
    it_poly_fit = 3, fl_mean = 1, fl_sd = 1
  )
ls_ffi_lm_cubic_fit$td_table_return

