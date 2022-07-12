## ----global_options, include = FALSE------------------------------------------------
try(source("../../.Rprofile"))


## clc

## clear

## 

## % Define inputs

## syms x11 x12 x21 x22 x31 x32 y1 y2 y3

## mt_sm_z = [1,x11,x12;1,x21,x22;1,x31,x32];

## ar_sm_y = [y1;y2;y3];

## 

## % Solve analytically

## ar_sm_solu = linsolve(mt_sm_z, ar_sm_y)

## 

## % Randomly draw x and y values

## rng(1234);

## mt_rand = rand(3,2);

## mt_rand = [0.1915, 0.6221, 0.4377;

##   0.7854, 0.7800, 0.2726]';

## [fl_x1, fl_x2, fl_x3] = deal(mt_rand(1,1), mt_rand(2,1), mt_rand(3,1));

## [fl_y1, fl_y2, fl_y3] = deal(mt_rand(1,2), mt_rand(2,2), mt_rand(3,2));

## [fl_x11, fl_x21, fl_x31] = deal(fl_x1, fl_x2, fl_x3);

## [fl_x12, fl_x22, fl_x32] = deal(fl_x1^2, fl_x2^2, fl_x3^2);

## 

## % Numerically evaluate coefficients

## ar_fl_solu = double(subs(ar_sm_solu, ...

##   {x11, x12, x21, x22, x31, x32, y1, y2, y3}, ...

##   {fl_x11,fl_x12,fl_x21,fl_x22,fl_x31,fl_x32,fl_y1,fl_y2,fl_y3}));

## disp(['ar_fl_solu=', num2str(ar_fl_solu')])

## 

## % Y predictions

## mt_fl_z = [1,fl_x11,fl_x12;1,fl_x21,fl_x22;1,fl_x31,fl_x32];

## ar_fl_y_pred = mt_fl_z*ar_fl_solu;

## ar_fl_x_actual = [fl_x1;fl_x2;fl_x3];

## ar_fl_y_actual = [fl_y1;fl_y2;fl_y3];

## 

## % Compare results

## tb_test = array2table([ar_fl_x_actual';ar_fl_y_actual';ar_fl_y_pred']');

## cl_col_names = ["x_actual","y_actual", "y_predict"];

## cl_row_names = strcat('obs_', string((1:3)));

## tb_test.Properties.VariableNames = matlab.lang.makeValidName(cl_col_names);

## tb_test.Properties.RowNames = matlab.lang.makeValidName(cl_row_names);

## display(tb_test);


## -----------------------------------------------------------------------------------
# Inputs X and Y
set.seed(123)
# Draw Randomly
mt_rnorm <- matrix(rnorm(6, mean = 1, sd = 1), nrow = 3, ncol = 2)
# # Fixed Values
# mt_rnorm <- matrix(c(
#   0.1915, 0.6221, 0.4377,
#   0.7854, 0.7800, 0.2726
# ), nrow = 3, ncol = 2)
colnames(mt_rnorm) <- c("x", "y")
x1 <- mt_rnorm[1, 1]
x2 <- mt_rnorm[2, 1]
x3 <- mt_rnorm[3, 1]
y1 <- mt_rnorm[1, 2]
y2 <- mt_rnorm[2, 2]
y3 <- mt_rnorm[3, 2]

# X quadratic
x11 <- x1
x12 <- x1**2
x21 <- x2
x22 <- x2**2
x31 <- x3
x32 <- x3**2

# Shared denominator
fl_denominator <- (x11 * x22 - x12 * x21
  - x11 * x32 + x12 * x31
  + x21 * x32 - x22 * x31)

# Solve for A, B, and C exact fit quadratic coefficients
fl_A <- (x11 * x22 * y3 - x12 * x21 * y3
  - x11 * x32 * y2 + x12 * x31 * y2
  + x21 * x32 * y1 - x22 * x31 * y1) / fl_denominator

fl_B <- -(x12 * y2 - x12 * y3
  - x22 * y1 + x22 * y3
  + x32 * y1 - x32 * y2) / fl_denominator

fl_C <- (x11 * y2 - x11 * y3
  - x21 * y1 + x21 * y3
  + x31 * y1 - x31 * y2) / fl_denominator

# Display
st_display <- paste0(
  "A(intercept)=", round(fl_A, 3),
  ", B(lin)=", round(fl_B, 3),
  ", C(quad)=", round(fl_C, 3)
)
print(st_display)


## -----------------------------------------------------------------------------------
# Estimation results
df_rnorm <- as_tibble(mt_rnorm)
# Linear and quadratic terms
rs_lm_quad <- stats::lm(y ~ x + I(x^2), data = df_rnorm)
print(stats::summary.lm(rs_lm_quad))
# Using orthogonal polynomials
# vs. rs_lm_quad: different parameters, but same predictions
rs_lm_quad_otho <- stats::lm(y ~ poly(x, 2), data = df_rnorm)
print(stats::summary.lm(rs_lm_quad_otho))


## -----------------------------------------------------------------------------------
# Matrix of input values
mt_vals_xs <- t(
  matrix(c(1, x1, x1**2, 1, x2, x2**2, 1, x3, x3**2),
    nrow = 3, ncol = 3
  )
)

# Predictions from LM poly prediction
ar_pred_lm <- mt_vals_xs %*% as.numeric(rs_lm_quad$coefficients)
as_pred_lm_otho <- stats::predict(rs_lm_quad_otho)

# Predictions based on analytical solutions
ar_pred_sym <- mt_vals_xs %*% c(fl_A, fl_B, fl_C)

# Combine results
kable(
  cbind(
    df_rnorm, ar_pred_sym,
    ar_pred_lm, as_pred_lm_otho
  ) %>%
    mutate(res = ar_pred_lm - y),
  caption = paste0(
    "Quadratic Fit of 3 Sets of Random (X,Y) Points"
  )
) %>% kable_styling_fc()


## clc

## clear

## 

## % Define inputs

## syms U V Q S

## mt_sm_z = [3, U; U, V];

## ar_sm_y = [Q; S];

## 

## % Solve analytically

## ar_sm_solu = linsolve(mt_sm_z, ar_sm_y)

## 

## % Randomly draw x and y values

## rng(1234);

## mt_rand = rand(3,2);

## % Use below to check not-exact fit, gap actual and predict of y

## mt_rand = [0.1915, 0.6221, 0.4377;

##   0.7854, 0.7800, 0.2726]';

## % Use below to check for exact fit 2nd 3rd points same

## % mt_rand = [0.1915, 0.6221, 0.6221;

## %  0.7854, 0.7800, 0.7800]';

## [fl_x1, fl_x2, fl_x3] = deal(mt_rand(1,1), mt_rand(2,1), mt_rand(3,1));

## [fl_y1, fl_y2, fl_y3] = deal(mt_rand(1,2), mt_rand(2,2), mt_rand(3,2));

## [fl_x11, fl_x21, fl_x31] = deal(fl_x1, fl_x2, fl_x3);

## [fl_x12, fl_x22, fl_x32] = deal(fl_x1^2, fl_x2^2, fl_x3^2);

## 

## % Define values of U V Q and S

## fl_U = fl_x11 + fl_x21 + fl_x31;

## fl_V = fl_x12 + fl_x22 + fl_x32;

## fl_Q = fl_y1 + fl_y2 + fl_y3;

## fl_S = fl_y1*fl_x11 + fl_y2*fl_x21 + fl_y3*fl_x31;

## 

## % Numerically evaluate coefficients

## ar_fl_solu = double(subs(ar_sm_solu, ...

##   {U, V, Q, S}, ...

##   {fl_U, fl_V, fl_Q, fl_S}));

## disp(['ar_fl_solu=', num2str(ar_fl_solu')])

## 

## % Y predictions

## mt_fl_z = [1,fl_x11;1,fl_x21;1,fl_x31];

## ar_fl_y_pred = mt_fl_z*ar_fl_solu;

## ar_fl_x_actual = [fl_x1;fl_x2;fl_x3];

## ar_fl_y_actual = [fl_y1;fl_y2;fl_y3];

## 

## % Compare results

## tb_test = array2table([ar_fl_x_actual';ar_fl_y_actual';ar_fl_y_pred']');

## cl_col_names = ["x_actual","y_actual", "y_predict"];

## cl_row_names = strcat('obs_', string((1:3)));

## tb_test.Properties.VariableNames = matlab.lang.makeValidName(cl_col_names);

## tb_test.Properties.RowNames = matlab.lang.makeValidName(cl_row_names);

## display(tb_test);


## -----------------------------------------------------------------------------------
# Inputs X and Y
set.seed(123)
# Draw Randomly
mt_rnorm <- matrix(rnorm(6, mean = 1, sd = 1), nrow = 3, ncol = 2)
# # Three fixed and different set of points
# mt_rnorm <- matrix(c(
#   0.1915, 0.6221, 0.4377,
#   0.7854, 0.7800, 0.2726
# ), nrow = 3, ncol = 2)
# # Below, the 2nd and 3rd points are the same
# mt_rnorm <- matrix(c(
#   0.1915, 0.6221, 0.6221,
#   0.7854, 0.7800, 0.7800
# ), nrow = 3, ncol = 2)

colnames(mt_rnorm) <- c("x", "y")
x1 <- mt_rnorm[1, 1]
x2 <- mt_rnorm[2, 1]
x3 <- mt_rnorm[3, 1]
y1 <- mt_rnorm[1, 2]
y2 <- mt_rnorm[2, 2]
y3 <- mt_rnorm[3, 2]

# X quadratic
x11 <- x1
x12 <- x1**2
x21 <- x2
x22 <- x2**2
x31 <- x3
x32 <- x3**2

# Define U and V, as well as Q and S
fl_U <- x11 + x21 + x31
fl_V <- x12 + x22 + x32
fl_Q <- y1 + y2 + y3
fl_S <- x11*y1 + x21*y2 + x31*y3

# Shared denominator
fl_denominator <- (3*fl_V - fl_U^2)

# Solve for A and B coefficients (not exact fit)
fl_A <- (fl_Q * fl_V - fl_S * fl_U) / fl_denominator

fl_B <- (3 * fl_S - fl_Q * fl_U) / fl_denominator

# Display
st_display <- paste0(
  "A(intercept)=", round(fl_A, 3),
  ", B(lin)=", round(fl_B, 3)
)
print(st_display)


## -----------------------------------------------------------------------------------
# Estimation results
df_rnorm <- as_tibble(mt_rnorm)
# Linear and quadratic terms
rs_lm_quad <- stats::lm(y ~ x, data = df_rnorm)
print(stats::summary.lm(rs_lm_quad))
# Using orthogonal polynomials
# vs. rs_lm_quad: different parameters, but same predictions
rs_lm_quad_otho <- stats::lm(y ~ poly(x, 1), data = df_rnorm)
print(stats::summary.lm(rs_lm_quad_otho))


## -----------------------------------------------------------------------------------
# Matrix of input values
mt_vals_xs <- t(
  matrix(c(1, x1, 1, x2, 1, x3),
    nrow = 2, ncol = 3
  )
)

# Predictions from LM poly prediction
ar_pred_lm <- mt_vals_xs %*% as.vector(rs_lm_quad$coefficients)
as_pred_lm_otho <- stats::predict(rs_lm_quad_otho)

# Predictions based on analytical solutions
ar_pred_sym <- mt_vals_xs %*% c(fl_A, fl_B)

# Combine results
kable(
  cbind(
    df_rnorm, ar_pred_sym,
    ar_pred_lm, as_pred_lm_otho
  ) %>%
    mutate(res = ar_pred_lm - y),
  caption = paste0(
    "Linear Fit of 3 Sets of Random (X,Y) Points"
  )
) %>% kable_styling_fc()

