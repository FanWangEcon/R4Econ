## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------
# Construct the formula
ffi_theta_lambda_0t1 <- function(theta, lambda) {
  if (is.finite(exp(lambda))) {
    theta * (exp(lambda) / (1 + theta * (exp(lambda) - 1)))
  } else {
    # If lambda is large, exp(lambda)=inf, ratio above becomes 1
    1
  }
}
# Test the function
print(ffi_theta_lambda_0t1(0.5, 1e1))
print(ffi_theta_lambda_0t1(0.5, 1e2))
print(ffi_theta_lambda_0t1(0.5, -1e3))


## ----------------------------------------------------------------------------------------------------------------------------
# Create Function
ffi_fixtheta_varylambda_0t1 <-
  function(ar_lambda_pos =
             1e1^(seq(-0.1, 1, length.out = 4)),
           theta = 0.5) {

    # Construct lambda vector
    ar_lambda <- sort(unique((c(-ar_lambda_pos, 0, ar_lambda_pos))))
    # sapply
    ar_theta_hat <- sapply(ar_lambda, ffi_theta_lambda_0t1, theta = theta)
    # Create table
    ar_st_varnames <- c("theta_hat", "lambda")
    tb_theta_hat_lambda <- as_tibble(
      cbind(round(ar_theta_hat, 5), ar_lambda)
    ) %>%
      rename_all(~ c(ar_st_varnames)) %>%
      mutate(theta = theta)
    # return
    return(tb_theta_hat_lambda)
  }

# Test function
tb_theta_hat_lambda <- ffi_fixtheta_varylambda_0t1()
# Print
kable(tb_theta_hat_lambda,
  caption = paste(
    "Theta-hat rescaling",
    ", given different lambda rescalers.",
    separator = " "
  )
) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# Evaluate at differing thetas
ar_lambda_pos <- 1e1^(seq(-0.1, 1, length.out = 2))
tb_theta_hat_lambda_low <- ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.1)
tb_theta_hat_lambda_mid <- ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.5)
tb_theta_hat_lambda_hgh <- ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.9)
# Combine
tb_theta_hat_lambda_combo <- bind_rows(
  tb_theta_hat_lambda_low,
  tb_theta_hat_lambda_mid,
  tb_theta_hat_lambda_hgh
)
# Print
kable(tb_theta_hat_lambda_combo,
  caption = paste(
    "Theta-hat rescaling",
    ", with multiple theta values",
    ", given different lambda rescalers.",
    separator = " "
  )
) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# Generate a denser result
ar_lambda_pos <- 1e1^(seq(-0.1, 1, length.out = 100))
tb_theta_hat_lambda_combo <- bind_rows(
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.01),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.10),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.25),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.50),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.75),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.90),
  ffi_fixtheta_varylambda_0t1(ar_lambda_pos, 0.99)
)
# Labeling
st_title <- paste0("Rescale a Fraction (theta), Constrained Between 0 and 1")
st_subtitle <- paste0(
  "Note that when LAMBDA = 0, THETA-HAT = theta."
)
st_caption <- paste0(
  "https://fanwangecon.github.io/",
  "R4Econ/math/solutions/htmlpdfr/fs_rescale.html"
)
st_x_label <- "Different rescaling LAMBDA values"
st_y_label <- "Rescaled THETA-HAT values between 0 and 1"
ar_y_breaks <- c(0.01, 0.10, 0.25, 0.50, 0.75, 0.90, 0.99)
# Graph
tb_theta_hat_lambda_combo %>%
  mutate(theta = as.factor(theta)) %>%
  ggplot(aes(
    x = lambda, y = theta_hat,
    color = theta, linetype = theta
  )) +
  geom_line(size = 2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed", color = "black", size = 1
  ) +
  labs(
    title = st_title,
    subtitle = st_subtitle,
    x = st_x_label,
    y = st_y_label,
    caption = st_caption
  ) +
  scale_y_continuous(
    breaks = ar_y_breaks,
    limits = c(0, 1)
  ) +
  theme(
    panel.grid.major.y = element_line(
      color = "black",
      size = 0.5,
      linetype = 1
    ),
    legend.key.width = unit(3, "line")
  )


## ----------------------------------------------------------------------------------------------------------------------------
# set values
e <- 0
f <- 10
z <- 2
alpha <- 1.5
# apply formulas from above
a <- -0.0625
b <- 1.625
c <- 0
# grid of values beween a and b, 11 points covering z = 2
ar_x <- seq(e, f, length.out = 11)
# rescale
ar_grid_quad <- a * ar_x^2 + b * ar_x + c
# show values
kable(print(as_tibble(cbind(ar_x, ar_grid_quad))),
  caption = paste0(
    "Quadratic Fit of Three Equations and Three Unknowns\n",
    "Satisfies: f(0)=0, f(10)=10, f(2)=3"
  )
) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# set values
e <- 0
f <- 3.5
z <- 0.5
alpha <- 1.5
# apply formulas from above
a <- -0.16666666
b <- 1.583333333
c <- 0
# grid of values beween a and b, 11 points covering z = 2
ar_x <- seq(e, f, length.out = 100000)
# rescale
ar_grid_quad <- a * ar_x^2 + b * ar_x + c
# show values
# cbind(ar_x, ar_grid_quad)
ar_x[which.min(abs(ar_grid_quad - 0.75))]

