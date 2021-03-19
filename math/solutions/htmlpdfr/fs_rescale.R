## ----global_options, include = FALSE------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------------------------------
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
ar_grid_quad <- a*ar_x^2 + b*ar_x + c
# show values
kable(print(as_tibble(cbind(ar_x, ar_grid_quad))), 
      caption = paste0("Quadratic Fit of Three Equations and Three Unknowns\n",
                       "Satisfies: f(0)=0, f(10)=10, f(2)=3")) %>% 
  kable_styling_fc()


## -----------------------------------------------------------------------------------------------------------------------------
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
ar_grid_quad <- a*ar_x^2 + b*ar_x + c
# show values
# cbind(ar_x, ar_grid_quad)
ar_x[which.min(abs(ar_grid_quad - 0.75))]

