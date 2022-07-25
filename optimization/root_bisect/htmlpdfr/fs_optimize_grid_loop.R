## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------
# Define Function
ffi_quad_func <- function(fl_mu) {
  1 + (fl_mu + 2)^2
}
# Test Function
print(paste0("ffi_quad_func(-3)=", ffi_quad_func(-3)))
print(paste0("ffi_quad_func(-2)=", ffi_quad_func(-2)))
print(paste0("ffi_quad_func(-1)=", ffi_quad_func(-1)))


## ----------------------------------------------------------------------------------------------------------------------------
# Function
ffi_find_min <- function(fl_min = -4, fl_max = 2, it_grid_len = 7) {

  # Construct grid where to evaluate the function
  ar_fl_mu <- seq(fl_min, fl_max, length.out = it_grid_len)

  # Evaluate likelihood
  ar_obj <- sapply(ar_fl_mu, ffi_quad_func)

  # Find min grid
  it_min_idx <- which.min(ar_obj)
  fl_min_val <- ar_obj[it_min_idx]

  # Find lower and upper bound
  fl_min_new <- ar_fl_mu[max(it_min_idx - 1, 1)]
  fl_max_new <- ar_fl_mu[min(it_min_idx + 1, it_grid_len)]

  # return
  return(list(
    fl_min_val = fl_min_val,
    fl_min_new = fl_min_new,
    fl_max_new = fl_max_new
  ))
}
# Test Function
print("ffi_find_min(-3,-1,10)")
print(ffi_find_min(-3, -1, 10))
# Test function if lower bound is actual min
print("ffi_find_min(-2,-1,10)")
print(ffi_find_min(-2, -1, 10))
# Test function if upper bound is actual min
print("ffi_find_min(-3,-2,10)")
print(ffi_find_min(-3, -2, 10))


## ----------------------------------------------------------------------------------------------------------------------------
# Initialize min and max and tolerance criteria
fl_min_cur <- -10
fl_max_cur <- 10
it_grid_len <- 10
fl_tol <- 1e-5
it_max_iter <- 5

# Initialize initial gaps etc
fl_gap <- 1e5
fl_min_val_last <- 1e5
it_iter <- 0

# Iteratively loop over grid to find the maximum by zooming in
while ((fl_gap > fl_tol) && it_iter <= it_max_iter) {

  # Iterator counts up
  it_iter <- it_iter + 1
  print(paste0("it_iter=", it_iter))

  # build array
  ls_find_min <- ffi_find_min(
    fl_min = fl_min_cur, fl_max = fl_max_cur, it_grid_len = it_grid_len
  )

  # Min objective value current
  fl_min_val <- ls_find_min$fl_min_val
  # Find new lower and upper bound
  fl_min_cur <- ls_find_min$fl_min_new
  fl_max_cur <- ls_find_min$fl_max_new
  print(paste0("fl_min_cur=", fl_min_cur))
  print(paste0("fl_max_cur=", fl_max_cur))

  # Compare
  fl_gap <- abs(fl_min_val - fl_min_val_last)
  fl_min_val_last <- fl_min_val
  print(paste0("fl_gap=", fl_gap))
}

