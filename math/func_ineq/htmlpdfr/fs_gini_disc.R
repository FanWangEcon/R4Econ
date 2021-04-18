## ----global_options, include = FALSE------------------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Formula, directly implement the GINI formula Following Step 4 above
ffi_dist_gini_vector_pos_test <- function(ar_pos) {
  # Check length and given warning
  it_n <- length(ar_pos)
  if (it_n <= 100)  warning('Data vector has n=',it_n,', max-inequality/max-gini=',(it_n-1)/(it_n + 1))
  # Sort
  ar_pos <- sort(ar_pos)
  # formula implement
  fl_gini <- 1 - ((2/(it_n+1)) * sum(cumsum(ar_pos))*(sum(ar_pos))^(-1))
  return(fl_gini)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Example Arrays of data
ar_equal_n1 = c(1)
ar_ineql_n1 = c(100)

ar_equal_n2 = c(1,1)
ar_ineql_alittle_n2 = c(1,2)
ar_ineql_somewht_n2 = c(1,2^3)
ar_ineql_alotine_n2 = c(1,2^5)
ar_ineql_veryvry_n2 = c(1,2^8)
ar_ineql_mostmst_n2 = c(1,2^13)

ar_equal_n10 = c(2,2,2,2,2,2, 2, 2, 2, 2)
ar_ineql_some_n10 = c(1,2,3,5,8,13,21,34,55,89)
ar_ineql_very_n10 = c(1,2^2,3^2,5^2,8^2,13^2,21^2,34^2,55^2,89^2)
ar_ineql_extr_n10 = c(1,2^2,3^3,5^4,8^5,13^6,21^7,34^8,55^9,89^10)


## ---- eval=TRUE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
# Hard-Code Small N Dist Tests
cat('\nSmall N=1 Hard-Code\n')
cat('ar_equal_n1:', ffi_dist_gini_vector_pos_test(ar_equal_n1), '\n')
cat('ar_ineql_n1:', ffi_dist_gini_vector_pos_test(ar_ineql_n1), '\n')

cat('\nSmall N=2 Hard-Code, converge to 1/3, see formula above\n')
cat('ar_ineql_alittle_n2:', ffi_dist_gini_vector_pos_test(ar_ineql_alittle_n2), '\n')
cat('ar_ineql_somewht_n2:', ffi_dist_gini_vector_pos_test(ar_ineql_somewht_n2), '\n')
cat('ar_ineql_alotine_n2:', ffi_dist_gini_vector_pos_test(ar_ineql_alotine_n2), '\n')
cat('ar_ineql_veryvry_n2:', ffi_dist_gini_vector_pos_test(ar_ineql_veryvry_n2), '\n')

cat('\nSmall N=10 Hard-Code, convege to 9/11=0.8181, see formula above\n')
cat('ar_equal_n10:', ffi_dist_gini_vector_pos_test(ar_equal_n10), '\n')
cat('ar_ineql_some_n10:', ffi_dist_gini_vector_pos_test(ar_ineql_some_n10), '\n')
cat('ar_ineql_very_n10:', ffi_dist_gini_vector_pos_test(ar_ineql_very_n10), '\n')
cat('ar_ineql_extr_n10:', ffi_dist_gini_vector_pos_test(ar_ineql_extr_n10), '\n')


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ar_choice_unique_sorted <- seq(0, 100, by=1)
ar_choice_prob <- dbinom(ar_choice_unique_sorted, 100, 0.01)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. to normalize, get mean (binomial so mean is p*N=50)
fl_mean <- sum(ar_choice_unique_sorted*ar_choice_prob);
# 2. get cumulative mean at each point
ar_mean_cumsum <- cumsum(ar_choice_unique_sorted*ar_choice_prob);


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3. Share of wealth (income etc) accounted for up to this sorted type
ar_height <- ar_mean_cumsum/fl_mean;
# 4. The total area, is the each height times each width summed up
fl_area_drm <- sum(ar_choice_prob*ar_height);


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# 5. area below 45 degree line might not be 1/2, depends on discretness
fl_area_below45 <- sum(ar_choice_prob*(cumsum(ar_choice_prob)/sum(ar_choice_prob)))

# 6. Gini is the distance between
fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45
print(paste0('fl_gini_index=', fl_gini_index))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Combining the code from above
ffi_dist_gini_random_var_pos_test <- function(ar_x_sorted, ar_prob_of_x) {
  # Check length and given warning
  
  # 1. to normalize, get mean (binomial so mean is p*N=50)
  fl_mean <- sum(ar_x_sorted*ar_prob_of_x);
  # 2. get cumulative mean at each point
  ar_mean_cumsum <- cumsum(ar_x_sorted*ar_prob_of_x);
  # 3. Share of wealth (income etc) accounted for up to this sorted type
  ar_height <- ar_mean_cumsum/fl_mean;
  # 4. The total area, is the each height times each width summed up
  fl_area_drm <- sum(ar_prob_of_x*ar_height);
  # 5. area below 45 degree line might not be 1/2, depends on discretness
  fl_area_below45 <- sum(ar_prob_of_x*(cumsum(ar_prob_of_x)/sum(ar_prob_of_x)))
  # 6. Gini is the distance between
  fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45
  
  return(fl_gini_index)
}



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
for (fl_binom_success_prob in seq(0.0001,0.9999,length.out=10)) {
  ar_x_sorted <- seq(0, 100, by=1)
  ar_prob_of_x <- dbinom(ar_x_sorted, 100, fl_binom_success_prob)
  fl_gini_index <- ffi_dist_gini_random_var_pos_test(ar_x_sorted, ar_prob_of_x)
  st_print <- paste0('binom p(success)=', fl_binom_success_prob ,
                   ', the fl_gini_index=', fl_gini_index)
  print(st_print)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# array
ar_x <- seq(1, 100, length.out = 30)
# prob array
ar_prob_x_unif <- rep.int(1, length(ar_x))/sum(rep.int(1, length(ar_x)))
# prob higher at lower values
ar_prob_x_lowval_highwgt <- rev(cumsum(ar_prob_x_unif))/sum(cumsum(ar_prob_x_unif))
# prob higher at lower values
ar_prob_x_highval_highwgt <- (cumsum(ar_prob_x_unif))/sum(cumsum(ar_prob_x_unif))
# show
kable(cbind(ar_x, ar_prob_x_unif, ar_prob_x_lowval_highwgt, ar_prob_x_highval_highwgt)) %>%
  kable_styling_fc()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ff_dist_gini_vector_pos(ar_x)
ff_dist_gini_random_var(ar_x, ar_prob_x_unif)
ff_dist_gini_random_var(ar_x, ar_prob_x_lowval_highwgt)
ff_dist_gini_random_var(ar_x, ar_prob_x_highval_highwgt)

