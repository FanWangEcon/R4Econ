## ----global_options, include = FALSE-------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Formula, directly implement the GINI formula Following Step 4 above
fv_dist_gini_vector_pos_test <- function(ar_pos) {
  # Check length and given warning
  it_n <- length(ar_pos)
  if (it_n <= 100)  warning('Data vector has n=',it_n,', max-inequality/max-gini=',(it_n-1)/(it_n + 1))
  # Sort
  ar_pos <- sort(ar_pos)
  # formula implement
  fl_gini <- 1 - ((2/(it_n+1)) * sum(cumsum(ar_pos))*(sum(ar_pos))^(-1))
  return(fl_gini)
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---- eval=TRUE, echo=FALSE----------------------------------------------------------------------------------------------------------------------------
# Hard-Code Small N Dist Tests
cat('\nSmall N=1 Hard-Code\n')
cat('ar_equal_n1:', fv_dist_gini_vector_pos_test(ar_equal_n1), '\n')
cat('ar_ineql_n1:', fv_dist_gini_vector_pos_test(ar_ineql_n1), '\n')

cat('\nSmall N=2 Hard-Code, converge to 1/3, see formula above\n')
cat('ar_ineql_alittle_n2:', fv_dist_gini_vector_pos_test(ar_ineql_alittle_n2), '\n')
cat('ar_ineql_somewht_n2:', fv_dist_gini_vector_pos_test(ar_ineql_somewht_n2), '\n')
cat('ar_ineql_alotine_n2:', fv_dist_gini_vector_pos_test(ar_ineql_alotine_n2), '\n')
cat('ar_ineql_veryvry_n2:', fv_dist_gini_vector_pos_test(ar_ineql_veryvry_n2), '\n')

cat('\nSmall N=10 Hard-Code, convege to 9/11=0.8181, see formula above\n')
cat('ar_equal_n10:', fv_dist_gini_vector_pos_test(ar_equal_n10), '\n')
cat('ar_ineql_some_n10:', fv_dist_gini_vector_pos_test(ar_ineql_some_n10), '\n')
cat('ar_ineql_very_n10:', fv_dist_gini_vector_pos_test(ar_ineql_very_n10), '\n')
cat('ar_ineql_extr_n10:', fv_dist_gini_vector_pos_test(ar_ineql_extr_n10), '\n')

