## ----global_options, include = FALSE-----------------------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----amto.matrix.fs_mat_linear_algebra.matrix_multiply-----------------------------------------------------------------------------------------------------------------
ar_row_one <- c(-1,+1)
ar_row_two <- c(-3,-2)
ar_row_three <- c(0.35,0.75)
mt_n_by_2 <- rbind(ar_row_one, ar_row_two, ar_row_three)

ar_row_four <- c(3,4)

# Matrix Multiplication
mt_out <- mt_n_by_2 %*% ar_row_four
print(mt_n_by_2)
print(ar_row_four)
print(mt_out)

