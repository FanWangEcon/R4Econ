## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----fixed matrix------------------------------------------------------------------------------------------------------------
ar_row_one <- c(-1,+1)
ar_row_two <- c(-3,-2)
ar_row_three <- c(0.35,0.75)

mt_n_by_2 <- rbind(ar_row_one, ar_row_two, ar_row_three)
kable(mt_n_by_2) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# An empty matrix with Logical NA
mt_named <- matrix(data=NA, nrow=2, ncol=2)
colnames(mt_named) <- paste0('c', seq(1,2))
rownames(mt_named) <- paste0('r', seq(1,2))
mt_named


## ----------------------------------------------------------------------------------------------------------------------------
# An empty matrix with Logical NA
mt_na <- matrix(data=NA, nrow=2, ncol=2)
str(mt_na)

# An empty matrix with numerica NA
mt_fl_na <- matrix(data=NA_real_, nrow=2, ncol=2)
mt_it_na <- matrix(data=NA_integer_, nrow=2, ncol=2)

str(mt_fl_na)
str(mt_fl_na)


## ----random matrix-----------------------------------------------------------------------------------------------------------
# Generate 15 random normal, put in 5 rows, and 3 columns
mt_rnorm <- matrix(rnorm(15,mean=0,sd=1), nrow=5, ncol=3)

# Generate 15 random normal, put in 5 rows, and 3 columns
mt_runif <- matrix(runif(15,min=0,max=1), nrow=5, ncol=5)

# Combine
mt_rnorm_runif <- cbind(mt_rnorm, mt_runif)

# Display
kable(round(mt_rnorm_runif, 3)) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# with byrow set to FALSE, will fill first col, then second col, etc..
mt_index_colbycol <- matrix(seq(0, 15), nrow=4, ncol=4, byrow=FALSE)
# Display
kable(mt_index_colbycol,
  caption= "with byrow=FALSE, the default, will fill col by col") %>%
  kable_styling_fc()
# with byrow set to TRUE, will fill row by row
mt_index_rowbyrow <- matrix(seq(0, 15), nrow=4, ncol=4, byrow=TRUE)
# Display
kable(mt_index_rowbyrow,
  caption= " with byrow=TRUE, will fill row by row") %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
fl_max_val <- 0.8
fl_min_val <- 0.2
mt_rnorm_runif_bd <- mt_rnorm_runif
mt_rnorm_runif_bd[which(mt_rnorm_runif < fl_min_val)] <- NA_real_
mt_rnorm_runif_bd[which(mt_rnorm_runif > fl_max_val)] <- NA_real_
# Print
print(mt_rnorm_runif_bd)


## ----------------------------------------------------------------------------------------------------------------------------
# Within row sort
mt_rnorm_runif_row_sort <- t(apply(
  mt_rnorm_runif, 1, sort
))
# Within column sort, note no transpose
mt_rnorm_runif_col_sort <- apply(
  mt_rnorm_runif, 2, sort
)
# Display
kable(round(mt_rnorm_runif_row_sort, 3),
      caption="Each row sort low to high") %>%
  kable_styling_fc()
kable(round(mt_rnorm_runif_col_sort, 3),
      caption="Each column sort low to high") %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
print(paste0('colSums=',
             paste(round(
               colSums(mt_rnorm_runif),3), collapse=',')
             ))
print(paste0('colMeans=',
             paste(round(
               colMeans(mt_rnorm_runif),3), collapse=',')
             ))
print(paste0('rowSums=',
             paste(round(
               rowSums(mt_rnorm_runif),3), collapse=',')
             ))
print(paste0('rowMeans=',
             paste(round(
               rowMeans(mt_rnorm_runif),3), collapse=',')
             ))


## ----------------------------------------------------------------------------------------------------------------------------
fl_new_first_col_val <- 111
fl_new_last_col_val <- 999
mt_with_more_columns <- cbind(rep(fl_new_first_col_val, dim(mt_rnorm_runif)[1]),
                              mt_rnorm_runif,
                              rep(fl_new_last_col_val, dim(mt_rnorm_runif)[1]))
# Display
kable(mt_with_more_columns) %>% kable_styling_fc_wide()

