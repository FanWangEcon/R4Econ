## ----global_options, include = FALSE------------------------------------------------
try(source("../../.Rprofile"))


## ----fixed matrix-------------------------------------------------------------------
ar_row_one <- c(-1,+1)
ar_row_two <- c(-3,-2)
ar_row_three <- c(0.35,0.75)

mt_n_by_2 <- rbind(ar_row_one, ar_row_two, ar_row_three)
kable(mt_n_by_2) %>%
  kable_styling_fc()


## -----------------------------------------------------------------------------------
# An empty matrix with Logical NA
mt_named <- matrix(data=NA, nrow=2, ncol=2)
colnames(mt_named) <- paste0('c', seq(1,2))
rownames(mt_named) <- paste0('r', seq(1,2))
mt_named


## -----------------------------------------------------------------------------------
# An empty matrix with Logical NA
mt_na <- matrix(data=NA, nrow=2, ncol=2)
str(mt_na)

# An empty matrix with numerica NA
mt_fl_na <- matrix(data=NA_real_, nrow=2, ncol=2)
mt_it_na <- matrix(data=NA_integer_, nrow=2, ncol=2)

str(mt_fl_na)
str(mt_fl_na)


## ----random matrix------------------------------------------------------------------
# Generate 15 random normal, put in 5 rows, and 3 columns
mt_rnorm <- matrix(rnorm(15,mean=0,sd=1), nrow=5, ncol=3)

# Generate 15 random normal, put in 5 rows, and 3 columns
mt_runif <- matrix(runif(15,min=0,max=1), nrow=5, ncol=5)

# Combine
mt_rnorm_runif <- cbind(mt_rnorm, mt_runif)

# Display
kable(round(mt_rnorm_runif, 3)) %>% kable_styling_fc()


## -----------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------
fl_new_first_col_val <- 111
fl_new_last_col_val <- 999
mt_with_more_columns <- cbind(rep(fl_new_first_col_val, dim(mt_rnorm_runif)[1]),
                              mt_rnorm_runif,
                              rep(fl_new_last_col_val, dim(mt_rnorm_runif)[1]))
# Display
kable(mt_with_more_columns) %>% kable_styling_fc_wide()

