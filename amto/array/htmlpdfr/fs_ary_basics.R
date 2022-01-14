## ----global_options, include = FALSE--------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------------------------------------------
ar_a <- c(1,2,3)
ar_b <- c(1,2,3,4)
prod(ar_a)
prod(ar_b)


## -------------------------------------------------------------------------------------------------------------------------
ar_a <- c(1,2,3)
ar_b <- c(1,2,3/1,2,3)
rep(0, length(ar_a))


## -------------------------------------------------------------------------------------------------------------------------
mt_x <- array(NA, dim=c(3, 3))
dim(mt_x)
print(mt_x)


## -------------------------------------------------------------------------------------------------------------------------
mt_x <- array(c(1, 1.5, 0, 2, 0, 4, 0, 3), dim=c(2, 4))
dim(mt_x)
print(mt_x)


## -------------------------------------------------------------------------------------------------------------------------
# Multidimensional Array
# 1 is r1c1t1, 1.5 in r2c1t1, 0 in r1c2t1, etc.
# Three dimensions, row first, column second, and tensor third
x <- array(c(1, 1.5, 0, 2, 0, 4, 0, 3), dim=c(2, 2, 2))
dim(x)
print(x)


## -------------------------------------------------------------------------------------------------------------------------
# A, Income Array
ar_income <- seq(0,200000,length.out=3)

# B. Exemptions and Deductions
fl_exemption <- 3500# exemption amount per household member
mt_deduction <- matrix(data=NA, nrow=2, ncol=5)# Marital-status and number of children-specific deduction
mt_deduction[1,1] <- 5450# Single filers
mt_deduction[1,2:5] <- 8000# Single filer with children
mt_deduction[2,] <- 10900# Married couples filing jointly

# C. Taxable Income
mn_taxable_income <- array(NA, dim=c(length(ar_income), 2, 5))
for (y in 1:length(ar_income)){
    for (m in 1:2){
        for (k in 0:4){
            mn_taxable_income[y,m,k+1] <- ar_income[y]-fl_exemption*m-fl_exemption*k-mt_deduction[m,k+1]
        }
    }
}

# D. Name dimensions
dimnames(mn_taxable_income)[[1]] = paste0('income=', round(ar_income, 0))
dimnames(mn_taxable_income)[[2]] = paste0('married=', 0:1)
dimnames(mn_taxable_income)[[3]] = paste0('kids=', 0:4)

# E. Print
dim(mn_taxable_income)
print(mn_taxable_income)


## -------------------------------------------------------------------------------------------------------------------------
it_M <- 5
it_N <- 4
ar_all_elements = seq(1,10,10)


## ----amto.array.fs_array_basics.slice.lastelement-------------------------------------------------------------------------
# Remove last element of array
vars.group.bydf <- c('23','dfa', 'wer')
vars.group.bydf[-length(vars.group.bydf)]
# Use the head function to remove last element
head(vars.group.bydf, -1)
head(vars.group.bydf, 2)


## -------------------------------------------------------------------------------------------------------------------------
# Remove first element of array
vars.group.bydf <- c('23','dfa', 'wer')
vars.group.bydf[2:length(vars.group.bydf)]
# Use Tail function
tail(vars.group.bydf, -1)
tail(vars.group.bydf, 2)


## -------------------------------------------------------------------------------------------------------------------------
# define array
ar_amin <- c(0, 0.25, 0.50, 0.75, 1)
# select without head and tail
tail(head(ar_amin, -1), -1)


## -------------------------------------------------------------------------------------------------------------------------
# define array
ar_amin <- c(0, 0.25, 0.50, 0.75, 1)
# select head and tail
c(head(ar_amin, 1), tail(ar_amin, 1))


## ----amto.array.fs_array_basics.NA.check----------------------------------------------------------------------------------
# Convert Inf and -Inf to NA
x <- c(1, -1, Inf, 10, -Inf)
na_if(na_if(x, -Inf), Inf)


## -------------------------------------------------------------------------------------------------------------------------
# Define a complex number
cx_number_a <- 0+0.0460246857561777i
# Define another complex number
cx_number_b <- complex(real = 0.02560982, imaginary = 0.0460246857561777)
# An array of numbers some of which are complex
ar_cx_number <- c(0.02560982+0.000000000i, 0.00000000+0.044895305i, 
                  0.00000000+0.009153429i, 0.05462045+0.000000000i, 
                  0.00000000+0.001198538i, 0.00000000+0.019267050i)


## -------------------------------------------------------------------------------------------------------------------------
# equi-length real component
ar_fl_number_re <- Re(ar_cx_number)
print(ar_fl_number_re)
# equi-length img component
ar_fl_number_im <- Im(ar_cx_number)
print(ar_fl_number_im)


## -------------------------------------------------------------------------------------------------------------------------
# subset of array that is real
ar_fl_number_re_subset <- Re(ar_cx_number[Re(ar_cx_number)!=0])
print(ar_fl_number_re_subset)


## -------------------------------------------------------------------------------------------------------------------------
# An array of integers
ar_it_vals <- seq(-5, 5, by = 1)
# Add positive sign in front of positive and zero elements
st_it_vals <- paste0(ar_it_vals)
st_it_vals[ar_it_vals>0] <- paste0("+", st_it_vals[ar_it_vals>0])
st_it_vals[ar_it_vals==0] <- paste0("±", st_it_vals[ar_it_vals==0])
# Display
print(st_it_vals)

