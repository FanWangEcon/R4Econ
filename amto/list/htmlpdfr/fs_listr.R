## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F--------------------------
options(knitr.duplicate.label = 'allow')

## ----loadlib, echo = T, results = 'hide', message=F, warning=F--------------------------------
library(tidyverse)
library(tidyr)
library(knitr)
library(kableExtra)


## ---------------------------------------------------------------------------------------------
# Define Lists
ls_num <- list(1,2,3)
ls_str <- list('1','2','3')
ls_num_str <- list(1,2,'3')

# Named Lists
ar_st_names <- c('e1','e2','e3')
ls_num_str_named <- ls_num_str
names(ls_num_str_named) <- ar_st_names

# Add Element to Named List
ls_num_str_named$e4 <- 'this is added'

# display
print(paste0('ls_num:', str(ls_num)))
print(paste0('ls_num[2:3]:', str(ls_num[2:3])))
print(paste0('ls_str:', str(ls_str)))
print(paste0('ls_str[2:3]:', str(ls_str[2:3])))
print(paste0('ls_num_str:', str(ls_num_str)))
print(paste0('ls_num_str[2:4]:', str(ls_num_str[2:4])))
print(paste0('ls_num_str_named:', str(ls_num_str_named)))
print(paste0('ls_num_str_named[c(\'e2\',\'e3\',\'e4\')]', str(ls_num_str_named[c('e2','e3','e4')])))


## ---------------------------------------------------------------------------------------------
# Dimensions
it_M <- 2
it_Q <- 3
it_N <- it_M*it_Q

# Initiate an Empty MxQ=N element list
ls_2d_flat <- vector(mode = "list", length = it_N)
ls_2d <- ls_2d_flat

# Named flat
ls_2d_flat_named <- ls_2d_flat
names(ls_2d_flat_named) <- paste0('e',seq(1,it_N))
ls_2d_named <- ls_2d_flat_named

# Reshape
dim(ls_2d) <- c(it_M, it_Q)
# named 2d list can not carry 1d name after reshape
dim(ls_2d_named) <- c(it_M, it_Q)

# display
print('ls_2d_flat')
print(ls_2d_flat)

print('ls_2d_flat_named')
print(ls_2d_flat_named)

print('ls_2d')
print(ls_2d)

print('ls_2d_named')
print(ls_2d_named)

# Select Values, double bracket to select from 2dim list
print('ls_2d[[1,2]]')
print(ls_2d[[1,2]])


## ---------------------------------------------------------------------------------------------
# Dimensions
it_M <- 3
it_Q <- 4
it_N <- it_M*it_Q

# Initiate an Empty MxQ=N element list
ls_2d_flat_named <- vector(mode = "list", length = it_N)
dim(ls_2d_flat_named) <- c(it_M, it_Q)

# Fill with values
for (it_Q_ctr in seq(1,it_Q)) {
  for (it_M_ctr in seq(1,it_M)) {
    # linear index
    ls_2d_flat_named[[it_M_ctr, it_Q_ctr]] <- (it_Q_ctr-1)*it_M+it_M_ctr
  }
}

# Replace row names, note rownames does not work
dimnames(ls_2d_flat_named)[[1]] <- paste0('row',seq(1,it_M))
dimnames(ls_2d_flat_named)[[2]] <- paste0('col',seq(1,it_Q))

# Element Specific Names
names(ls_2d_flat_named) <- paste0('e',seq(1,it_N))

# These are not element names, can still name each element
# display
print('ls_2d_flat_named')
print(ls_2d_flat_named)
print('str(ls_2d_flat_named)')
print(str(ls_2d_flat_named))

# Select elements with with dimnames
print('ls_2d_flat_named[[\'row2\',\'col2\']]')
print(ls_2d_flat_named[['row2','col2']])

# Select elements with element names
print('ls_2d_flat_named[[\'e5\']]')
print(ls_2d_flat_named[['e5']])

# Select elements with index
print('ls_2d_flat_named[[5]]')
print(ls_2d_flat_named[[5]])


