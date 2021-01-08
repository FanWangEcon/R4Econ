## ----global_options, include = FALSE----------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---------------------------------------------------------------------------------------------------------------------
ar_a <- c(1,2,3)
ar_b <- c(1,2,3/1,2,3)
rep(0, length(ar_a))


## ----amto.array.fs_array_basics.multi.2d------------------------------------------------------------------------------
# Multidimensional Array
# 1 is r1c1t1, 1.5 in r2c1t1, 0 in r1c2t1, etc.
# Three dimensions, row first, column second, and tensor third
x <- array(c(1, 1.5, 0, 2, 0, 4, 0, 3), dim=c(2, 2, 2))
dim(x)
print(x)


## ----amto.array.fs_array_basics.slice.lastelement---------------------------------------------------------------------
# Remove last element of array
vars.group.bydf <- c('23','dfa', 'wer')
vars.group.bydf[-length(vars.group.bydf)]
# Use the head function to remove last element
head(vars.group.bydf, -1)
head(vars.group.bydf, 2)


## ---------------------------------------------------------------------------------------------------------------------
# Remove first element of array
vars.group.bydf <- c('23','dfa', 'wer')
vars.group.bydf[2:length(vars.group.bydf)]
# Use Tail function
tail(vars.group.bydf, -1)
tail(vars.group.bydf, 2)


## ---------------------------------------------------------------------------------------------------------------------
# define array
ar_amin <- c(0, 0.25, 0.50, 0.75, 1)
# select without head and tail
tail(head(ar_amin, -1), -1)


## ---------------------------------------------------------------------------------------------------------------------
# define array
ar_amin <- c(0, 0.25, 0.50, 0.75, 1)
# select head and tail
c(head(ar_amin, 1), tail(ar_amin, 1))


## ----amto.array.fs_array_basics.NA.check------------------------------------------------------------------------------
# Convert Inf and -Inf to NA
x <- c(1, -1, Inf, 10, -Inf)
na_if(na_if(x, -Inf), Inf)

