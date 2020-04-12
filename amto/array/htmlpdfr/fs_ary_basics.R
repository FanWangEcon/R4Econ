## ----global_options, include = FALSE--------------------------------------------------------------
try(source("../../.Rprofile"))


## ----amto.array.fs_array_basics.multi.2d----------------------------------------------------------
# Multidimensional Array
# 1 is r1c1t1, 1.5 in r2c1t1, 0 in r1c2t1, etc.
# Three dimensions, row first, column second, and tensor third
x <- array(c(1, 1.5, 0, 2, 0, 4, 0, 3), dim=c(2, 2, 2))
dim(x)
print(x)


## ----amto.array.fs_array_basics.slice.lastelement-------------------------------------------------
# Remove last element of array
vars.group.bydf <- c('23','dfa', 'wer')
vars.group.bydf[-length(vars.group.bydf)]


## ----amto.array.fs_array_basics.NA.check----------------------------------------------------------
# Convert Inf and -Inf to NA
x <- c(1, -1, Inf, 10, -Inf)
na_if(na_if(x, -Inf), Inf)

