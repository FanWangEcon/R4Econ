## ----global_options, include = FALSE------------------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# generate income cut-offs
fl_bin_start <- 0
# width equal to 20,000
fl_bin_width <- 2e4
# final point is 100 million
fl_bin_final_end <- 1e8
# final segment starting point is 100,000 dollars
fl_bin_final_start <- 1e5
# generate tincome bins
ar_income_bins <- c(seq(fl_bin_start, fl_bin_final_start, by=fl_bin_width), 
                    fl_bin_final_end)
# Display
print(ar_income_bins)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
fl_bin_start <- 0
fl_bin_width <- 5e3
fl_bin_final_end <- 1e8
fl_bin_final_start <- 2e5
ar_income_bins <- c(seq(fl_bin_start, fl_bin_final_start, by=fl_bin_width), 
                    fl_bin_final_end)
print(ar_income_bins)


## ----amto.array.fs_gen_arrays.special.log-------------------------------------------------------------------------------------------------------------------------
# Parameters
it.lower.bd.inc.cnt <- 3
fl.log.lower <- -10
fl.log.higher <- -9
fl.min.rescale <- 0.01
it.log.count <- 4
# Generate
ar.fl.log.rescaled <- exp(log(10)*seq(log10(fl.min.rescale),
                                      log10(fl.min.rescale +
                                              (fl.log.higher-fl.log.lower)),
                                      length.out=it.log.count))
ar.fl.log <- ar.fl.log.rescaled + fl.log.lower - fl.min.rescale
# Print
ar.fl.log


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
it_len <- 10
ar_x <- ceiling(runif(it_len)*5+10)
ar_prob <- dbinom(seq(0,it_len-1,length.out = it_len), it_len-1, prob=0.5)
print(cbind(ar_x,ar_prob))
print(paste0('sum(ar_prob)=',sum(ar_prob)))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ls_sorted_res <- sort(ar_x, decreasing = FALSE, index.return=TRUE)
ar_idx_increasing_x <- ls_sorted_res$ix
ar_x_sorted <- ls_sorted_res$x
ar_prob_sorted <- ar_prob[ar_idx_increasing_x]
print(cbind(ar_x_sorted,ar_prob_sorted))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ar_x_unique <- unique(ar_x_sorted)
mt_prob_unique <- aggregate(ar_prob_sorted, by=list(ar_x_sorted), FUN=sum)
ar_x_unique_prob <- mt_prob_unique$x
print(cbind(ar_x_unique, ar_x_unique_prob))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
# data
set.seed(123)
it_len <- 30
ar_x <- ceiling(runif(it_len)*20+10)
ar_prob <- runif(it_len)
ar_prob <- ar_prob/sum(ar_prob)
# step 1, sort
ls_sorted_res <- sort(ar_x, decreasing = FALSE, index.return=TRUE)
# step 2, unique sorted
ar_x_unique <- unique(ls_sorted_res$x)
# step 3, mass for each unique
mt_prob_unique <- aggregate(ar_prob[ls_sorted_res$ix], by=list(ls_sorted_res$x), FUN=sum)
ar_x_unique_prob <- mt_prob_unique$x
# results
print(cbind(ar_x_unique, ar_x_unique_prob))

