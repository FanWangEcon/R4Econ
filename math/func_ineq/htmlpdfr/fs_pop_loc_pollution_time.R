## ----global_options, include = FALSE-----------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------
# Locations and Time periods
it_M_location <- 10
it_D_y <- 12
# Locations to display
it_loc_disp <- 10
ar_it_loc_disp <- seq(1, it_M_location, length.out=it_loc_disp)
ar_it_loc_disp <- round(ar_it_loc_disp)
# Number of time periods to display
it_time_disp <- 12
ar_it_time_disp <- seq(1, it_D_y, length.out=it_time_disp)
ar_it_time_disp <- round(ar_it_time_disp)


## ----------------------------------------------------------------------------------------------------------
# Define parameters
fl_mean_meanlog_acrossM <- 3.4
fl_sd_meanlog_acrossM <- 0.5
fl_mean_sdlog_acrossM <- 0.5
fl_sd_sdlog_acrossM <- 0.1
# Vector of means and sds by locations
set.seed(123)
# random means
ar_meanlog_acrossM <- log(rlnorm(
  it_M_location, meanlog = fl_mean_meanlog_acrossM, sdlog=fl_sd_meanlog_acrossM))
# random sd 
ar_sdlog_acrossM <- log(rlnorm(
  it_M_location, meanlog = fl_mean_sdlog_acrossM, sdlog=fl_sd_sdlog_acrossM))
# random peaks 
it_mix_cnt <- 2
mt_it_peak <- matrix(sample(seq(1, it_D_y-1), it_M_location*it_mix_cnt, replace=TRUE), 
                      nrow=it_M_location, ncol=it_mix_cnt)
# print
print(paste(round(ar_meanlog_acrossM,2)))
print(paste(round(ar_sdlog_acrossM,2)))
print(mt_it_peak)


## ----------------------------------------------------------------------------------------------------------
# 1. Generate matrix to be filled
mt_Z_cone <- matrix(data=NA, nrow=it_M_location, ncol=it_D_y)
rownames(mt_Z_cone) <- paste0('m=', seq(1,it_M_location))
colnames(mt_Z_cone) <- paste0('d=', seq(1,it_D_y))

# 2. Fill matrix row by row
set.seed(456)
for (it_m in seq(1, it_M_location)){
  # Get mean and sd
  fl_meanlog <- ar_meanlog_acrossM[it_m]
  fl_sdlog <- ar_sdlog_acrossM[it_m]
  # Get peaks
  ar_it_peaks <- mt_it_peak[it_m,]
  # Generate random number, 2 x it_D_y for mixture of it_mix_cnt distributions
  mt_fl_draws <- matrix(rlnorm(it_D_y*it_mix_cnt, meanlog=fl_meanlog, sdlog=fl_sdlog), 
                        nrow=it_mix_cnt, ncol=it_D_y)
  # Resort ascending before peak, descending after peak
  for (it_mix_ctr in seq(1,it_mix_cnt)){
    it_peak <- ar_it_peaks[it_mix_ctr]
    mt_fl_draws[it_mix_ctr, 1:it_peak] <- sort(mt_fl_draws[it_mix_ctr, 1:it_peak])
    mt_fl_draws[it_mix_ctr, seq(it_peak+1, it_D_y)] <- sort(
      mt_fl_draws[it_mix_ctr, seq(it_peak+1, it_D_y)], 
      decreasing=TRUE)
  }
  # Average across mixtures, equal weights
  ar_fl_draws_mix_weighted <- colSums(mt_fl_draws)/it_mix_cnt
  # Fill matrix
  mt_Z_cone[it_m, ] <- ar_fl_draws_mix_weighted
}

# 3. tibble and display
# Combine to tibble, add name col1, col2, etc.
ar_st_varnames <- c('location', colnames(mt_Z_cone))
tb_Z_cone <- as_tibble(mt_Z_cone) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames)) %>%
  mutate(location = paste0('m=', location))

# Display
st_caption = "PM10 exposure across locations and time"
tb_Z_cone[ar_it_loc_disp,c(1,ar_it_time_disp+1)] %>%
  kable(caption = st_caption) %>% kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------
# 1. number of quantiles of interest
ar_quantiles <- c(0.2, 0.5, 0.8)
it_quantiles <- length(ar_quantiles)

# 2. Generate matrix to be filled
mt_S_moments <- matrix(data=NA, nrow=it_M_location, ncol=1+it_quantiles)
rownames(mt_S_moments) <- paste0('m=', seq(1,it_M_location))
colnames(mt_S_moments) <- c('pm_indi_mean', paste0('pm_indi_q', round(ar_quantiles*100,0)))

# 3. Compute quantiles
for (it_m in seq(1, it_M_location)){
  ar_Z <- mt_Z_cone[it_m, ]
  fl_mean <- mean(ar_Z, na.rm=TRUE)
  # note we use type=1, this uses the nearest-rank method
  ar_quant_vals <- stats::quantile(ar_Z, probs=ar_quantiles, na.rm = TRUE, type=1)
  mt_S_moments[it_m,] <- c(fl_mean, ar_quant_vals)
}


## ----------------------------------------------------------------------------------------------------------
# Column Names
ar_st_varnames <- c("locational_path", colnames(mt_S_moments))

# Combine to tibble, add name col1, col2, etc.
tb_loc_indi_dist <- as_tibble(mt_S_moments) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames)) %>%
  mutate(locational_path = paste0("locational_path=", locational_path))

# Display
st_caption = "PM10 exposure individual-moments across locations/paths"
tb_loc_indi_dist[ar_it_loc_disp,] %>%
  kable(caption = st_caption) %>% kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------
# 1. number of quantiles of interest
ar_thresholds <- seq(0, 90, by=10)
ar_thresholds <- c(0, 15, 35, 50, 75)
it_thresholds <- length(ar_thresholds)

# 2. Generate matrix to be filled
mt_S_mean_thres <- matrix(data=NA, nrow=it_M_location, ncol=it_thresholds)
rownames(mt_S_mean_thres) <- paste0('m=', seq(1,it_M_location))
colnames(mt_S_mean_thres) <- paste0('pm_indi_thr', ar_thresholds)

# 3. Compute quantiles
for (it_m in seq(1, it_M_location)){
  ar_Z <- mt_Z_cone[it_m, ]
  fl_mean <- mean(ar_Z)
  ar_mean_thres <- c()
  for (it_thres in seq(1, it_thresholds)){
    ar_Z_thres <- ar_Z
    ar_Z_thres[ar_Z < ar_thresholds[it_thres]] <- 0
    fl_mean_thres <- mean(ar_Z_thres, na.rm = T)
    ar_mean_thres <- c(ar_mean_thres, fl_mean_thres)
  }
  # note we use type=1, this uses the nearest-rank method
  mt_S_mean_thres[it_m,] <- ar_mean_thres 
}


## ----------------------------------------------------------------------------------------------------------
# Column Names
ar_st_varnames <- c("locational_path", colnames(mt_S_mean_thres))

# Combine to tibble, add name col1, col2, etc.
tb_loc_indi_dist_thres <- as_tibble(mt_S_mean_thres) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames)) %>%
  mutate(locational_path = paste0("locational_path=", locational_path))

# Display
st_caption = "PM10 exposure individual < threshold to 0 means across locations/paths"
tb_loc_indi_dist_thres[ar_it_loc_disp,] %>%
  kable(caption = st_caption) %>% kable_styling_fc_wide()

