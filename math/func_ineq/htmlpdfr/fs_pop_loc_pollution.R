## ----global_options, include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_dist_gini_random_var_pos_test <- function(ar_data, ar_prob_data) {
  #' @param ar_data array sorted array values low to high
  #' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1

  fl_mean <- sum(ar_data*ar_prob_data);
  ar_mean_cumsum <- cumsum(ar_data*ar_prob_data);
  ar_height <- ar_mean_cumsum/fl_mean;
  fl_area_drm <- sum(ar_prob_data*ar_height);
  fl_area_below45 <- sum(ar_prob_data*(cumsum(ar_prob_data)/sum(ar_prob_data)))
  fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45
  return(fl_gini_index)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Formula
ffi_atkinson_random_var_ineq <- function(ar_data, ar_prob_data, fl_rho) {
  #' @param ar_data array sorted array values
  #' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1
  #' @param fl_rho float inequality aversion parameter fl_rho = 1 for planner
  #' without inequality aversion. fl_rho = -infinity for fully inequality averse.

  fl_mean <- sum(ar_data*ar_prob_data);
  fl_atkinson <- 1 - (sum(ar_prob_data*(ar_data^{fl_rho}))^(1/fl_rho))/fl_mean
  return(fl_atkinson)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Formula
ffi_std_drv <- function(ar_data, ar_prob_data) {
  #' @param ar_data array array values
  #' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1

  fl_mean <- sum(ar_data*ar_prob_data)
  fl_std <- sqrt(sum(ar_prob_data*(ar_data - fl_mean)^2))
  return(fl_std)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Percentiles to be used for overall, across group, as well as within group calculations.
# For later purposes, what are the percentiles of interest to compute
ar_fl_percentiles <- c(0.1, 0.2, 0.8, 0.9)
# For Percentile ratios of interest, specify lower and upper bounds
ar_fl_ratio_upper <- c(0.8, 0.9)
ar_fl_ratio_lower <- c(0.2, 0.1)

# 7 different age groups and 12 different locationso
it_N_pop_groups <- 100
it_M_location <- 20
# it_N_pop_groups <- 7
# it_M_location <- 10
# Matrix of demographics by location
mt_pop_data_frac <- matrix(data=NA, nrow=it_M_location, ncol=it_N_pop_groups)
colnames(mt_pop_data_frac) <- paste0('popgrp', seq(1,it_N_pop_groups))
rownames(mt_pop_data_frac) <- paste0('location', seq(1,it_M_location))

# For succinct visualization select subset of population groups to display
it_popgrp_disp <- 7
ar_it_popgrp_disp <- seq(1, it_N_pop_groups, length.out=it_popgrp_disp)
ar_it_popgrp_disp <- round(ar_it_popgrp_disp)
ar_it_popgrp_disp_withoverall <- c(ar_it_popgrp_disp, it_N_pop_groups+1, it_N_pop_groups+2)
st_popgrp_disp <- paste0('(', it_popgrp_disp, ' of ', it_N_pop_groups, ' pop-groups shown)')
it_loc_disp <- 10
ar_it_loc_disp <- seq(1, it_M_location, length.out=it_loc_disp)
ar_it_loc_disp <- round(ar_it_loc_disp)
st_loc_disp <- paste0('(', it_loc_disp, ' of ', it_M_location, ' locations shown)')

# Display
st_caption = paste('Location and demographic cell',
  st_popgrp_disp, st_loc_disp, sep=" ")
mt_pop_data_frac[ar_it_loc_disp, ar_it_popgrp_disp] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Share of population per location
set.seed(123)
ar_p_loc <- dbinom(0:(3*it_M_location-1), 3*it_M_location-1, 0.5)
it_start <- length(ar_p_loc)/2-it_M_location/2
ar_p_loc <- ar_p_loc[it_start:(it_start+it_M_location-1)]
ar_p_loc <- ar_p_loc/sum(ar_p_loc)

# Different bernoulli "win" probability for each location
set.seed(234)
# ar_fl_unif_prob <- sort(runif(it_M_location)*(0.25)+0.4)
ar_fl_unif_prob <- sort(runif(it_M_location))

# Generate population proportion by locality
for (it_loc in 1:it_M_location ) {
  ar_p_pop_condi_loc <- dbinom(0:(it_N_pop_groups-1), it_N_pop_groups-1, ar_fl_unif_prob[it_loc])
  mt_pop_data_frac[it_loc,] <- ar_p_pop_condi_loc*ar_p_loc[it_loc]
}

# Sum of cells, should equal to 1
print(paste0('pop frac sum = ', sum(mt_pop_data_frac)))

# Display
st_caption = paste('Share of population in each location and demographic cell',
  st_popgrp_disp, st_loc_disp, sep=" ")
round((mt_pop_data_frac[ar_it_loc_disp, ar_it_popgrp_disp])*100, 3) %>%
  kable(caption=st_caption) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fl_meanlog <- 3.4
fl_sdlog <- 0.35
hist(rlnorm(1000, meanlog = fl_meanlog, sdlog = fl_sdlog))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# draw
set.seed(123)
ar_pollution_loc <- rlnorm(it_M_location, meanlog = fl_meanlog, sdlog = fl_sdlog)
# pollution dataframe
# 5 by 3 matrix

# Column Names
ar_st_varnames <- c('location','avgdailypm10')

# Combine to tibble, add name col1, col2, etc.
tb_loc_pollution <- as_tibble(ar_pollution_loc) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames)) %>%
  mutate(location = paste0('location', location))

# Display
st_caption = paste('PM10 Exposure across locations', st_loc_disp, sep=" ")
tb_loc_pollution[ar_it_loc_disp,] %>%
  kable(caption = st_caption) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_data_frac_long <- as_tibble(mt_pop_data_frac, rownames='location') %>%
  pivot_longer(cols = starts_with('popgrp'),
               names_to = c('popgrp'),
               names_pattern = paste0("popgrp(.*)"),
               values_to = "pop_frac")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_pollution_long <- df_pop_data_frac_long %>%
  left_join(tb_loc_pollution, by='location')

# display
st_caption = paste('Population x Location Long Frame (15 rows shown)', sep=" ")
df_pop_pollution_long[
  round(seq(1, dim(df_pop_pollution_long)[1], length.out=15)),] %>%
  kable(caption = st_caption) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Follow four steps above
df_pop_pollution_by_popgrp_cdf <- df_pop_pollution_long %>%
  arrange(popgrp, avgdailypm10) %>%
  group_by(popgrp) %>%
  mutate(cdf_pop_condi_popgrp_sortpm10 = cumsum(pop_frac/sum(pop_frac)),
         pmf_pop_condi_popgrp_sortpm10 = (pop_frac/sum(pop_frac)))

# Display
st_caption = paste('Distribution within groups, sorted CDFs (15 rows shown)', sep=" ")
df_pop_pollution_by_popgrp_cdf[
  round(seq(1, dim(df_pop_pollution_by_popgrp_cdf)[1], length.out=15)),] %>%
  kable(caption = st_caption) %>% kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Stats 1: excess pollution burden
df_excess_pollution_burden <- df_pop_pollution_by_popgrp_cdf %>%
  ungroup() %>%
  mutate(pm10_overall_mean = weighted.mean(avgdailypm10, pop_frac)) %>%
  group_by(popgrp) %>%
  mutate(
    popgrp_mass = sum(pop_frac), # The share of population for this group
    pm10_grp_mean = weighted.mean(avgdailypm10, pop_frac) # Pop-group mean
  ) %>%
  slice(1) %>%
  mutate(pm10_grp_exc_burden = pm10_grp_mean/pm10_overall_mean - 1) %>%
  select(popgrp, popgrp_mass,
         pm10_grp_mean, pm10_overall_mean, pm10_grp_exc_burden)
fl_pm10_overall_mean <- mean(df_excess_pollution_burden %>% pull(pm10_overall_mean))

# Stats 2: share of people within group below or above overall mean
df_share_below_or_excess <- df_pop_pollution_by_popgrp_cdf %>%
  arrange(popgrp, avgdailypm10) %>%
  filter(avgdailypm10 < fl_pm10_overall_mean) %>%
  slice_tail() %>%
  mutate(pm10_grp_shr_exc = 1 - cdf_pop_condi_popgrp_sortpm10) %>%
  select(popgrp, pm10_grp_shr_exc)
# merge stats 2 with stats 1
df_excess_pollution_burden <- df_excess_pollution_burden %>%
  left_join(df_share_below_or_excess, by="popgrp")

# display
st_caption = paste('Mean and Excess Burden by Population Groups',
  st_popgrp_disp, sep=" ")
df_excess_pollution_burden[ar_it_popgrp_disp,] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Stats 3: percentiles and ratios
# Stats 3a: generate key within group percentiles
# 1. 20th and 80th percentiles
# 2. 10th and 90th percentiles
# 3. 50th percentile
# Generate pollution quantiles by population groups
for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {

  # Current within group percentile to compute
  fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
  svr_percentile <- paste0('pm10_p', round(fl_percentile*100))

  # Frame with specific percentile
  df_within_percentiles_cur <- df_pop_pollution_by_popgrp_cdf %>%
    group_by(popgrp) %>%
    filter(cdf_pop_condi_popgrp_sortpm10 >= fl_percentile) %>%
    slice(1) %>%
    mutate(!!sym(svr_percentile) := avgdailypm10) %>%
    select(popgrp, one_of(svr_percentile))

  # Merge percentile frames together
  if (it_percentile_ctr > 1) {
    df_within_percentiles <- df_within_percentiles %>%
      left_join(df_within_percentiles_cur, by='popgrp')
  } else {
    df_within_percentiles <- df_within_percentiles_cur
  }
}

# display
st_caption = paste('PM10 Exposure Distribution by Population Groups',
  st_popgrp_disp, sep=" ")
df_within_percentiles[ar_it_popgrp_disp,] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_location_mean <- df_pop_pollution_by_popgrp_cdf %>%
  ungroup() %>%
  group_by(location) %>%
  mutate(
    location_mass = sum(pop_frac), # The share of population for this group
    pm10_mean = weighted.mean(avgdailypm10, pop_frac) # Pop-group mean, don't need this, common within group
  ) %>%  
  slice(1) %>%
  ungroup() %>%
  arrange(pm10_mean) %>%
  mutate(cdf_sortpm10 = cumsum(location_mass)) %>%
  select(location, location_mass, cdf_sortpm10, pm10_mean) %>%
  mutate(popgrp = "overall")

# display
st_caption = paste('PM10 Exposure Distribution Overall', st_loc_disp, sep=" ")
df_location_mean[ar_it_loc_disp,] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_popgrp_mean <- df_excess_pollution_burden %>%
  ungroup() %>%
  arrange(pm10_grp_mean) %>%
  mutate(cdf_sortpm10 = cumsum(popgrp_mass)) %>%
  select(popgrp, popgrp_mass, cdf_sortpm10, pm10_grp_mean) %>%
  rename(pm10_mean = pm10_grp_mean) %>%
  mutate(popgrp = "across-group")

# display
st_caption = paste('PM10 Exposure Across Groups', st_popgrp_disp, sep=" ")
df_popgrp_mean[ar_it_popgrp_disp,] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (it_df in c(1,2)) {
  for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {

    # Load in data-frames
    if (it_df == 1) {
      df_working_inputs <- df_location_mean
    } else if (it_df == 2) {
      df_working_inputs <- df_popgrp_mean
    }

    # Current within group percentile to compute
    fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
    svr_percentile <- paste0('pm10_p', round(fl_percentile*100))

    # Frame with specific percentile
    df_within_percentiles_cur <- df_working_inputs %>%
      filter(cdf_sortpm10 >= fl_percentile) %>%
      slice(1) %>%
      mutate(!!sym(svr_percentile) := pm10_mean) %>%
      select(popgrp, one_of(svr_percentile))

    # Merge percentile frames together
    if (it_percentile_ctr > 1) {
      df_percentiles <- df_percentiles %>%
        left_join(df_within_percentiles_cur, by='popgrp')
    } else {
      df_percentiles <- df_within_percentiles_cur
    }
  }
  
  if (it_df == 1) {
    df_location_mean_perc <- df_percentiles
  } else if (it_df == 2) {
    df_popgrp_mean_perc <- df_percentiles
  }

}

# Stack results together

# display
st_caption = paste('Overall PM10 Distribution')
df_location_mean_perc %>%
  kable(caption = st_caption) %>%
  kable_styling_fc()

st_caption = paste('Across Population Group PM10 Distribution')
df_popgrp_mean_perc %>%
  kable(caption = st_caption) %>%
  kable_styling_fc()  


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Percentiles and excess burden data combined
df_excburden_percentiles <- df_excess_pollution_burden %>%
  left_join(df_within_percentiles, by="popgrp")
# Overall and Across Group percentiles
df_excburden_percentiles <- bind_rows(df_excburden_percentiles, df_location_mean_perc, df_popgrp_mean_perc)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# merge stats 3 with stats 1 and 2
df_excess_pollution_burden <- df_excess_pollution_burden %>%
  left_join(df_within_percentiles, by="popgrp")

# Stats 3b: Percentiles to Relative Burdens
# Convert percentiles to be relative of overall means
for (it_percentile_ctr in seq(1, length(ar_fl_percentiles))) {

  # Current within group percentile to compute
  fl_percentile <- ar_fl_percentiles[it_percentile_ctr]
  svr_percentile <- paste0('pm10_p', round(fl_percentile*100))
  svr_perc_exc_burden <- paste0('pm10_grp_excbrd_p', round(fl_percentile*100))

  # Percentiles to excess percentiles
  df_excess_pollution_burden  <- df_excess_pollution_burden  %>%
    mutate(!!sym(svr_perc_exc_burden) := (!!sym(svr_percentile)/pm10_overall_mean) - 1)
}

# display
st_caption = paste('PM10 Within Population Group Percentiles and Excess Burden',
  st_popgrp_disp, sep=" ")
df_excess_pollution_burden[ar_it_popgrp_disp,] %>%
  select(-pm10_overall_mean,
         -starts_with('pm10_p')) %>%
  kable(caption = st_caption) %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lower and upper bound or relative within group ratios
# can only use values appearing in the percentiles list prior
# Stats 4c: Ratios
# Generate P80 to P20 ratio, and P90 to P10 standard inequality ratios
for (it_ratio_ctr in seq(1, length(ar_fl_ratio_upper))) {

  # Upper and lower percentile bounds
  fl_ratio_upper <- ar_fl_ratio_upper[it_ratio_ctr]
  fl_ratio_lower <- ar_fl_ratio_lower[it_ratio_ctr]
  svr_ratio_upper_perc <- paste0('pm10_p', round(fl_ratio_upper*100))
  svr_ratio_lower_perc <- paste0('pm10_p', round(fl_ratio_lower*100))

  # New relative within group ratio variable name
  svr_ratio <- paste0('pm10_rat_p', round(fl_ratio_upper*100), '_dvd_p', round(fl_ratio_lower*100))

  # Generate P80 to P20 ratio, etc.
  df_excburden_percentiles  <- df_excburden_percentiles  %>%
    mutate(!!sym(svr_ratio) := !!sym(svr_ratio_upper_perc)/!!sym(svr_ratio_lower_perc))
}

# display
st_caption = paste('PM10 Exposure within/across/overall Population Group P80-P20 Inequality',
  st_popgrp_disp, sep=" ")
df_excburden_percentiles[ar_it_popgrp_disp_withoverall,] %>%
  select(-pm10_overall_mean,
         -starts_with('pm10_grp_excbrd_p'),
         -starts_with('pm10_p')) %>%
  kable(caption = st_caption) %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Rounding excess burden s.d.
df_scatter_main <- df_excburden_percentiles %>% 
  filter(!popgrp %in% c("overall", "across-group")) 
fl_pm10_rat_p80_dvd_p20_overall <- mean(df_excburden_percentiles %>%
  filter(popgrp %in% c("overall")) %>% pull(pm10_rat_p80_dvd_p20))
fl_pm10_rat_p80_dvd_p20_acrossgrp <- mean(df_excburden_percentiles %>%
  filter(popgrp %in% c("across-group")) %>% pull(pm10_rat_p80_dvd_p20))
st_title <- paste0("Relative Percentile Ratios and Excess Burdens")
# title_line1 <- paste0("Histogram shows the distribution of Relative Ratios")

# Generate a Data Sample by Drawing from the Distribution
it_sample_draws <- 1e6
# ar_it_draws <- sample(1:it_N_pop_groups, it_sample_draws, replace=TRUE, prob=ar_data_grp_shares)
# ar_sample_draws <- ar_data_grp_exc_burden[ar_it_draws]
# Draw histogram
pl_excess_burden <- df_scatter_main %>%
  ggplot(aes(x=pm10_grp_exc_burden, 
             y=pm10_rat_p80_dvd_p20)) +
  geom_jitter(aes(size=popgrp_mass, color=pm10_grp_mean), width = 0.15, size=2) +
  geom_smooth(span = 0.50, se=TRUE) +
  theme_bw() + 
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=2) +
  geom_hline(aes(yintercept=fl_pm10_rat_p80_dvd_p20_overall, 
                 linetype="Overall"), 
             color="red", size=2) +
  geom_hline(aes(yintercept=fl_pm10_rat_p80_dvd_p20_acrossgrp, 
                 linetype="Across-groups"), 
             color="green", size=2) +
  labs(title = st_title,
       # subtitle = paste0(title_line1),
       x = 'Excess Pollution Burden  = (Pollution Share)/(Pop Share) - 1',
       y = 'P80 to P20 Ratios',
       caption = 'Based on simulated random data for testing.') + 
  scale_linetype_manual(name = "", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("red", "green"))))
  
# Print
print(pl_excess_burden)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. SORT FIRST!
df_excess_pollution_burden_sorted <- df_excess_pollution_burden %>% arrange(pm10_grp_mean)
# 2. Obtain the means across groups, and also excess burden across groups
ar_data_grp_means <- df_excess_pollution_burden_sorted %>% pull(pm10_grp_mean)
ar_data_grp_exc_burden <- df_excess_pollution_burden_sorted %>% pull(pm10_grp_exc_burden)
# 3. Obtain the probability mass for each group
ar_data_grp_shares <- df_excess_pollution_burden_sorted %>% pull(popgrp_mass)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# compute gini over group means, and standard deviations
fl_grp_means_gini <- ffi_dist_gini_random_var_pos_test(ar_data_grp_means, ar_data_grp_shares)
# STD
fl_grp_means_std <- ffi_std_drv(ar_data_grp_means, ar_data_grp_shares)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Log10 scaled Inequality Measures
ar_rho <- 1 - (10^(c(seq(-2,2, length.out=30))))
tb_rho <- as_tibble(unique(ar_rho))
# Array of atkinson values
ar_grp_means_atkinson <- apply(tb_rho, 1, function(row){
  ffi_atkinson_random_var_ineq(ar_data_grp_means, ar_data_grp_shares, row[1])})
# atkinson results table
ar_st_varnames <- c('id','rho','atkinson_index')
tb_atkinson <- as_tibble(cbind(ar_rho, ar_grp_means_atkinson)) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames)) %>%
  mutate(one_minus_rho = 1 - rho)
# Max Atkinson
fl_atkinson_max <- max(tb_atkinson %>% pull(atkinson_index))
# display
it_rows_shown <- 10
st_caption <- paste0('Atkinson Inequality Index',
  '(', it_rows_shown, ' of ', length(ar_rho), ' inequality preferences shown)')
tb_atkinson[round(seq(1, length(ar_rho), length.out = it_rows_shown)),] %>%
  kable(caption = st_caption) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fl_grp_exc_burden_std <- ffi_std_drv(ar_data_grp_exc_burden, ar_data_grp_shares)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# x-labels
x.labels <- c('lambda=0.99', 'lambda=0.90', 'lambda=0', 'lambda=-10', 'lambda=-100')
x.breaks <- c(0.01, 0.10, 1, 10, 100)

# title line 2
fl_grp_means_gini_fmt <- round(fl_grp_means_gini, 3)
st_title <- paste0("Inequality Over Group Means (GINI=", fl_grp_means_gini_fmt," and ATKINSON)")
title_line1 <- paste0("The literature computes group means and computes Atkinson Index over group means")
title_line2 <- paste0("BLACK = Atkinson index's values differ depending on inequality aversion (x-axis)")
title_line3 <- paste0("RED = GINI (", fl_grp_means_gini_fmt,") index has a fixed value")

# Graph Results--Draw
pl_gini_atkinson <- tb_atkinson %>%
  ggplot(aes(x=one_minus_rho, y=atkinson_index)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=fl_grp_means_gini, linetype='dashed', color='red', size=2) +
  # geom_vline(xintercept=c(1), linetype="dotted") +
  labs(title = st_title,
       subtitle = paste0(title_line1, '\n', title_line2, '\n', title_line3),
       x = 'log10 Rescale of lambda, Log10(1-lambda)\nlambda=1 Utilitarian (Cares about mean pollution only), lambda=-infty Rawlsian (Cares about inequality only)',
       y = paste0('GINI and Atkinson Index, 0 to 1, 0 = perfect equality'),
       caption = 'Based on simulated random data for testing.') +
  scale_x_continuous(trans='log10', labels = x.labels, breaks = x.breaks) +
  ylim(0, max(min(fl_atkinson_max*1.1, 1), fl_grp_means_gini)) +
  theme(text = element_text(size = 10),
        legend.position="right")

# Print
print(pl_gini_atkinson)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Rounding excess burden s.d.
fl_grp_exc_burden_std_fmt <- round(fl_grp_exc_burden_std, 3)
st_title <- paste0("Distribution of Excess Burden (Across Group Variation), s.d.=", fl_grp_exc_burden_std_fmt)
title_line1 <- paste0("Histogram shows the distribution of excess burden by population groups")
title_line2 <- paste0("Excess Burden = (Pollution Share)/(Pop Share) - 1")

# Generate a Data Sample by Drawing from the Distribution
it_sample_draws <- 1e6
ar_it_draws <- sample(1:it_N_pop_groups, it_sample_draws, replace=TRUE, prob=ar_data_grp_shares)
ar_sample_draws <- ar_data_grp_exc_burden[ar_it_draws]
# Draw histogram
pl_excess_burden <- as_tibble(ar_sample_draws) %>%
  ggplot(aes(x=value)) +
  # geom_histogram(aes(y=..density..),
  #                colour="darkblue", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=0),
             color="blue", linetype="dashed", size=2) +
  labs(title = st_title,
       subtitle = paste0(title_line1, '\n', title_line2),
       x = 'Excess Pollution Burden  = (Pollution Share)/(Pop Share) - 1',
       y = 'Density',
       caption = 'Based on simulated random data for testing.')
# Print
print(pl_excess_burden)

