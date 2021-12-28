## ----global_options, include = FALSE------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------
# 7 different age groups and 12 different locationso
it_N_pop_groups <- 7
it_M_location <- 12
# Matrix of demographics by location
mt_pop_data_frac <- matrix(data=NA, nrow=it_M_location, ncol=it_N_pop_groups)
colnames(mt_pop_data_frac) <- paste0('popgrp', seq(1,it_N_pop_groups))
rownames(mt_pop_data_frac) <- paste0('location', seq(1,it_M_location))
# Display
mt_pop_data_frac %>% kable() %>% kable_styling_fc()


## -----------------------------------------------------------------------------------------------------
# Share of population per location
set.seed(123)
ar_p_loc <- dbinom(0:(3*it_M_location-1), 3*it_M_location-1, 0.5)
it_start <- length(ar_p_loc)/2-it_M_location/2
ar_p_loc <- ar_p_loc[it_start:(it_start+it_M_location+1)]
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
round(mt_pop_data_frac*100, 2) %>%
  kable(caption='Share of population in each location and demographic cell') %>%
  kable_styling_fc()


## -----------------------------------------------------------------------------------------------------
fl_meanlog <- 3.4
fl_sdlog <- 0.35
hist(rlnorm(1000, meanlog = fl_meanlog, sdlog = fl_sdlog))


## -----------------------------------------------------------------------------------------------------
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
kable(tb_loc_pollution) %>% kable_styling_fc()


## -----------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_data_frac_long <- as_tibble(mt_pop_data_frac, rownames='location') %>%
  pivot_longer(cols = starts_with('popgrp'),
               names_to = c('popgrp'),
               names_pattern = paste0("popgrp(.*)"),
               values_to = "pop_frac")


## -----------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_pollution_long <- df_pop_data_frac_long %>%
  left_join(tb_loc_pollution, by='location')

# display
df_pop_pollution_long[1:round(it_N_pop_groups*2.5),] %>% kable() %>% kable_styling_fc()


## -----------------------------------------------------------------------------------------------------
# Follow four steps above
df_pop_pollution_by_popgrp_cdf <- df_pop_pollution_long %>%
  arrange(popgrp, avgdailypm10) %>%
  group_by(popgrp) %>%
  mutate(cdf_pop_condi_popgrp_sortpm10 = cumsum(pop_frac/sum(pop_frac)),
         pmf_pop_condi_popgrp_sortpm10 = (pop_frac/sum(pop_frac)))
# display
df_pop_pollution_by_popgrp_cdf[1:round(it_N_pop_groups*5.5),] %>%
  kable() %>% kable_styling_fc_wide()


## -----------------------------------------------------------------------------------------------------
# Stats 1: excess pollution burden
df_excess_pollution_burden <- df_pop_pollution_by_popgrp_cdf %>%
  ungroup() %>%
  mutate(pm10_overall_mean = weighted.mean(avgdailypm10, pop_frac)) %>%
  group_by(popgrp) %>%
  mutate(pm10_grp_mean = weighted.mean(avgdailypm10, pop_frac)) %>%
  slice(1) %>%
  mutate(pm10_grp_exc_burden = pm10_grp_mean/pm10_overall_mean - 1) %>%
  select(popgrp, pm10_grp_mean, pm10_overall_mean, pm10_grp_exc_burden)
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
df_excess_pollution_burden %>%
  kable(caption = 'PM10 Exposure Distribution by Population Groups') %>%
  kable_styling_fc()


## -----------------------------------------------------------------------------------------------------
# Stats 3: percentiles and ratios
ar_fl_percentiles <- c(0.1, 0.2, 0.8, 0.9)
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
df_within_percentiles %>%
  kable(caption = 'PM10 Exposure Distribution by Population Groups') %>%
  kable_styling_fc()



## -----------------------------------------------------------------------------------------------------
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
      mutate(!!sym(svr_perc_exc_burden) := !!sym(svr_percentile)/pm10_overall_mean)
}

# display
df_excess_pollution_burden %>%
  select(-pm10_overall_mean,
         -starts_with('pm10_p')) %>%
  kable(caption = 'PM10 Exposure Distribution by Population Groups') %>%
  kable_styling_fc_wide()


## -----------------------------------------------------------------------------------------------------
# lower and upper bound or relative within group ratios
# can only use values appearing in the percentiles list prior
ar_fl_ratio_upper <- c(0.8, 0.9)
ar_fl_ratio_lower <- c(0.2, 0.1)
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
    df_excess_pollution_burden  <- df_excess_pollution_burden  %>%
      mutate(!!sym(svr_ratio) := !!sym(svr_ratio_upper_perc)/!!sym(svr_ratio_lower_perc))
}

# display
df_excess_pollution_burden %>%
  select(-pm10_overall_mean,
         -starts_with('pm10_grp_excbrd_p'),
         -starts_with('pm10_p')) %>%
  kable(caption = 'PM10 Exposure Distribution by Population Groups') %>%
  kable_styling_fc_wide()

