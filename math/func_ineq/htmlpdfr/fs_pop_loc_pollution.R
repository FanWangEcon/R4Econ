## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# 7 different age groups and 12 different locationso
it_N_pop_groups <- 7
it_M_location <- 12
# Matrix of demographics by location
mt_pop_data_frac <- matrix(data=NA, nrow=it_M_location, ncol=it_N_pop_groups)  
colnames(mt_pop_data_frac) <- paste0('popgrp', seq(1,it_N_pop_groups))
rownames(mt_pop_data_frac) <- paste0('location', seq(1,it_M_location))
# Display
mt_pop_data_frac %>% kable() %>% kable_styling_fc()


## ------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------
fl_meanlog <- 3.4
fl_sdlog <- 0.35
hist(rlnorm(1000, meanlog = fl_meanlog, sdlog = fl_sdlog))


## ------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_data_frac_long <- as_tibble(mt_pop_data_frac, rownames='location') %>%
  pivot_longer(cols = starts_with('popgrp'),
               names_to = c('popgrp'),
               names_pattern = paste0("popgrp(.*)"),
               values_to = "pop_frac")


## ------------------------------------------------------------------------------------------------------
# Reshape population data, so each observation is location/demo
df_pop_pollution_long <- df_pop_data_frac_long %>%
  left_join(tb_loc_pollution, by='location')

# display
df_pop_pollution_long[1:round(it_N_pop_groups*2.5),] %>% kable() %>% kable_styling_fc()


## ------------------------------------------------------------------------------------------------------
# Follow four steps above
df_pop_pollution_by_popgrp_cdf <- df_pop_pollution_long %>%
  arrange(popgrp, avgdailypm10) %>%
  group_by(popgrp) %>%
  mutate(cdf_pop_condi_popgrp_sortpm10 = cumsum(pop_frac/sum(pop_frac)),
         pmf_pop_condi_popgrp_sortpm10 = (pop_frac/sum(pop_frac)))
# display
df_pop_pollution_by_popgrp_cdf[1:round(it_N_pop_groups*5.5),] %>% 
  kable() %>% kable_styling_fc_wide()


## ------------------------------------------------------------------------------------------------------
ffi_dist_gini_random_var_pos_test <- function(ar_x_sorted, ar_prob_of_x) {
  fl_mean <- sum(ar_x_sorted*ar_prob_of_x);
  ar_mean_cumsum <- cumsum(ar_x_sorted*ar_prob_of_x);
  ar_height <- ar_mean_cumsum/fl_mean;
  fl_area_drm <- sum(ar_prob_of_x*ar_height);
  fl_area_below45 <- sum(ar_prob_of_x*(cumsum(ar_prob_of_x)/sum(ar_prob_of_x)))
  fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45
  return(fl_gini_index)
}


## ------------------------------------------------------------------------------------------------------
# Compute GINI by group
df_pop_pollu_gini <- df_pop_pollution_by_popgrp_cdf %>%
  group_by(popgrp) %>%
  do(popgrp_gini = ffi_dist_gini_random_var_pos_test(
    .$avgdailypm10, .$pmf_pop_condi_popgrp_sortpm10)) %>%
  unnest(c(popgrp_gini)) %>%
  left_join(df_pop_pollution_by_popgrp_cdf %>% 
              group_by(popgrp) %>% slice(1L) %>% 
              select(popgrp)
              , by="popgrp")
# Display
df_pop_pollu_gini %>% kable() %>% kable_styling_fc()


## ------------------------------------------------------------------------------------------------------
# Visaulize Distributions, CDF for different population groups
lineplot <- df_pop_pollution_by_popgrp_cdf %>%
    select(popgrp, avgdailypm10, cdf_pop_condi_popgrp_sortpm10 ) %>%
    ggplot(aes(x=avgdailypm10, y=cdf_pop_condi_popgrp_sortpm10, 
               colour=popgrp)) +
        geom_line() +
        geom_point() +
        labs(title = 'CDF of Pollution Distribution by Population Types',
             x = 'Pollution Levels',
             y = 'Cumulative Probability Mass',
             caption = 'Testing Data') 
print(lineplot)


## ------------------------------------------------------------------------------------------------------
# Visaulize Distributions, CDF for different population groups
lineplot_pmf <- df_pop_pollution_by_popgrp_cdf %>%
    select(popgrp, avgdailypm10, pmf_pop_condi_popgrp_sortpm10 ) %>%
    ggplot(aes(x=avgdailypm10, y=pmf_pop_condi_popgrp_sortpm10, 
               colour=popgrp)) +
        geom_line() +
        geom_point() +
        labs(title = 'Prob Mass Func of Pollution by Population Types',
             x = 'Pollution Levels',
             y = 'Probability Mass',
             caption = 'Testing Data') 
print(lineplot_pmf)


## ------------------------------------------------------------------------------------------------------
# Generate pollution quantiles by population groups
df_pop_pollution_distribution <- df_pop_pollution_by_popgrp_cdf %>% 
  group_by(popgrp) %>%
  mutate(pm10_mean = weighted.mean(avgdailypm10, pop_frac)) %>%
  mutate(pm10_sd_gap = pop_frac*(avgdailypm10 - pm10_mean)^2, 
         pm10_sd = sqrt(weighted.mean(pm10_sd_gap, pop_frac))) %>%
  select(-pm10_sd_gap) %>%
  filter(cdf_pop_condi_popgrp_sortpm10 >= 0.10) %>%
  slice(1) %>%
  mutate(pm10_p10 = avgdailypm10) %>%
  select(popgrp, pm10_mean, pm10_sd, pm10_p10) %>%
  left_join(df_pop_pollu_gini, by='popgrp') %>% 
  left_join(df_pop_pollution_by_popgrp_cdf %>% 
              filter(cdf_pop_condi_popgrp_sortpm10 >= 0.20) %>%
              slice(1) %>%
              mutate(pm10_p20 = avgdailypm10) %>%
              select(popgrp, pm10_p20), 
            by='popgrp') %>%
  left_join(df_pop_pollution_by_popgrp_cdf %>% 
              filter(cdf_pop_condi_popgrp_sortpm10 >= 0.50) %>%
              slice(1) %>%
              mutate(pm10_p50 = avgdailypm10) %>%
              select(popgrp, pm10_p50), 
            by='popgrp') %>%
  left_join(df_pop_pollution_by_popgrp_cdf %>% 
              filter(cdf_pop_condi_popgrp_sortpm10 >= 0.80) %>%
              slice(1) %>%
              mutate(pm10_p80 = avgdailypm10) %>%
              select(popgrp, pm10_p80), 
            by='popgrp') %>%
  left_join(df_pop_pollution_by_popgrp_cdf %>% 
              filter(cdf_pop_condi_popgrp_sortpm10 >= 0.90) %>%
              slice(1) %>%
              mutate(pm10_p90 = avgdailypm10) %>%
              select(popgrp, pm10_p90), 
            by='popgrp') %>%
  select(popgrp, pm10_mean, pm10_sd, popgrp_gini, everything())
# display
df_pop_pollution_distribution %>% 
  kable(caption = 'PM10 Exposure Distribution by Population Groups') %>% 
  kable_styling_fc()

