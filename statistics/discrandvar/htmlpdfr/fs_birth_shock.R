## ----global_options, include = FALSE----------------------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This function generates the distribution of gestational weeks at birth
ffi_gestation_age_at_birth_dist <- function(
  mu_gabirth_days = 276,
  sd_gabirth_days = 14
){
  # # Parameters
  # # gabirth = gestational age at birth
  # mu_gabirth_days <- 276
  # sd_gabirth_days <- 14
  # from https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_disc_approx_cts.html
  it_binom_n <- round((mu_gabirth_days / 7)^2 / (mu_gabirth_days / 7 - (sd_gabirth_days / 7)^2))
  fl_binom_p <- 1 - (sd_gabirth_days / 7)^2 / (mu_gabirth_days / 7)

  # Same graphing code as from: https://fanwangecon.github.io/Stat4Econ/probability_discrete/htmlpdfr/binomial.html#24_binomial_example:_wwii_german_soldier
  # Generate Data
  ar_grid_gabirth <- 0:it_binom_n
  ar_pdf_gabirth <- dbinom(ar_grid_gabirth, it_binom_n, fl_binom_p)
  ar_cdf_gabirth <- pbinom(ar_grid_gabirth, it_binom_n, fl_binom_p)
  df_dist_gabirth <- tibble(gabirth = (ar_grid_gabirth), prob = ar_pdf_gabirth, cum_prob = ar_cdf_gabirth)

  # Two axis colors
  axis_sec_ratio <- max(ar_cdf_gabirth) / max(ar_pdf_gabirth)
  right_axis_color <- "blue"
  left_axis_color <- "red"

  # Probabilities
  plt_dist_gabirth <- df_dist_gabirth %>%
    ggplot(aes(x = gabirth)) +
    geom_bar(aes(y = prob),
      stat = "identity", alpha = 0.5, width = 0.5, fill = left_axis_color
    )

  # Cumulative Probabilities
  plt_dist_gabirth <- plt_dist_gabirth +
    geom_line(aes(y = cum_prob / axis_sec_ratio),
      alpha = 0.75, size = 1, color = right_axis_color
    )

  # Titles Strings etc
  graph_title <- paste0("Gestational age at birth (weeks)\n",
      "Prob mass (Left) and cumulative prob (Right)")
  graph_caption <- paste0("Assuming the binomial properties apply\n",
      "fl_binom_p = ", fl_binom_p, ", it_binom_n = ", it_binom_n)
  graph_title_x <- paste0("Gestational age at birth (weeks)\n",
    "mean gestational age (days) at birth = ", mu_gabirth_days, "\n",
    "standard deviation of g.a. (days) at birth = ", sd_gabirth_days)
  graph_title_y_axisleft <- "Prob x weeks of gestation"
  graph_title_y_axisright <- "Prob at most x weeks of gestation"

  # Titles etc
  plt_dist_gabirth <- plt_dist_gabirth +
    labs(
      title = graph_title,
      x = graph_title_x,
      y = graph_title_y_axisleft,
      caption = graph_caption
    ) +
    scale_y_continuous(
      sec.axis =
        sec_axis(~ . * axis_sec_ratio, name = graph_title_y_axisright)
    ) +
    scale_x_continuous(
      labels = ar_grid_gabirth[floor(seq(1, it_binom_n, length.out = 10))],
      breaks = ar_grid_gabirth[floor(seq(1, it_binom_n, length.out = 10))]
    ) +
    theme(
      axis.text.y = element_text(face = "bold"),
      axis.text.y.right = element_text(color = right_axis_color),
      axis.text.y.left = element_text(color = left_axis_color)
    )

  # Print
  return(list(
    df_dist_gabirth=df_dist_gabirth,
    plt_dist_gabirth=plt_dist_gabirth
  ))
}
# Test the function
ls_gsbirth <- ffi_gestation_age_at_birth_dist(mu_gabirth_days = 276, sd_gabirth_days = 14)
# Figure
print(ls_gsbirth$plt_dist_gabirth)
# Table
df_dist_gabirth <- ls_gsbirth$df_dist_gabirth
kable(df_dist_gabirth %>% filter(prob >= 0.01)) %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This function generates the distribution of conception weeks across
# the year, with two peak months, and some randomness
ffi_concept_distribution_year <- function(it_max_weeks = 52,
                                          it_peak_wk_1st = 15,
                                          it_peak_wk_2nd = 40,
                                          fl_binom_1st_wgt = 0.150,
                                          fl_binom_2nd_wgt = 0.075,
                                          it_runif_seed = 123) {
  # # Peak (local) months and weights
  # it_max_weeks <- 52
  # it_peak_wk_1st <- 15
  # it_peak_wk_2nd <- 40

  # # Weights for the two binomial and the remaining weight is for an uniform distribution
  # fl_binom_1st_wgt <- 0.25
  # fl_binom_2nd_wgt <- 0.10

  # Discrete random variables
  ar_fl_binom_1st <- dbinom(
    0:(it_max_weeks - 1), (it_max_weeks - 1),
    (it_peak_wk_1st - 1) / (it_max_weeks - 1)
  )
  ar_fl_binom_2nd <- dbinom(
    0:(it_max_weeks - 1), (it_max_weeks - 1),
    (it_peak_wk_2nd - 1) / (it_max_weeks - 1)
  )
  set.seed(it_runif_seed)
  ar_random_base <- runif(it_max_weeks, min = 0.5, max = 1)
  ar_random_base <- ar_random_base / sum(ar_random_base)

  # Mix two binomials and a uniform
  ar_fl_p_concept_week <- ar_fl_binom_1st * fl_binom_1st_wgt +
    ar_fl_binom_2nd * fl_binom_2nd_wgt +
    ar_random_base * (1 - fl_binom_1st_wgt - fl_binom_2nd_wgt)

  # Dataframe
  df_dist_conception <- tibble(conception_calendar_week = 1:it_max_weeks,
                               conception_prob = ar_fl_p_concept_week)

  # Line plot
  # Title
  st_title <- paste0(
      "Distribution of conception month of birth\n",
      "over weeks of one specific year, seed=", it_runif_seed
    )

  # Display
  plt_concept_week_of_year <- df_dist_conception %>%
    ggplot(aes(x = conception_calendar_week, y= conception_prob)) +
    geom_line() +
    labs(
      title = st_title,
      x = 'Weeks of year',
      y = 'Share of conception this week'
      ) +
    scale_x_continuous(n.breaks = 12) +
    scale_y_continuous(n.breaks = 10) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
    )

  # Return
  return(list(
    df_dist_conception = df_dist_conception,
    plt_concept_week_of_year = plt_concept_week_of_year
  ))
}
# Call function with defaults
ls_concept <- ffi_concept_distribution_year(it_max_weeks = 52,
                                            it_peak_wk_1st = 15,
                                            it_peak_wk_2nd = 40,
                                            it_runif_seed = 123)
ls_concept$plt_concept_week_of_year
df_dist_conception <- ls_concept$df_dist_conception
kable(df_dist_conception) %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_daily_temp_simulation <- function(
  fl_mthly_mean_lowest = 57,
  fl_mthly_mean_highest = 84,
  fl_record_lowest = 32,
  fl_record_highest = 102.4,
  it_weeks_in_year = 52,
  it_days_in_week = 7,
  it_years = 6,
  fl_mu = 4.15,
  fl_sin_scaler = 0.20,
  fl_sigma_nv = 0.15,
  fl_rho_persist = 0.40,
  it_rand_seed = 123,
  st_extreme_cold_percentile = "p05",
  st_extreme_heat_percentile = "p95"
){
  # # Guangzhou temp info
  # fl_mthly_mean_lowest <- 57
  # fl_mthly_mean_highest <- 84
  # fl_record_lowest <- 32
  # fl_record_highest <- 102.4

  # # Total number of periods (over three years)
  # it_weeks_in_year <- 52
  # it_days_in_week <- 7
  # it_years <- 6
  it_max_days <- it_weeks_in_year*it_days_in_week
  T <- it_max_days * it_years

  # # Mean temp
  # fl_mu <- 4.15
  # fl_sin_scaler <- 0.20
  # # AR 1 parameter
  # fl_sigma_nv <- 0.15
  # fl_rho_persist <- 0.40

  # Generate a vector of shocks
  set.seed(it_rand_seed)
  ar_nv_draws <- rnorm(T, mean = 0, sd = fl_sigma_nv)

  # Generate a vector of epsilons
  ar_epsilon_ar1 <- vector("double", length=T)
  ar_epsilon_ar1[1] <- ar_nv_draws[1]
  for (it_t in 2:T) {
    ar_epsilon_ar1[it_t] <- fl_rho_persist*ar_epsilon_ar1[it_t-1] + ar_nv_draws[it_t]
  }

  # Generate week by week sin curve values
  ar_day_at_t <- rep(1:it_max_days, it_years)
  ar_year_at_t <- as.vector(t(matrix(data=rep(1:it_years, it_max_days),
                                     nrow=it_years, ncol=it_max_days)))
  ar_base_temp <- sin((ar_day_at_t/it_max_days)*2*pi + ((3-1/6)/2)*pi)

  # Generate overall temperature in Fahrenheit
  ar_fahrenheit_city_over_t <-
    exp(fl_mu + ar_epsilon_ar1 + fl_sin_scaler*ar_base_temp)

  # Dataframe with Temperatures
  mt_fahrenheit_info <- cbind(ar_day_at_t, ar_year_at_t, ceiling(ar_day_at_t/it_days_in_week),
    ar_fahrenheit_city_over_t,
    exp(ar_base_temp), ar_epsilon_ar1, ar_nv_draws)
  ar_st_varnames <- c('survey_t','day_of_year', 'year', 'week_of_year',
    'Fahrenheit', 'FnoShock', 'AR1Shock', 'RandomDraws')

  # Combine to tibble, add name col1, col2, etc.
  df_fahrenheit <- as_tibble(mt_fahrenheit_info) %>%
    rowid_to_column(var = "t") %>%
    rename_all(~c(ar_st_varnames))

  # Generate extreme temperatures
  df_stats_fahrenheit <- REconTools::ff_summ_percentiles(df_fahrenheit, FALSE)
  # Add Extreme Thresholds
  fl_lowF_threshold <- df_stats_fahrenheit %>% filter(var == "Fahrenheit") %>% pull(st_extreme_cold_percentile)
  fl_highF_threshold <- df_stats_fahrenheit %>% filter(var == "Fahrenheit") %>% pull(st_extreme_heat_percentile)
  df_fahrenheit <- df_fahrenheit %>%
    mutate(extreme_cold = case_when(Fahrenheit <= fl_lowF_threshold ~ 1, TRUE ~ 0)) %>%
    mutate(extreme_hot = case_when(Fahrenheit >= fl_highF_threshold ~ 1, TRUE ~ 0))
  # REconTools::ff_summ_percentiles(df_fahrenheit, FALSE)

  # Title
  st_title <- paste0('Simulated Temperature for Guangzhou (Sine Wave + AR(1))\n',
      'Each subplot is a different year\n',
      'RED = Guangzhou Temp 1971–2000 lowest and highest monthly averages\n',
      'BLUE = Guangzhou Temp 1961–2000 record lows and highs')

  # Display
  plt_fahrenheit <- df_fahrenheit %>%
    ggplot(aes(x = ar_day_at_t, y=Fahrenheit)) +
    geom_line() +
    geom_hline(yintercept = fl_mthly_mean_lowest, linetype = "solid", colour = "red", size = 1) +
    geom_hline(yintercept = fl_mthly_mean_highest, linetype = "solid", colour = "red", size = 1) +
    geom_hline(yintercept = fl_record_lowest, linetype = "dashed", colour = "blue", size = 1) +
    geom_hline(yintercept = fl_record_highest, linetype = "dashed", colour = "blue", size = 1) +
    facet_wrap(~ year) +
    labs(
      title = st_title,
      x = 'Calendar day in year',
      y = 'Temperature in Fahrenheit'
      ) +
    scale_x_continuous(n.breaks = 12) +
    scale_y_continuous(n.breaks = 10) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
    )

  # Return
  return(list(
    df_fahrenheit = df_fahrenheit,
    plt_fahrenheit = plt_fahrenheit
  ))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Test 1: Call function with AR1 + Since Curve
ls_fahrenheit <- ffi_daily_temp_simulation(
  fl_mthly_mean_lowest = 57,
  fl_mthly_mean_highest = 84,
  fl_record_lowest = 32,
  fl_record_highest = 102.4,
  it_weeks_in_year = 52,
  it_days_in_week = 7,
  it_years = 2,
  fl_mu = 4.15,
  fl_sin_scaler = 0.25,
  fl_sigma_nv = 0.15,
  fl_rho_persist = 0.70,
  it_rand_seed = 123)
print(ls_fahrenheit$plt_fahrenheit)
# df_fahrenheit <- ls_fahrenheit$df_fahrenheit
# kable(df_fahrenheit) %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Test 2: Call function with defaults
ls_fahrenheit <- ffi_daily_temp_simulation(
  fl_mthly_mean_lowest = 57,
  fl_mthly_mean_highest = 84,
  fl_record_lowest = 32,
  fl_record_highest = 102.4,
  it_weeks_in_year = 52,
  it_days_in_week = 7,
  it_years = 2,
  fl_mu = 4.15,
  fl_sin_scaler = 0.25,
  fl_sigma_nv = 0.15,
  fl_rho_persist = 0.0,
  it_rand_seed = 123)
# Show
print(ls_fahrenheit$plt_fahrenheit)
# df_fahrenheit <- ls_fahrenheit$df_fahrenheit
# kable(df_fahrenheit) %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Test 2: Call function with defaults
ls_fahrenheit <- ffi_daily_temp_simulation(
  fl_mthly_mean_lowest = 57,
  fl_mthly_mean_highest = 84,
  fl_record_lowest = 32,
  fl_record_highest = 102.4,
  it_weeks_in_year = 52,
  it_days_in_week = 7,
  it_years = 2,
  fl_mu = 4.15,
  fl_sin_scaler = 0.25,
  fl_sigma_nv = 0.025,
  fl_rho_persist = 0.0,
  it_rand_seed = 123)
# Show
print(ls_fahrenheit$plt_fahrenheit)
# df_fahrenheit <- ls_fahrenheit$df_fahrenheit
# kable(df_fahrenheit) %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_pop_concept_birth_simu <- function(
  it_pop_n = 50000,
  it_days_in_week = 7,
  it_weeks_in_month = 4,
  it_months_in_year = 12,
  it_years = 3,
  it_rng_seed = 123,
  fl_pre_term_ratio = 0.84,
  fl_peak_concept_frac_of_year_1st = 0.3,
  fl_peak_concept_frac_of_year_2nd = 0.9,
  fl_binom_1st_wgt = 0.15,
  fl_binom_2nd_wgt = 0.05,
  fl_mu_gabirth_days_365 = 276,
  fl_sd_gabirth_days_365 = 14
) {

  # # 1. Define parameters
  # # 1.a Number of individuals of interest
  # it_pop_n <- 50000
  # # 1.b Number of days per week, week per month
  # # for simplicity, 7 days per week, 4 weeks per month, 12 months
  # it_days_in_week <- 7
  # it_weeks_in_month <- 4
  # it_months_in_year <- 12
  # it_years <- 3
  # # 1.c random draw seed
  # it_rng_seed <- 123
  # # 1.d pre-term threshold
  # # what fraction of maximum pregnancy length is pre-term?
  # # Max length 44 weeks for example, 44*0.85 is about 37 months.
  # fl_pre_term_ratio <- 0.84

  # 1.e Other dates
  it_weeks_in_year <- it_weeks_in_month*it_months_in_year
  it_days_in_month <- it_days_in_week*it_weeks_in_month
  it_days_in_year <- it_days_in_week*it_weeks_in_month*it_months_in_year
  # 1.f Month of conception distribution
  it_peak_wk_1st <- round(it_weeks_in_year*fl_peak_concept_frac_of_year_1st)
  it_peak_wk_2nd <- round(it_weeks_in_year*fl_peak_concept_frac_of_year_2nd)
  # fl_binom_1st_wgt <- 0.15
  # fl_binom_2nd_wgt <- 0.05
  # # 1.g Gestational age at birth distribution parameters
  mu_gabirth_days <- round((fl_mu_gabirth_days_365/365)*it_days_in_year)
  sd_gabirth_days <- round((fl_sd_gabirth_days_365/365)*it_days_in_year)

  # 2. Date of conception random draws
  # 2.a Week of conception distribution
  ls_concept_fc <- ffi_concept_distribution_year(
    it_max_weeks = it_weeks_in_year,
    it_peak_wk_1st = it_peak_wk_1st, it_peak_wk_2nd = it_peak_wk_2nd,
    fl_binom_1st_wgt = fl_binom_1st_wgt, fl_binom_2nd_wgt = fl_binom_2nd_wgt,
    it_runif_seed = it_rng_seed*210)
  df_dist_conception_week <- ls_concept_fc$df_dist_conception
  # 2.b.1 Randomly (uniformly) drawing the day of birth
  set.seed(it_rng_seed*221)
  ar_draws_conception_day_of_week <- sample(
    it_days_in_week, it_pop_n, replace=TRUE)
  # 2.b.2 Week of conception draws
  set.seed(it_rng_seed*222)
  ar_draws_conception_week <- sample(
    df_dist_conception_week$conception_calendar_week,
    it_pop_n,
    prob=df_dist_conception_week$conception_prob,
    replace=TRUE)
  # 2.b.3 Randomly (uniformly) drawing the year of conception
  set.seed(it_rng_seed*223)
  ar_draws_conception_year <- sample(
    it_years, it_pop_n, replace=TRUE)
  # 2.c Date of birth
  ar_draws_concept_date <- (ar_draws_conception_year-1)*it_days_in_year +
    (ar_draws_conception_week-1)*it_days_in_week +
    ar_draws_conception_day_of_week
  ar_draws_conception_day_of_year <- (ar_draws_conception_week-1)*it_days_in_week +
    ar_draws_conception_day_of_week

  # 3. Gestational age at birth distribution simulation
  # 3.a Gestational age distribution
  ls_gsbirth_fc <- ffi_gestation_age_at_birth_dist(
    mu_gabirth_days = mu_gabirth_days, sd_gabirth_days = sd_gabirth_days)
  df_dist_gabirth <- ls_gsbirth_fc$df_dist_gabirth
  # 3.b.1 Gestational day of week draws (random)
  set.seed(it_rng_seed*321)
  ar_draws_gsbirth_day_of_week <- sample(
    it_days_in_week, it_pop_n, replace=TRUE)
  # 3.b.2 Gestational week draws
  set.seed(it_rng_seed*322)
  ar_draws_gsbirth_week <- sample(
    df_dist_gabirth$gabirth,
    it_pop_n,
    prob=df_dist_gabirth$prob,
    replace=TRUE)
  # 3.c Gestational days at birth
  ar_draws_gsbirth_day <- ar_draws_gsbirth_week*it_days_in_week + ar_draws_gsbirth_day_of_week
  ar_draws_birth_date <- ar_draws_concept_date + ar_draws_gsbirth_day

  # 4. Create dataframe
  # 4.a Variables and labels
  mt_birth_data <- cbind(
    ar_draws_concept_date, ar_draws_birth_date, ar_draws_gsbirth_day,
    ar_draws_conception_year, ar_draws_conception_week,
    ar_draws_conception_day_of_week, ar_draws_conception_day_of_year,
    ar_draws_gsbirth_week, ar_draws_gsbirth_day_of_week)
  ar_st_varnames <- c('id',
    'survey_date_conception', 'survey_date_birth', 'gestation_length_in_days',
    'conception_year', 'conception_week',
    'conception_day_of_week', 'concept_day_of_year',
    'gestational_week_at_birth', 'gestational_day_of_week_at_birth')
  # 4.b tibble with conception and birth data
  df_birth_data <- as_tibble(mt_birth_data) %>%
    rowid_to_column(var = "id") %>%
    rename_all(~c(ar_st_varnames)) %>%
    arrange(survey_date_conception, survey_date_birth)
  # 4.c generate cut-off for preterm
  it_pre_term_threshold <- round(fl_pre_term_ratio*length(df_dist_gabirth$gabirth)*it_days_in_week)
  df_birth_data <- df_birth_data %>% mutate(
    preterm = case_when(it_pre_term_threshold >= gestation_length_in_days ~ 1,
                        TRUE ~ 0) )

  # 5. Display data
  # Display
  st_title <- paste0('Day of year of conception and gestational age at birth\n',
    'pop=', it_pop_n, ', days-in-year=', it_days_in_year, ', seed=', it_rng_seed, '\n',
    'mean-ga-at-birth-in-month=', round(mu_gabirth_days/it_days_in_month, 3),
    ', sd-ga-at-birth-in-month=', round(sd_gabirth_days/it_days_in_month, 3))
  plt_concept_birth <- df_birth_data %>%
    mutate(preterm = factor(preterm)) %>%
    ggplot(aes(x = concept_day_of_year, y=gestation_length_in_days, color=preterm)) +
    facet_wrap(~ conception_year) +
    geom_point() +
    labs(
      title = st_title,
      x = 'Calendar day in year',
      y = 'Gestational age in days at birth'
      ) +
    scale_x_continuous(n.breaks = 12) +
    scale_y_continuous(n.breaks = 10) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
    )
  # print(plt_concept_birth)
  # kable(df_birth_data) %>% kable_styling_fc_wide()
  # plot(df_birth_data$survey_date_conception, df_birth_data$gestation_length_in_days)

  # Return
  return(list(
    df_birth_data = df_birth_data,
    plt_concept_birth = plt_concept_birth,
    ls_concept_fc = ls_concept_fc,
    ls_gsbirth_fc = ls_gsbirth_fc
  ))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define some dates
it_days_in_week <- 7
it_weeks_in_month <- 4
it_months_in_year <- 12
it_years <- 3
# Simulate
ls_concept_birth <- ffi_pop_concept_birth_simu(
  it_pop_n = 5000,
  it_days_in_week = it_days_in_week,
  it_weeks_in_month = it_weeks_in_month,
  it_months_in_year = it_months_in_year,
  it_years = it_years,
  it_rng_seed = 999,
  fl_pre_term_ratio = 0.84,
  fl_peak_concept_frac_of_year_1st = 0.3,
  fl_peak_concept_frac_of_year_2nd = 0.9,
  fl_binom_1st_wgt = 0.00,
  fl_binom_2nd_wgt = 0.00,
  fl_mu_gabirth_days_365 = 276,
  fl_sd_gabirth_days_365 = 14
)
# Get dataframe and print distribution
df_birth_data_rand_cor0 <- ls_concept_birth$df_birth_data
print(ls_concept_birth$ls_concept_fc$plt_concept_week_of_year)
print(ls_concept_birth$ls_gsbirth_fc$plt_dist_gabirth)
print(ls_concept_birth$plt_concept_birth)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Simulate
ls_concept_birth <- ffi_pop_concept_birth_simu(
  it_pop_n = 5000,
  it_days_in_week = it_days_in_week,
  it_weeks_in_month = it_weeks_in_month,
  it_months_in_year = it_months_in_year,
  it_years = it_years,
  it_rng_seed = 999,
  fl_pre_term_ratio = 0.84,
  fl_peak_concept_frac_of_year_1st = 0.2,
  fl_peak_concept_frac_of_year_2nd = 0.9,
  fl_binom_1st_wgt = 0.95,
  fl_binom_2nd_wgt = 0.00,
  fl_mu_gabirth_days_365 = 276,
  fl_sd_gabirth_days_365 = 14
)
# Get dataframe and print distribution
df_birth_data_CFeb_cor0 <- ls_concept_birth$df_birth_data
print(ls_concept_birth$ls_concept_fc$plt_concept_week_of_year)
print(ls_concept_birth$ls_gsbirth_fc$plt_dist_gabirth)
print(ls_concept_birth$plt_concept_birth)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Simulate the temperature distribution using just define parameters
ls_fahrenheit <- ffi_daily_temp_simulation(
  fl_mthly_mean_lowest = 57,
  fl_mthly_mean_highest = 84,
  fl_record_lowest = 32,
  fl_record_highest = 102.4,
  it_weeks_in_year = it_months_in_year*it_weeks_in_month,
  it_days_in_week = it_days_in_week,
  it_years = it_years+1,
  it_rand_seed = 999,
  st_extreme_cold_percentile = "p05",
  st_extreme_heat_percentile = "p95")
print(ls_fahrenheit$plt_fahrenheit)
df_fahrenheit <- ls_fahrenheit$df_fahrenheit
summary(df_fahrenheit$Fahrenheit)
df_stats_fahrenheit <- REconTools::ff_summ_percentiles(df_fahrenheit, FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define the extreme cold function
ffi_extreme_cold_percent_gestation <- function(df_fahrenheit, it_date_conception, it_date_birth){

  # get extreme cold
  ar_extreme_cold <- df_fahrenheit %>%
    filter(survey_t >= it_date_conception & survey_t <= it_date_birth) %>% pull(extreme_cold)

  # extreme cold days
  it_extreme_cold_days <- sum(ar_extreme_cold)

 return(it_extreme_cold_days)
}
# Test the function
it_extreme_cold_days <- ffi_extreme_cold_percent_gestation(df_fahrenheit, 11, 200)
print(it_extreme_cold_days)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Given two dataframes with birth data and temperature data, find cold exposure
ffi_birth_extreme_exposure <- function(df_birth_data, df_fahrenheit){
  # apply row by row, anonymous function
  # see: https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_apply.html#122_anonymous_function
  mt_birth_cold <- apply(df_birth_data, 1, function(row) {

      id <- row[1]
      it_date_conception <- row[2]
      it_date_birth <- row[3]
      it_preterm <- row[11]

      it_extreme_cold_days <- ffi_extreme_cold_percent_gestation(
        df_fahrenheit, it_date_conception, it_date_birth)

      mt_all_res <- cbind(id, it_date_conception, it_date_birth,
                          it_preterm, it_extreme_cold_days)

      return(mt_all_res)
    })

  # Column Names
  ar_st_varnames <- c('id', 'survey_date_conception', 'survey_date_birth',
          'preterm', 'days_extreme_cold')

  # Combine to tibble, add name col1, col2, etc.
  tb_birth_cold <- as_tibble(t(mt_birth_cold)) %>%
    rename_all(~c(ar_st_varnames)) %>%
    mutate(days_extreme_cold_percent = days_extreme_cold/(survey_date_birth-survey_date_conception))

  # Show Results
  # kable(tb_birth_cold[1:20,]) %>% kable_styling_fc()
  return(tb_birth_cold)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scenario (A)
tb_birth_cold_rand_cor0 <- ffi_birth_extreme_exposure(df_birth_data_rand_cor0, df_fahrenheit)
# Scenario (B)
tb_birth_cold_CFeb_cor0 <- ffi_birth_extreme_exposure(df_birth_data_CFeb_cor0, df_fahrenheit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# summarize
str_stats_group <- 'allperc'
ar_perc <- c(0.05, 0.25, 0.5, 0.75, 0.95)

# For tb_birth_cold_rand_cor0
ls_summ_by_group <- REconTools::ff_summ_bygroup(
  tb_birth_cold_rand_cor0, c('preterm'),
  'days_extreme_cold', str_stats_group, ar_perc)
df_table_grp_stats_rand_cor0 <- ls_summ_by_group$df_table_grp_stats
print(df_table_grp_stats_rand_cor0)
# Visualize
plt_rand_cor0_level <- tb_birth_cold_rand_cor0 %>%
  mutate(preterm = factor(preterm)) %>%
  group_by(preterm) %>% mutate(days_extreme_cold_mean = mean(days_extreme_cold)) %>% ungroup() %>%
  ggplot(aes(x=days_extreme_cold, color=preterm)) +
  geom_density() +
  geom_vline(aes(xintercept=days_extreme_cold_mean, color=preterm), linetype="dashed") +
  labs(
    title = paste0('Scenario (A), Extreme cold DAYS Distribution\n',
                   'Uniform Conception\n',
                   'Conception and Birth uncorrelated'),
    x = 'Days exposed to extreme cold',
    y = 'Density'
    ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
  )
print(plt_rand_cor0_level)

# For tb_birth_cold_CFeb_cor0
ls_summ_by_group <- REconTools::ff_summ_bygroup(
  tb_birth_cold_CFeb_cor0, c('preterm'),
  'days_extreme_cold', str_stats_group, ar_perc)
df_table_grp_stats_CFeb_cor0 <- ls_summ_by_group$df_table_grp_stats
print(df_table_grp_stats_CFeb_cor0)
# Visualize
plt_CFeb_cor0_level <- tb_birth_cold_CFeb_cor0 %>%
  mutate(preterm = factor(preterm)) %>%
  group_by(preterm) %>% mutate(days_extreme_cold_mean = mean(days_extreme_cold)) %>% ungroup() %>%
  ggplot(aes(x=days_extreme_cold, color=preterm)) +
  geom_density() +
  geom_vline(aes(xintercept=days_extreme_cold_mean, color=preterm), linetype="dashed") +
  labs(
    title = paste0('Scenario (B), Extreme cold DAYS Distribution (dashed line means)\n',
                   'Conception Concentrated around Feb.\n',
                   'Conception and Birth uncorrelated'),
    x = 'Days exposed to extreme cold',
    y = 'Density'
    ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
  )
print(plt_CFeb_cor0_level)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# summarize
str_stats_group <- 'allperc'
ar_perc <- c(0.05, 0.25, 0.5, 0.75, 0.95)

# For tb_birth_cold_rand_cor0
ls_summ_by_group <- REconTools::ff_summ_bygroup(
  tb_birth_cold_rand_cor0, c('preterm'),
  'days_extreme_cold_percent', str_stats_group, ar_perc)
df_table_grp_stats_rand_cor0 <- ls_summ_by_group$df_table_grp_stats
print(df_table_grp_stats_rand_cor0)
# Visualize
plt_rand_cor0_level <- tb_birth_cold_rand_cor0 %>%
  mutate(preterm = factor(preterm)) %>%
  group_by(preterm) %>% mutate(days_extreme_cold_percent_mean = mean(days_extreme_cold_percent)) %>% ungroup() %>%
  ggplot(aes(x=days_extreme_cold_percent, color=preterm)) +
  geom_density() +
  geom_vline(aes(xintercept=days_extreme_cold_percent_mean, color=preterm), linetype="dashed") +
  labs(
    title = paste0('Scenario (A), Extreme cold DAYS percent of Gestation Distribution\n',
                   'Uniform Conception\n',
                   'Conception and Birth uncorrelated'),
    x = 'Days exposed to extreme cold',
    y = 'Density'
    ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
  )
print(plt_rand_cor0_level)

# For tb_birth_cold_CFeb_cor0
ls_summ_by_group <- REconTools::ff_summ_bygroup(
  tb_birth_cold_CFeb_cor0, c('preterm'),
  'days_extreme_cold_percent', str_stats_group, ar_perc)
df_table_grp_stats_CFeb_cor0 <- ls_summ_by_group$df_table_grp_stats
print(df_table_grp_stats_CFeb_cor0)
# Visualize
plt_CFeb_cor0_level <- tb_birth_cold_CFeb_cor0 %>%
  mutate(preterm = factor(preterm)) %>%
  group_by(preterm) %>% mutate(days_extreme_cold_percent_mean = mean(days_extreme_cold_percent)) %>% ungroup() %>%
  ggplot(aes(x=days_extreme_cold_percent, color=preterm)) +
  geom_density() +
  geom_vline(aes(xintercept=days_extreme_cold_percent_mean, color=preterm), linetype="dashed") +
  labs(
    title = paste0('Scenario (B), Extreme cold DAYS percent of Gestation Distribution\n',
                   'Conception Concentrated around Feb.\n',
                   'Conception and Birth uncorrelated'),
    x = 'Days exposed to extreme cold',
    y = 'Density'
    ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.1, hjust = 0.1)
  )
print(plt_CFeb_cor0_level)

