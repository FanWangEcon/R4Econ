## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------
# Parameter Setups
it_M <- 10
it_Q_max <- 100
fl_rnorm_mu <- 1
ar_rnorm_sd <- seq(0.01, 0.2, length.out=it_M)
set.seed('789')
ar_it_q <- sample.int(it_Q_max, it_M, replace=TRUE)

# N by Q varying parameters
mt_data <- cbind(ar_it_q, ar_rnorm_sd)
tb_M <- as_tibble(mt_data) %>% rowid_to_column(var = "ID") %>%
                rename(sd = ar_rnorm_sd,
                       Q = ar_it_q) %>%
                mutate(mean = fl_rnorm_mu) %>%
                select(ID, Q,
                       mean, sd)

# Show table
kable(tb_M, caption = paste0("M=", it_M,
  " countries (ID is country ID), observation per country (Q)",
  ", mean and s.d. of wages each country")) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# A. Normal Draw Expansion, Explicitly Name
set.seed('123')
tb_income_norm_dot_dollar <- tb_M %>% group_by(ID) %>%
  do(income = rnorm(.$Q, mean=.$mean, sd=.$sd)) %>%
  unnest(c(income)) %>%
  left_join(tb_M, by="ID")

# Normal Draw Expansion again, dot dollar differently with string variable name
set.seed('123')
tb_income_norm_dollar_dot <- tb_M %>% group_by(ID) %>%
  do(income = rnorm(`$`(., 'Q'), mean = `$`(., 'mean'), sd = `$`(., 'sd'))) %>%
  unnest(c(income)) %>%
  left_join(tb_M, by="ID")

# Normal Draw Expansion again, dot double bracket
set.seed('123')
svr_mean <- 'mean'
svr_sd <- 'sd'
svr_Q <- 'Q'
tb_income_norm_dot_bracket_db <- tb_M %>% group_by(ID) %>%
  do(income = rnorm(.[[svr_Q]], mean = .[[svr_mean]], sd = .[[svr_sd]])) %>%
  unnest(c(income)) %>%
  left_join(tb_M, by="ID")


## ----------------------------------------------------------------------------------------------------------------------------
# Show dataframe dimension
print(dim(tb_income_norm_dot_bracket_db))
# Show first 20 rows
kable(head(tb_income_norm_dot_bracket_db, 20),
  caption = "ID = country ID, wage draws"
  ) %>% kable_styling_fc()
# Display country-specific summaries
REconTools::ff_summ_bygroup(tb_income_norm_dot_bracket_db, c("ID"), "income")$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------------------------------
# Gini by Group
tb_gini_norm <- tb_income_norm_dot_bracket_db %>% group_by(ID) %>%
  do(inc_gini_norm = REconTools::ff_dist_gini_vector_pos(.$income)) %>%
  unnest(c(inc_gini_norm)) %>%
  left_join(tb_M, by="ID")

# display
kable(tb_gini_norm,
  caption = paste0(
    "Country-specific wage GINI based on income draws",
    ", ID=country-ID, Q=sample-size-per-country",
    ", mean=true-income-mean, sd=true-income-sd"
  )) %>%
  kable_styling_fc()

