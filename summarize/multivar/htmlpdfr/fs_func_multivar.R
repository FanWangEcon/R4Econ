## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
# grouping by variables
ar_st_groups <- c("gear", "carb")
# use across to conduct operation over multiple variables
mtcars_3var_times10 <- mtcars %>% 
  group_by(across(one_of(ar_st_groups))) %>%
  mutate(across(matches("mpg|cyl|disp"), ~ .x * 10)) %>%
  select(gear, carb, mpg, cyl, disp) %>% head(n=5)
# pring
# Multiply several variables by 10
kable(mtcars_3var_times10 %>% slice_head(n = 5)) %>% 
    kable_styling_fc()


## -------------------------------------------------------------------------------------
# Generate data, 12 months as columns, and 
mt_data_rand <- matrix(rnorm(36, mean=0, sd=1), nrow=3, ncol=12)
it_rows <- seq(1, dim(mt_data_rand)[1])
it_cols <- seq(1, dim(mt_data_rand)[2])
# convert to table, column as month with leading 0
colnames(mt_data_rand) <- paste0('m', sprintf("%02d", it_cols))
tb_data_full <- as_tibble(mt_data_rand, rownames = NA) %>% 
    mutate(loc = paste0("loc", sprintf("%02d", row_number()))) %>%
    select(loc, everything())
# Display
kable(tb_data_full) %>% kable_styling_fc_wide()  


## -------------------------------------------------------------------------------------
# Extract the data components from the tibble, tibble has row and column names
tb_data_only <- tb_data_full %>% 
    column_to_rownames(var = "loc") %>% 
    select(contains("m"))
# Compute row specific quantiles
ar_quantiles_by_row <- apply(tb_data_only, 1, quantile, probs=0.75)
# Display
print(ar_quantiles_by_row)


## -------------------------------------------------------------------------------------
# One particular quantil from location
tb_loc_quantile <- as_tibble(ar_quantiles_by_row) %>% 
    mutate(loc = names(ar_quantiles_by_row)) %>%
    rename(quantile = value) %>%
    select(loc, everything())
# Display
kable(tb_loc_quantile) %>% kable_styling_fc()  


## -------------------------------------------------------------------------------------
# we introduce NA value to first row
mtcars[1,1] <- NA
# Rename variables, and sum across
mtcars_rowsum <- mtcars %>%
    rename(stats_mpg = mpg, stats_cyl = cyl, stats_hp = hp) %>%
    mutate(
        cs_reduce = purrr::reduce(
            dplyr::pick(contains("stats")),
            `+`
        ),
        cs_rowsum = base::rowSums(
            dplyr::pick(contains("stats")),
            na.rm = TRUE
        ),
        cs_manual = stats_mpg + stats_cyl + stats_hp
    ) %>%
    select(matches("stats|cs"), gear)
# Display
# caption: "sum across columns"
kable(mtcars_rowsum %>% slice_head(n = 5)) %>% kable_styling_fc_wide()


## -------------------------------------------------------------------------------------
# we introduce NA value to first row
# mtcars[1,1] <- NA
# Rename variables, and sum across
mtcars_grpsum <- mtcars_rowsum %>%  
    arrange(gear) %>% group_by(gear) %>%
    # srs = sum row sum
    mutate_at(vars(matches("stats|cs")), 
        .funs = list(gs = ~sum(., na.rm=TRUE))
    ) %>% 
    select(gear, matches("gs")) %>% 
    slice_head(n=1)
# Display
# caption: "gs = group sum, cs = col sum over the columns 
# with stats as prefix, sum across rows after col sum; gear = 4 
# difference for cs-rowsum-gs because it allowed for summing 
# ignoring NA for values across columns"
kable(mtcars_grpsum) %>% kable_styling_fc_wide()


## ----support function multivar cumsum-------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- "date"

# NA dataframe, note need to define as NA_real_
# if define as NA, will not be able to replace with 99 column
# would be logical rather than double.
df_NA <- as_tibble(matrix(NA_real_, nrow = it_N, ncol = it_M)) %>%
    rowid_to_column(var = svr_id) %>%
    rename_at(
        vars(starts_with("V")),
        funs(str_replace(., "V", "var"))
    )
kable(df_NA) %>%
    kable_styling_fc()

# Replace NA
df_NA_replace <- df_NA %>%
    mutate_at(vars(one_of(c("var1", "var2"))), list(~ replace_na(., 0))) %>%
    mutate_at(vars(one_of(c("var3", "var5"))), list(~ replace_na(., 99)))

kable(df_NA_replace) %>%
    kable_styling_fc()


## ----support function multivar cumsum-------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- "date"

# random dataframe, daily profit of firms
# dp_fx: daily profit firm ID something
set.seed(123)
df_daily_profit <- as_tibble(matrix(rnorm(it_N * it_M), nrow = it_N, ncol = it_M)) %>%
    rowid_to_column(var = svr_id) %>%
    rename_at(
        vars(starts_with("V")),
        funs(str_replace(., "V", "dp_f"))
    )
kable(df_daily_profit) %>%
    kable_styling_fc()

# cumulative sum with suffix
df_cumu_profit_suffix <- df_daily_profit %>%
    mutate_at(vars(contains("dp_f")), .funs = list(cumu = ~ cumsum(.)))
kable(df_cumu_profit_suffix) %>%
    kable_styling_fc_wide()

# cumulative sum variables naming to prefix
df_cumu_profit <- df_cumu_profit_suffix %>%
    rename_at(vars(contains("_cumu")), list(~ paste("cp_f", gsub("_cumu", "", .), sep = ""))) %>%
    rename_at(vars(contains("cp_f")), list(~ gsub("dp_f", "", .)))
kable(df_cumu_profit) %>%
    kable_styling_fc_wide()

