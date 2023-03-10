## ----global_options, include = FALSE---------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------
# We use the mtcars dataset
tb_tab_joint <- mtcars %>%
    group_by(gear, am) %>%
    tally()
# Display
tb_tab_joint %>%
    kable(caption = "cross tabulation, stacked") %>%
    kable_styling_fc()


## --------------------------------------------------------------------------------------------------------------
# We use the mtcars dataset
tb_cross_tab <- mtcars %>%
    group_by(gear, am) %>%
    tally() %>%
    spread(am, n)
# Display
tb_cross_tab %>%
    kable(caption = "cross tabulation") %>%
    kable_styling_fc()


## --------------------------------------------------------------------------------------------------------------
tb_dist_tab <- mtcars %>%
    # .keep_all to keep all variables
    distinct(am, mpg, .keep_all = TRUE) %>%
    group_by(am) %>%
    tally()
# Display
tb_dist_tab %>%
    kable(caption = "Tabulate distinct groups") %>%
    kable_styling_fc()


## --------------------------------------------------------------------------------------------------------------
# 1. Array of Years in the Survey
ar_years_in_survey <- c(2, 3, 1, 10, 2, 5)
ar_start_yaer <- c(1, 2, 3, 1, 1, 1)
ar_end_year <- c(2, 4, 3, 10, 2, 5)
mt_combine <- cbind(ar_years_in_survey, ar_start_yaer, ar_end_year)

# This is the individual attribute dataset, attributes that are invariant acrosss years
tb_indi_attributes <- as_tibble(mt_combine) %>% rowid_to_column(var = "ID")

# Display
tb_indi_attributes %>%
    head(10) %>%
    kable() %>%
    kable_styling_fc()


## --------------------------------------------------------------------------------------------------------------
# 2. Sort and generate variable equal to sorted index
tb_indi_panel <- tb_indi_attributes %>% uncount(ar_years_in_survey)

# 3. Panel now construct exactly which year in survey, note that all needed is sort index
# Note sorting not needed, all rows identical now
tb_indi_panel <- tb_indi_panel %>%
    group_by(ID) %>%
    mutate(yr_in_survey = row_number())

tb_indi_panel <- tb_indi_panel %>%
    mutate(calendar_year = yr_in_survey + ar_start_yaer - 1)

# Show results Head 10
tb_indi_panel %>%
    head(10) %>%
    kable() %>%
    kable_styling_fc()

