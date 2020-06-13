## ----global_options, include = FALSE------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- 'student_id'
svr_date <- 'class_day'

# dataframe
df_panel_skeleton <- as_tibble(matrix(it_M, nrow=it_N, ncol=1)) %>%
  rowid_to_column(var = svr_id) %>%
  uncount(V1) %>%
  group_by(!!sym(svr_id)) %>% mutate(!!sym(svr_date) := row_number()) %>%
  ungroup()

# Print
kable(df_panel_skeleton) %>%
  kable_styling_fc()


## -----------------------------------------------------------------------------------------------------------------------------------
# Define
it_N <- 5
it_M <- 3
svr_id <- 'indi_id'
svr_gender <- 'female'
svr_asian <- 'asian'
svr_age <- 'year_of_age'
# Define Height Related Variables
svr_brthgt <- 'birth_height'
svr_hgtgrow <- 'hgt_growth'
svr_hgtgrow_cumu <- 'hgt_growcumu'
svr_height <- 'height'

# panel dataframe following
set.seed(123)
df_panel_indiage <- as_tibble(matrix(it_M, nrow=it_N, ncol=1)) %>%
  mutate(!!sym(svr_gender) := rbinom(n(), 1, 0.5),
         !!sym(svr_asian) := rbinom(n(), 1, 0.5),
         !!sym(svr_brthgt) := rnorm(n(), mean=60,sd=3)) %>%
  uncount(V1) %>%
  group_by(!!sym(svr_gender), !!sym(svr_asian), !!sym(svr_brthgt)) %>%
  mutate(!!sym(svr_age) := row_number(),
         !!sym(svr_hgtgrow) := runif(n(), min=5, max=15),
         !!sym(svr_hgtgrow_cumu) := cumsum(!!sym(svr_hgtgrow)),
         !!sym(svr_height) := !!sym(svr_brthgt) + !!sym(svr_hgtgrow_cumu))  %>%
  ungroup()

# Add Height Index
kable(df_panel_indiage) %>% kable_styling_fc()


## -----------------------------------------------------------------------------------------------------------------------------------
# group id
svr_group_id <- 'female_asian_id'
# Define
ls_svr_group_vars <- c('female', 'asian')

# panel dataframe following
df_panel_indiage_id <- df_panel_indiage %>%
  arrange(!!!syms(ls_svr_group_vars)) %>%
  group_by(!!!syms(ls_svr_group_vars)) %>%
  mutate(!!sym(svr_group_id) := (row_number()==1)*1) %>%
  ungroup() %>%
  mutate(!!sym(svr_group_id) := cumsum(!!sym(svr_group_id))) %>%
  select(one_of(svr_group_id, ls_svr_group_vars), everything())

# Add Height Index
kable(df_panel_indiage_id) %>%
  kable_styling_fc_wide()

