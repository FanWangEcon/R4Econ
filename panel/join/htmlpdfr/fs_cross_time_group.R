## ----global_options, include = FALSE-----------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------
# Load data, and treat index as "year"
# pretend data to be country-data
df_attitude <- as_tibble(attitude) %>%
  rowid_to_column(var = "year") %>% 
  select(year, rating, complaints, learning) %>% 
  rename(stats_usa = rating, 
         stats_canada = complaints, 
         stats_uk = learning)

# Wide to Long
df_attitude <- df_attitude %>%
  pivot_longer(cols = starts_with('stats_'),
               names_to = c('country'),
               names_pattern = paste0("stats_(.*)"),
               values_to = "rating")

# Print 
kable(df_attitude[1:10,]) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------
# Sort and get list of countries
ar_countries_sorted <- df_attitude %>% 
  ungroup() %>% distinct(country) %>% arrange(country) %>% 
  pull(country)
st_ratio_var <- paste0('ratings_ratio_vs_country', ar_countries_sorted[1])

# Generate ratio over the first location
df_attitude <- df_attitude %>% 
  arrange(year, country) %>% group_by(year) %>% 
  mutate(!!sym(st_ratio_var) := rating/first(rating))
  
# Print 
kable(df_attitude[1:10,]) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------
# Sort and get list of countries
ar_years_sorted <- df_attitude %>% 
  ungroup() %>% distinct(year) %>% arrange(year) %>% 
  pull(year)
st_ratio_var <- paste0('ratings_ratio_vs_year', ar_years_sorted[1])

# Generate ratio over the first location
df_attitude <- df_attitude %>% 
  arrange(country, year) %>% group_by(country) %>% 
  mutate(!!sym(st_ratio_var) := rating/first(rating))
  
# Print
# Within each country, we show the first 3 years 
kable(df_attitude %>% 
  group_by(country) %>% 
  slice_min(order_by = year, n = 3)
  ) %>% kable_styling_fc()

