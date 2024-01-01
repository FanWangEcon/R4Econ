## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
# Mesh
ar_st_varnames <- c('hh','week','dayofweek')
ar_it_ids <- c(1,2)
ar_it_weeks <- c(1,2)
ar_it_daysofweek <- c(1,2,3)
df_idwkday_mesh <- tidyr::expand_grid(
  ar_it_ids, ar_it_weeks, ar_it_daysofweek) %>%
  rename_all(~c(ar_st_varnames))

# Randomly drop a subset of rows 
# Different subset of ID and Week for each DayOfWeek. 
it_M <- 4
set.seed(456)
df_idwkday_mesh <- df_idwkday_mesh[sample(dim(df_idwkday_mesh)[1], it_M, replace=FALSE),] %>% 
  arrange(!!!syms(ar_st_varnames))

# Display
st_caption <- "File A (ID x Week x DayOfWeek)"
kable(df_idwkday_mesh, caption=st_caption) %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# Generate day of week specific product file
ar_st_varnames <- c('hh', 'dayofweek', 'product')
ar_it_product <- c(10,11,12,13,14)
df_dayproduct_mesh <- tidyr::expand_grid(
  ar_it_ids, ar_it_daysofweek, ar_it_product) %>%
  rename_all(~c(ar_st_varnames))

# Make each day product list not identical
it_M <- 8
set.seed(123)
df_dayproduct_mesh <- df_dayproduct_mesh[sample(dim(df_dayproduct_mesh)[1], it_M, replace=FALSE),] %>% 
  arrange(!!!syms(ar_st_varnames))

# Display
st_caption <- "File B (ID x DayOfWeek x Product)"
kable(df_dayproduct_mesh, caption=st_caption) %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# left join
df_left_join <- df_idwkday_mesh %>% 
  left_join(df_dayproduct_mesh, 
  by= c('hh'='hh', 'dayofweek'='dayofweek'))
# Display left-join
st_caption <- "File C, left-join (ID x Week x DayOfweek x Product)"
kable(df_left_join, caption=st_caption) %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# full join
df_full_join <- df_idwkday_mesh %>% 
  full_join(df_dayproduct_mesh, 
  by= c('hh'='hh', 'dayofweek'='dayofweek'))
# Display full-join
st_caption <- "File C, full-join (ID x Week x DayOfweek x Product)"
kable(df_full_join, caption=st_caption) %>% kable_styling_fc()

