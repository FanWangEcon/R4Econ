## ----global_options, include = FALSE-----------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------
kable(mtcars %>%
  arrange(cyl, desc(disp)) %>%
    # Select and filter to reduce display clutter
    select(cyl, disp, mpg)) %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# use mtcars: slice_head gets the lowest sorted value
df_groupby_top_mpg <- mtcars %>%
  rownames_to_column(var = "car") %>%
  arrange(cyl, desc(mpg)) %>%
  group_by(cyl) %>%
  slice_head(n=3) %>%
  select(car, cyl, mpg, disp, hp)

# display
kable(df_groupby_top_mpg) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------------------------
# We use what we just created in the last block.
df_groupby_top_mpg_diff <- df_groupby_top_mpg %>%
  group_by(cyl) %>%
  mutate(mpg_diff_higher_minus_lower = mpg - lead(mpg)) %>%
  mutate(mpg_diff_lower_minus_higher = mpg - lag(mpg))

# display
kable(df_groupby_top_mpg_diff)  %>% kable_styling_fc()

