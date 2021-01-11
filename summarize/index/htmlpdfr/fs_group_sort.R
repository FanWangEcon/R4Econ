## ----global_options, include = FALSE---------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------
kable(mtcars %>% arrange(cyl, mpg)) %>% kable_styling_fc()


## --------------------------------------------------------------------------------------------------------------------
# use mtcars: slice_head gets the lowest sorted value
df_groupby_top_mpg <- mtcars %>%
  arrange(cyl, mpg) %>%
  group_by(cyl) %>%
  slice_head(n=1) %>%
  select(cyl, mpg)

# display
kable(df_groupby_top_mpg) %>% kable_styling_fc()

