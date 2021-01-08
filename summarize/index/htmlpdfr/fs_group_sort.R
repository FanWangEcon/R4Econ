## ----global_options, include = FALSE----------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---------------------------------------------------------------------------------------------------------------------
# use mtcars
df_groupby_top_mpg <- mtcars %>%
  arrange(cyl, mpg) %>%
  group_by(cyl) %>%
  slice_head(n=1)
# display
kable(df_groupby_top_mpg) %>% kable_styling_fc()

