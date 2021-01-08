## ----global_options, include = FALSE----------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---------------------------------------------------------------------------------------------------------------------
# Load the REconTools Dataset df_hgt_wgt
data("df_hgt_wgt")
# str(df_hgt_wgt)

# Select several rows
df_hgt_wgt_sel <- df_hgt_wgt %>% 
  filter(S.country == "Cebu") %>%
  select(indi.id, svymthRound, prot)


## ---------------------------------------------------------------------------------------------------------------------
# Group by indi.id and sort by protein
df_hgt_wgt_sel_cummean <- df_hgt_wgt_sel %>%
  arrange(indi.id, svymthRound) %>%
  group_by(indi.id) %>%
  mutate(prot_cummean = cummean(prot))

# display results
REconTools::ff_summ_percentiles(df_hgt_wgt_sel_cummean)
# display results
df_hgt_wgt_sel_cummean %>% filter(indi.id %in% c(17, 18)) %>% 
  kable() %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------
# https://stackoverflow.com/a/49906718/8280804
# Group by indi.id and sort by protein
df_hgt_wgt_sel_cummean_noNA <- df_hgt_wgt_sel %>%
  arrange(indi.id, svymthRound) %>%
  group_by(indi.id, isna = is.na(prot)) %>%
  mutate(prot_cummean = ifelse(isna, NA, cummean(prot)))

# display results
df_hgt_wgt_sel_cummean_noNA %>% filter(indi.id %in% c(17, 18)) %>% 
  kable() %>% kable_styling_fc()

