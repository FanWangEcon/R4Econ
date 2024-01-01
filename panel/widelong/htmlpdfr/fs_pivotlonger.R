## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
# Generate A Matrix
set.seed(123)
ar_a <- c(1.1,5.1)
ar_z <- seq(-2.5, 2.53, length.out=11)
mt_ev = matrix(rnorm(length(ar_a)*length(ar_z)), 
  nrow=length(ar_a), ncol=length(ar_z))

# Name Matrix
rownames(mt_ev) <- paste0('ai', seq(1:length(ar_a)))
colnames(mt_ev) <- paste0('zi', seq(1:length(ar_z)))

# to tibble
tb_ev <- as_tibble(mt_ev) %>% rowid_to_column(var = "ai")

# Print
print(mt_ev)
# Display
kable(tb_ev, caption = "Wide table") %>% kable_styling_fc()


## -------------------------------------------------------------------------------------

# longer
tb_ev_long <- tb_ev %>%
  pivot_longer(cols = starts_with('zi'),
               names_to = c('zi'),
               names_pattern = paste0("zi(.*)"),
               values_to = "ev") %>%
  mutate(zi = as.numeric(zi))

# Merge with a and z values
tb_ev_long <- tb_ev_long %>%
  left_join(as_tibble(ar_a) %>%
              rowid_to_column(var = "ai") %>%
              rename(a = value)
              , by = 'ai') %>%
  left_join(as_tibble(ar_z) %>%
              rowid_to_column(var = "zi") %>%
              rename(z = value),
            by = 'zi') %>%
  select(a,ai,z,zi,ev)

# Display
kable(tb_ev_long, caption = "Long table") %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# Generate A Matrix
set.seed(123)
ar_year <- c(1995, 1997, 1999)
ar_vars <- c("wage_model", "quant_model", "wage_simu", "quant_simu")
mt_equi = matrix(rnorm(length(ar_year)*length(ar_vars)), 
  nrow=length(ar_year), ncol=length(ar_vars))

# Name Matrix
rownames(mt_equi) <- ar_year
colnames(mt_equi) <- ar_vars

# to tibble
tb_equi <- as_tibble(mt_equi, rownames = "year")

# Print
print(mt_equi)
# Display
kable(tb_equi, caption = "Wide table") %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# longer
tb_equi_long <- tb_equi %>%
  pivot_longer(cols = matches('wage|quant'),
               names_to = c('variable', 'source'),
               names_pattern = paste0("(.*)_(.*)"),
               values_to = "value") 

# Display
kable(tb_equi_long, caption = "Long table, Two Variables") %>% kable_styling_fc()

