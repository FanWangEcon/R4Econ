## ----global_options, include = FALSE-------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate A Matrix
set.seed(123)
ar_a <- c(1.2, 22.2, 33.33)
ar_z <- c(-2.5,  -1, 0, 1, 2.53)
mt_ev = matrix(rnorm(ar_a*ar_z), nrow=length(ar_a), ncol=length(ar_z))

# Name Matrix
rownames(mt_ev) <- paste0('ai', seq(1:length(ar_a)))
colnames(mt_ev) <- paste0('zi', seq(1:length(ar_z)))

# to tibble
tb_ev <- as_tibble(mt_ev) %>% rowid_to_column(var = "ai")

# longer 
tb_ev_long <- tb_ev %>% 
  pivot_longer(cols = starts_with('zi'), 
               names_to = c('zi'),
               names_pattern = paste0("zi(.)"),
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
kable(tb_ev) %>% kable_styling_fc()  
kable(tb_ev_long) %>% kable_styling_fc()  

