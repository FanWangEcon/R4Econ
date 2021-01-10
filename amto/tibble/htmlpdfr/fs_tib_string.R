## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# Sting data inputs
ls_st_abc <- c('a', 'b', 'c')
ls_st_efg <- c('e', 'f', 'g')
ls_st_opq <- c('o', 'p', 'q')
mt_str = cbind(ls_st_abc, ls_st_efg, ls_st_opq)

# Column Names
ar_st_varnames <- c('id','var1','var2','var3')

# Combine to tibble, add name col1, col2, etc.
tb_st_combine <- as_tibble(mt_str) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames))

# Display
kable(tb_st_combine) %>% kable_styling_fc()


## ---- amto.tibble.fs_tib_string.find_replace, eval=FALSE-----------------------------------------------
## # if string value is contained in variable
## ("bridex.B" %in% (df.reg.out.all$vars_var.y))
## # if string value is not contained in variable:
## # 1. type is variable name
## # 2. Toyota|Mazda are strings to be excluded
## filter(mtcars, !grepl('Toyota|Mazda', type))
## 
## # filter does not contain string
## rs_hgt_prot_log_tidy %>% filter(!str_detect(term, 'prot'))

