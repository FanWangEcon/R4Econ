## ----global_options, include = FALSE----------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---- amto.tibble.fs_tib_string.find_replace, eval=FALSE--------------------------------------------------------------------------------------------------
## # if string value is contained in variable
## ("bridex.B" %in% (df.reg.out.all$vars_var.y))
## # if string value is not contained in variable:
## # 1. type is variable name
## # 2. Toyota|Mazda are strings to be excluded
## filter(mtcars, !grepl('Toyota|Mazda', type))
## 
## # filter does not contain string
## rs_hgt_prot_log_tidy %>% filter(!str_detect(term, 'prot'))

