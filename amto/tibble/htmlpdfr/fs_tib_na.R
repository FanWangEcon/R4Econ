## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---- amto.tibble.fs_tib_na.find_replace, eval=FALSE-----------------------------------------------------------------------------------------------------
## # For dataframe
## df.reg <-df.reg %>% na_if(-Inf) %>% na_if(Inf)
## # For a specific variable in dataframe
## df.reg.use %>% mutate(!!(var.input) := na_if(!!sym(var.input), 0))
## 
## # Setting to NA
## df.reg.use <- df.reg.guat %>% filter(!!sym(var.mth) != 0)
## df.reg.use.log <- df.reg.use
## df.reg.use.log[which(is.nan(df.reg.use$prot.imputed.log)),] = NA
## df.reg.use.log[which(df.reg.use$prot.imputed.log==Inf),] = NA
## df.reg.use.log[which(df.reg.use$prot.imputed.log==-Inf),] = NA
## df.reg.use.log <- df.reg.use.log %>% drop_na(prot.imputed.log)
## # df.reg.use.log$prot.imputed.log

