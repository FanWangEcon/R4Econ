#' ---
#' title: "R Example Apply the Same Function Over Multiple Variables"
#' author: Fan Wang
#' output:
#'   pdf_document: default
#'   word_document: default
#'   html_notebook: default
#'   html_document: default
#' urlcolor: blue
#' always_allow_html: yes
#' ---
#' 
#' ### Apply the Same Function Multiple Variables
#' 
#' > Go back to [fan](http://fanwangecon.github.io/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository, or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository.
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F----------------------------------------------------------------------------------------------------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
# file name
st_file_name = 'fs_func_multivar'
# Generate R File
try(purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2))
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/support/function/fs_func_multivar", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/support/function/fs_func_multivar", "html_document")

#' 
#' #### Replace NA for Multiple Variables
#' 
#' Replace some variables NA by some values, and other variables' NAs by other values. 
#' 
## ----support function multivar cumsum-----------------------------------------------------------------------------------------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- 'date'

# NA dataframe
df_NA <- as_tibble(matrix(NA, nrow=it_N, ncol=it_M)) %>%
  rowid_to_column(var = svr_id) %>%
  rename_at(vars(starts_with("V")),
            funs(str_replace(., "V", "var")))
kable(df_NA) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

# Replace NA
df_NA_replace <- df_NA %>% 
  mutate_at(vars(one_of(c('var1', 'var2'))), list(~replace_na(., 0))) %>%
  mutate_at(vars(one_of(c('var3', 'var5'))), list(~replace_na(., 99)))
kable(df_NA_replace) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

#' 
#' #### Cumulative Sum Multiple Variables
#' 
#' Each row is a different date, each column is the profit a firms earns on a date, we want to compute cumulatively how much a person is earning. Also renames variable names below jointly.
#' 
## ----support function multivar cumsum-----------------------------------------------------------------------------------------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- 'date'

# random dataframe, daily profit of firms
# dp_fx: daily profit firm ID something
set.seed(123)
df_daily_profit <- as_tibble(matrix(rnorm(it_N*it_M), nrow=it_N, ncol=it_M)) %>%
  rowid_to_column(var = svr_id) %>%
  rename_at(vars(starts_with("V")),
            funs(str_replace(., "V", "dp_f")))
kable(df_daily_profit) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

# cumulative sum with suffix
df_cumu_profit_suffix <- df_daily_profit %>%
  mutate_at(vars(contains('dp_f')), .funs = list(cumu = ~cumsum(.)))
kable(df_cumu_profit_suffix) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

# cumulative sum variables naming to prefix
df_cumu_profit <- df_cumu_profit_suffix %>%
  rename_at(vars(contains( "_cumu") ), list(~paste("cp_f", gsub("_cumu", "", .), sep = ""))) %>%
  rename_at(vars(contains( "cp_f") ), list(~gsub("dp_f", "", .)))
kable(df_cumu_profit) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

