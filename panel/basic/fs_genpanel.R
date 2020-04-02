#' ---
#' title: "TIDYVERSE Generate Panel Data Structures"
#' author: Fan Wang
#' output:
#'   pdf_document: default
#'   word_document: default
#'   html_document: default
#'   html_notebook: default
#' urlcolor: blue
#' always_allow_html: yes
#' ---
#' 
#' ### Generate Panel Data Structures
#' 
#' > Go back to [fan](http://fanwangecon.github.io/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository, or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository.
#' 
#' Generate panel data structure.
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F----------------------------------------------------------------------------------------------------
options(knitr.duplicate.label = 'allow')

## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------------------------------------------------------------------------------------------------
library(tibble)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
# file name
st_file_name = 'fs_genpanel'
# Generate R File
try(purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2))
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/panel/basic/fs_genpanel.Rmd", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/panel/basic/fs_genpanel.Rmd", "html_document")

#' 
#' #### Balanced Panel Skeleton
#' 
#' There are $N$ individuals, each could be observed $M$ times. In the example below, there are 3 students, each observed over 4 dates. This just uses the [uncount](https://tidyr.tidyverse.org/reference/uncount.html) function from *tidyr*.
#' 
## ----rand draws panel random select-------------------------------------------------------------------------------------------------------------------------------------
# Define
it_N <- 3
it_M <- 5
svr_id <- 'student_id'
svr_date <- 'class_day'

# dataframe
set.seed(123)
df_panel_skeleton <- as_tibble(matrix(it_M, nrow=it_N, ncol=1)) %>%
  rowid_to_column(var = svr_id) %>%
  uncount(V1) %>%
  group_by(!!sym(svr_id)) %>% mutate(!!sym(svr_date) := row_number()) %>%
  ungroup()

# Print
kable(df_panel_skeleton) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

