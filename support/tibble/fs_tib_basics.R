#' ---
#' title: "R DPLYR Tibble Basics"
#' output:
#'   pdf_document: default
#'   word_document: default
#'   html_document: default
#'   html_notebook: default
#' urlcolor: blue
#' always_allow_html: yes
#' ---
#' 
#' Go back to [fan](http://fanwangecon.github.io/CodeDynaAsset/)'s [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository.
#' 
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F----------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

#' 
## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(R4Econ)
# file name
st_file_name = 'fs_tib_basics'
# Generate R File
purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2)
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/support/tibble/fs_tib_basics.Rmd", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/support/tibble/fs_tib_basics.Rmd", "html_document")

#' 
#' 
#' # Tibble Basics
#' 
#' ## Generate Tibble given Matrixes and Arrays
#' 
#' Given Arrays and Matrixes, Generate Tibble and Name Variables/Columns
#' 
#' - naming tibble columns
#' - tibble variable names
#' - dplyr rename tibble
#' - dplyr rename tibble all variables
#' - dplyr rename all columns by index
#' - see also: [SO-51205520](https://stackoverflow.com/questions/45535157/difference-between-dplyrrename-and-dplyrrename-all)
#' 
## -----------------------------------------------------------------------------
# Base Inputs
ar_col <- c(-1,+1)
mt_rnorm <- matrix(rnorm(4,mean=0,sd=1), nrow=2, ncol=2)

# Combine Matrix
mt_combine <- cbind(ar_col, mt_rnorm)
colnames(mt_combine) <- paste0('matcolvar', seq(1,dim(mt_combine)[2]))

# Variable Names
ar_st_varnames <- paste0('tibcolvar', c(1,2,3))

# Combine to tibble, add name col1, col2, etc.
tb_combine <- as_tibble(mt_combine) %>% rename_all(~c(ar_st_varnames))

# Tibble back to matrix
mt_tb_combine_back <- data.matrix(tb_combine)

# Display
kable(mt_combine) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
kable(tb_combine) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))
kable(mt_tb_combine_back) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

#' 
#' ## Tibble Row and Column and Summarize
#' Show what is in the table: 1, column and row names; 2, contents inside table.
#' 
## -----------------------------------------------------------------------------
tb_iris <- as_tibble(iris)
rownames(tb_iris)
colnames(tb_iris)
colnames(tb_iris)
summary(tb_iris)

#' 
#' ## Tibble Sorting
#' 
#' - dplyr arrange desc reverse
#' - dplyr sort
#' 
## -----------------------------------------------------------------------------
# Sort in Ascending Order
tb_iris %>% select(Species, Sepal.Length, everything()) %>%
  arrange(Species, Sepal.Length) %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

# Sort in Descending Order
tb_iris %>% select(Species, Sepal.Length, everything()) %>%
  arrange(desc(Species), desc(Sepal.Length)) %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

#' 
#' 
#' # R4Econ Function
#' 
#' ## R4Econ Summarize over Tible
#' Use R4Econ's summary tool.
#' 
## -----------------------------------------------------------------------------
df_summ_stats <- ff_summ_percentiles(tb_iris)
kable(t(df_summ_stats)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

