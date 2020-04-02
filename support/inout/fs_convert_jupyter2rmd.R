#' ---
#' title: "Convert Jupyter Files to RMD"
#' output:
#'   html_notebook: default
#'   pdf_document:
#'     df_print: paged
#'   html_document:
#'     df_print: paged
#'   word_document: default
#' urlcolor: blue
#' ---
#' 
#' Back to [Fan](https://fanwangecon.github.io)'s [Reusable R Code](https://fanwangecon.github.io/R4Econ/) table of content.
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F-----
options(knitr.duplicate.label = 'allow')

## ----loadlib, echo = T, results = 'hide', message=F, warning=F-----------
library(tidyverse)
library(tidyr)
library(rmarkdown)
library(knitr)
library(kableExtra)
# file name
st_file_name = 'fs_convert_jupyter2rmd'
# Generate R File
try(purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2))
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/support/array/fs_meshr.Rmd", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/support/array/fs_meshr.Rmd", "html_document")

#' 
#' # Jupyter Files and RMD
#' 
#' Rmarkdown in Rstudio is easier for debugging, and allows for easier interaction with current workspace.
#' 
#' # Jupyter to RMD conversion
#' 
#' Rmarkdown has a conversion program: [convert_ipynb](https://rmarkdown.rstudio.com/docs/reference/convert_ipynb.html).
#' 
## ------------------------------------------------------------------------
# Generate Paths
spt_file_root = 'C:/Users/fan/Stat4Econ/descriptive/'
spt_file_name = 'DataBasketball'
spt_file_full_ipynb = paste0(spt_file_root, spt_file_name, '.ipynb')
spt_file_full_rmd = paste0(spt_file_root, spt_file_name, '.rmd')

# Convert from IPYNB to RMD
file_nb_rmd = rmarkdown:::convert_ipynb(spt_file_full_ipynb)
st_nb_rmd = xfun::file_string(file_nb_rmd)

# Save RMD
fileConn <- file(spt_file_full_rmd)
writeLines(st_nb_rmd, fileConn)
close(fileConn)

# Convert to PDF and HTML
rmarkdown::render(spt_file_full_rmd, "pdf_document")
rmarkdown::render(spt_file_full_rmd, "html_document")

