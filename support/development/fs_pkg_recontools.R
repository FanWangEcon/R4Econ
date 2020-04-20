## ----global_options, include = FALSE-----------------------------------------------------------------
try(source("C:/Users/fan/R4Econ/.Rprofile"))
library(reticulate)


## ----------------------------------------------------------------------------------------------------
# file_path_root <- 'C:/Users/fan/R4Econ/support/development/fs_pkg_recontools'
# file_path_r <- paste0(file_path_root, '.R')
# file_path_rmd <- paste0(file_path_root, '.Rmd')
# knitr::purl(file_path_rmd, output=file_path_r, documentation = 1)


## ----------------------------------------------------------------------------------------------------
spt_root <- 'C:/Users/fan/REconTools'
spt_log <- 'C:/Users/fan/REconTools/log/devtools/'
sink(paste0(spt_log, 'devtools_load_all.Rhistory'))
start_time <- Sys.time()
setwd(spt_root)
devtools::load_all()
library(REconTools)
end_time <- Sys.time()
print(paste0('devtools::load_all() took:', end_time - start_time))
sink()


## ----echo = T, results='hide', message=FALSE, warning=FALSE------------------------------------------
# run examples
spt_root <- 'C:/Users/fan/REconTools'
spt_log <- 'C:/Users/fan/REconTools/log/devtools/'
sink(paste0(spt_log, 'devtools_run_examples.Rhistory'))
start_time <- Sys.time()
setwd(spt_root)
devtools::document()
devtools::run_examples()
end_time <- Sys.time()
print(paste0('Document and devtools::run_examples() took:', end_time - start_time))
sink()


## ---- echo = T, results = 'hide'---------------------------------------------------------------------
# check
spt_root <- 'C:/Users/fan/REconTools'
spt_log <- 'C:/Users/fan/REconTools/log/devtools/'
sink(paste0(spt_log, 'devtools_check.Rhistory'))
start_time <- Sys.time()
setwd(spt_root)
devtools::check()
end_time <- Sys.time()
print(paste0('devtools::check() took:', end_time - start_time))
sink()


## ---- echo = T, results = 'hide'---------------------------------------------------------------------
# check
spt_root <- 'C:/Users/fan/REconTools'
spt_log <- 'C:/Users/fan/REconTools/log/devtools/'
setwd(spt_root)
start_time <- Sys.time()
sink(paste0(spt_log, 'pkgdown_build_site.Rhistory'))
pkgdown::build_site()
end_time <- Sys.time()
print(paste0('pkgdown::build_site() took:', end_time - start_time))
sink()

