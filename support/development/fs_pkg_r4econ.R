## ----global_options, include = FALSE-----------------------------------------------------------------
try(source("C:/Users/fan/R4Econ/.Rprofile"))
library(reticulate)


## ----------------------------------------------------------------------------------------------------
# file_path_root <- 'C:/Users/fan/R4Econ/support/development/fs_pkg_r4econ'
# file_path_r <- paste0(file_path_root, '.R')
# file_path_rmd <- paste0(file_path_root, '.Rmd')
# knitr::purl(file_path_rmd, output=file_path_r, documentation = 1)


## ----------------------------------------------------------------------------------------------------
# run examples
spt_root <- 'C:/Users/fan/R4Econ'
spt_log <- 'C:/Users/fan/R4Econ/_log/rmdclean/'
sink(paste0(spt_log, 'rmd_clean_test.log'))
start_time <- Sys.time()
setwd(spt_root)

bl_test = TRUE
bl_gen_if_git_old = FALSE
ff_sup_clean_rmd(ar_spt_root = c('C:/Users/fan/R4Econ'),
                 ar_spn_skip = c('support', 'index.Rmd', 'main.Rmd'),
                 st_folder_pdf = '/htmlpdfr/',
                 st_folder_html = '/htmlpdfr/',
                 st_folder_R = '/htmlpdfr/',
                 bl_gen_if_git_old = bl_gen_if_git_old,
                 bl_recursive = TRUE,
                 bl_verbose = FALSE,
                 bl_test = bl_test)

end_time <- Sys.time()
print(paste0('ff_sup_clean_rmd(), bl_test = TRUE, took:', end_time - start_time))
sink()


## ----------------------------------------------------------------------------------------------------
spt_root <- 'C:/Users/fan/R4Econ'
spt_log <- 'C:/Users/fan/R4Econ/_log/rmdclean/'
sink(paste0(spt_log, 'rmd_clean_actual.log'))
start_time <- Sys.time()
setwd(spt_root)

bl_test = FALSE
bl_gen_if_git_old = TRUE
ff_sup_clean_rmd(ar_spt_root = c('C:/Users/fan/R4Econ'),
                 ar_spn_skip = c('support', 'index.Rmd', 'main.Rmd'),
                 st_folder_pdf = '/htmlpdfr/',
                 st_folder_html = '/htmlpdfr/',
                 st_folder_R = '/htmlpdfr/',
                 bl_gen_if_git_old = bl_gen_if_git_old,
                 bl_recursive = TRUE,
                 bl_verbose = FALSE,
                 bl_test = bl_test)

end_time <- Sys.time()
print(paste0('ff_sup_clean_rmd(), bl_test = FALSE, took:', end_time - start_time))
sink()


## import pyfan.util.rmd.bookdownparse as ft_bookdownparse

##
## # Path Roots

## sfc_prj = 'R4Econ'

## sph_prj = 'C:/Users/fan/R4Econ/'

## spn_prj_bookdown_yml = '_bookdown.yml'

##
## # Read me preamble, and endnotes

## spn_prj_readme_pre = 'README_pre.md'

## spn_prj_readme_end = 'README_end.md'

## spn_prj_readme_toc = 'README_toc.md'

## # combine preamble, contents, suffix together

## spn_prj_readme_ful = 'README.md'

##
## # Generate README TOC

## ft_bookdownparse.fs_yml2readme(sfc_prj=sfc_prj, sph_prj=sph_prj,

##                                spn_prj_bookdown_yml=spn_prj_bookdown_yml,

##                                spn_prj_readme_toc=spn_prj_readme_toc)

##
## # Combine README Files

## filenames = [spn_prj_readme_pre, spn_prj_readme_toc, spn_prj_readme_end]

## with open(sph_prj + spn_prj_readme_ful, 'w') as outfile:

##     for fname in filenames:

##         sph_full_fnmae = sph_prj + fname

##         with open(sph_full_fnmae) as infile:

##             outfile.write(infile.read())

##             outfile.write('\n')


## ----------------------------------------------------------------------------------------------------
spt_root <- 'C:/Users/fan/R4Econ'
spt_log <- 'C:/Users/fan/R4Econ/_log/rmdclean/'

sink(paste0(spt_log, 'render_bookdown_html.log'))
start_time <- Sys.time()
setwd(spt_root)

# render html
bookdown::render_book('index.Rmd', 'bookdown::gitbook')

end_time <- Sys.time()
print(paste0('bookdown::render_book, html, took:', end_time - start_time))
sink()


## ----------------------------------------------------------------------------------------------------
spt_root <- 'C:/Users/fan/R4Econ'
spt_log <- 'C:/Users/fan/R4Econ/_log/rmdclean/'

sink(paste0(spt_log, 'render_bookdown_pdf.log'))
start_time <- Sys.time()
setwd(spt_root)

bookdown::render_book('index.Rmd', 'bookdown::pdf_book')

end_time <- Sys.time()
print(paste0('bookdown::render_book, pdf, took:', end_time - start_time))
sink()


