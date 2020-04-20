# source('C:/Users/fan/R4Econ/support/inout/fs_rmd_knitall.R')
#
# library(tidyverse)
# library(tidyr)
# library(knitr)
# library(kableExtra)

# Knit all Rmd Files stored in folders
# I maintain both bookdown as well as individually compiled PDFs for each page
# This file finds all Rmd files in R4Eon and knits them all to pdf and html.
spt_root <- 'C:/Users/fan/R4Econ/'
# if the path contains these skip

spn_skip <- c('summarize', 'panel', 'support')
spn_skip <- c('')

# Group A
spt_amto <- paste0(spt_root, 'amto')
spt_summ <- paste0(spt_root, 'summarize')
spt_func <- paste0(spt_root, 'function')
ls_path_group_a <- c(spt_amto, spt_summ, spt_func)

# Group B
spt_math <- paste0(spt_root, 'math')
spt_gent <- paste0(spt_root, 'generate')
spt_panl <- paste0(spt_root, 'panel')
ls_path_group_b <- c(spt_math, spt_panl)

# Group C
spt_linr <- paste0(spt_root, 'linreg')
spt_regn <- paste0(spt_root, 'regnonlin')
spt_opti <- paste0(spt_root, 'optimization')
spt_dyna <- paste0(spt_root, 'dynamic')
ls_path_group_c <- c(spt_opti, spt_regn, spt_linr)

# Group TEMP
spt_one <- paste0(spt_root, 'amto')
spt_two <- paste0(spt_root, 'summarize/dist')
spt_thr <- paste0(spt_root, 'summarize/aggregate')
spt_fou <- paste0(spt_root, 'function/noloop')
spt_fiv <- paste0(spt_root, 'math')
spt_six <- paste0(spt_root, 'panel')
ls_path_group_temp <- c(spt_one, spt_two, spt_thr)
ls_path_group_temp <- c(ls_path_group_temp, spt_fou, spt_fiv, spt_six)

# All ls_path_group_use
ls_path_group <- c(ls_path_group_a, ls_path_group_b, ls_path_group_c)

# Group To Use
ls_path_group_use <- ls_path_group
ls_path_group_use <- paste0(spt_root, '')
# ls_path_group_use <- ls_path_group_temp

# Get Path
ls_sfls <- list.files(path=ls_path_group_use, recursive=T, pattern=".Rmd", full.names=T)

# Excludes elements of path that have exclusion strings
if (spn_skip != '') {
  ls_sfls <- ls_sfls[!grepl(paste(spn_skip, collapse = "|"), ls_sfls)]
}

# print
for (spt_file in ls_sfls) {
  # 1. Check if the RMD file has been modified or is new, if neither, do not generate pdf html
  # 2. store pdf and html files in a subfolder
  # 3. main folder keeps only Rmd file
  # 4. delete tex and other files

  st_fullpath_noname <- dirname(spt_file)
  st_fullpath_nosufx <- sub('\\.Rmd$', '', spt_file)
  st_file_wno_suffix <- sub('\\.Rmd$', '', basename(spt_file))

  setwd(st_fullpath_noname)

  # Check if the RMD file has been modified or is new, if neither, do not generate pdf html
  spg_check_git_status <- paste0('git status -s ', spt_file)
  st_git_status <- toString(system(spg_check_git_status, intern=TRUE))
  bl_modified <- grepl(' M ', st_git_status, fixed=TRUE)
  bl_anewfile <- grepl('?? ', st_git_status, fixed=TRUE)
  bl_nochange <- (st_git_status == "")

  if (bl_modified == 1) {
    print(paste0('MODIFIED: ', spt_file))
  } else if (bl_anewfile == 1) {
    print(paste0('A NEW FL: ', spt_file))
  } else {
    print(paste0('NO CHNGE: ', spt_file))
  }

  if (bl_modified + bl_anewfile == 10) {
    print(paste0('spt_file:',spt_file))

    print(paste0('st_fullpath_noname:', st_fullpath_noname))
    print(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
    print(paste0('st_file_wno_suffix:', st_file_wno_suffix))

    spth_pdf_html <- paste0(st_fullpath_noname, '/htmlpdfr/')
    sfle_pdf_html <- paste0(st_fullpath_noname, '/htmlpdfr/', st_file_wno_suffix)
    print(spth_pdf_html)

    sfl_nht <- paste0(st_fullpath_nosufx, '.nb.html')
    sfl_tex <- paste0(st_fullpath_nosufx, '.tex')
    sfl_pdf <- paste0(st_fullpath_nosufx, '.pdf')
    sfl_htm <- paste0(st_fullpath_nosufx, '.html')
    sfl_Rla <- paste0(st_fullpath_nosufx, '.R')
    sfl_log <- paste0(st_fullpath_nosufx, '.log')

    sfl_sub_nht <- paste0(sfle_pdf_html, '.nb.html')
    sfl_sub_tex <- paste0(sfle_pdf_html, '.tex')

    if (grepl('_main', spt_file)) {

      # try(file.remove(paste0(st_fullpath_nosufx, '.pdf')))
      # try(file.remove(paste0(st_fullpath_nosufx, '.html')))

    } else {

      # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header = "C:/Users/fan/R4Econ/preamble.tex"))', output_dir = spth_pdf_html)
      # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header))', output_dir = spth_pdf_html)

      print(paste0('spt_file:',spth_pdf_html, ', PDF started'))
      rmarkdown::render(spt_file, output_format='pdf_document', output_dir = spth_pdf_html)
      print(paste0('spt_file:',spth_pdf_html, ', PDF finished'))

      print(paste0('spt_file:',spth_pdf_html, ', HTML started.'))
      rmarkdown::render(spt_file, output_format='html_document', output_dir = spth_pdf_html)
      print(paste0('spt_file:',spth_pdf_html, ', HTML finished.'))

      print(paste0('purl_to:', paste0(sfle_pdf_html, ".R")))
      knitr::purl(spt_file, output=paste0(sfle_pdf_html, ".R"), documentation = 1)

    }

    try(file.remove(sfl_nht))
    try(file.remove(sfl_tex))
    try(file.remove(sfl_pdf))
    try(file.remove(sfl_htm))
    try(file.remove(sfl_Rla))
    try(file.remove(sfl_log))

    try(file.remove(sfl_sub_nht))
    try(file.remove(sfl_sub_tex))

  }
}
