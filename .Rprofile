# rm(list = ls(all.names = TRUE))
# Load Libraries
library(tidyverse)
library(tidyr)
library(forcats)
library(knitr)
library(kableExtra)
library(REconTools)

# formatR needed for tidy.opts below
library(formatR)

# jointly use R and Python Together

# RMD Options
options(knitr.duplicate.label = "allow")
knitr::opts_chunk$set(fig.width=7, fig.height=4, fig.align="center")
# knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)
knitr::opts_chunk$set(warning=FALSE, message=FALSE, cache=FALSE)

# Output HTML or Latex
if (knitr::is_latex_output()) {
  options(knitr.table.format = "latex")
} else {
  options(knitr.table.format = "html")
}

# Table Output Options
kable_styling_fc = function(kable_input){
  kable_styling(kable_input,
    bootstrap_options = c("striped", "hover", "responsive"),
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    fixed_thead = T,
    position = "center",
    font_size = NULL,
    row_label_position = "l")
}

# Table Output Options:
# 1. scale_down for TEX
# 2. box width: see R4Econ\style.css for body width, set width to bodywidth - 225
if (knitr::is_latex_output()) {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l")
  }
} else {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l") %>%
    scroll_box(width = "875px")
  }
}

# Get Current File Path
spt_file_current <- knitr::current_input(dir = TRUE)
print(paste0('spt_file_current:',spt_file_current))
spt_file_current <- gsub(x = spt_file_current,  pattern = "_mod.Rmd", replacement = ".Rmd")

if(!is.null(spt_file_current)) {
  sfc_prj='/R4Econ'
  sph_gitpages_root='https://fanwangecon.github.io/'
  sph_github_root='https://github.com/FanWangEcon/'
  sph_branch='/master'
  sph_pdf='/htmlpdfr'
  sph_html='/htmlpdfr'
  sph_r='/htmlpdfr'

  spt_root <- 'C:/Users/fan/R4Econ/'
  spn_prj_rmd <- gsub(spt_root, "", spt_file_current)
  spt_rmd_path <- paste0('/',dirname(spn_prj_rmd))

  st_fullpath_noname <- dirname(spt_file_current)
  st_fullpath_nosufx <- sub('\\.Rmd$', '', spt_file_current)
  st_file_wno_suffix <- sub('\\.Rmd$', '', basename(spt_file_current))
  print(paste0('st_fullpath_noname:', st_fullpath_noname))
  print(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
  print(paste0('st_file_wno_suffix:', st_file_wno_suffix))

  spth_pdf_html <- paste0(st_fullpath_noname, '/htmlpdfr/')
  sfle_pdf_html <- paste0(st_fullpath_noname, '/htmlpdfr/', st_file_wno_suffix)
  print(spth_pdf_html)

  sph_source_blob_root = paste0(sph_github_root, sfc_prj, '/blob', sph_branch, spt_rmd_path, '/')
  sph_rmd_pdf = paste0(sph_source_blob_root, sph_pdf, '/', st_file_wno_suffix, '.pdf')
  sph_rmd_r = paste0(sph_source_blob_root, sph_r, '/', st_file_wno_suffix, '.R')
  sph_rmd_rmd = paste0(sph_source_blob_root, '/', st_file_wno_suffix, '.Rmd')

  sph_source_web_root = paste0(sph_gitpages_root, sfc_prj, spt_rmd_path, '/')
  sph_rmd_html = paste0(sph_source_web_root, sph_html, '/', st_file_wno_suffix, '.html')

  st_head_link = '> Go to the'
  st_head_link = paste0(st_head_link, ' [**RMD**](', sph_rmd_rmd ,'),')
  st_head_link = paste0(st_head_link, ' [**R**](', sph_rmd_r ,'),')
  st_head_link = paste0(st_head_link, ' [**PDF**](', sph_rmd_pdf ,'),')
  st_head_link = paste0(st_head_link, ' or [**HTML**](', sph_rmd_html ,')')
  st_head_link = paste0(st_head_link, ' version of this file.')

  # Common Shared Text and Strings
  total_area <- (800 * 7) / 2
  if (st_file_wno_suffix == 'Panel-Data-and-Optimization-with-R') {
    text_shared_preamble_one <- paste0("> Go back to [fan](http://fanwangecon.github.io/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R Code Examples](https://fanwangecon.github.io/R4Econ/) Repository ([bookdown site](https://fanwangecon.github.io/R4Econ/bookdown)), or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository ([bookdown site](https://fanwangecon.github.io/Stat4Econ/bookdown)).")
  } else {
    text_shared_preamble_one <- paste0(st_head_link, " Go back to [fan](http://fanwangecon.github.io/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R Code Examples](https://fanwangecon.github.io/R4Econ/) Repository ([bookdown site](https://fanwangecon.github.io/R4Econ/bookdown)), or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository ([bookdown site](https://fanwangecon.github.io/Stat4Econ/bookdown)).")
  }
}

text_shared_preamble_two <- ""
text_shared_preamble_thr <- ""

if (knitr::is_latex_output()) {
    text_top_count <- ""
    text_end_count <- ""
} else {
    text_top_count <- "[![HitCount](http://hits.dwyl.io/fanwangecon/R4Econ.svg)](https://github.com/FanWangEcon/R4Econ)  [![Star](https://img.shields.io/github/stars/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/watchers)"
    text_end_count <- "[![](https://img.shields.io/github/last-commit/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/pulls)"
}
