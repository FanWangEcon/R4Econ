# Knit all Rmd Files stored in folders
# I maintain both bookdown as well as individually compiled PDFs for each page
# This file finds all Rmd files in R4Eon and knits them all to pdf and html.
spt_root <- 'C:/Users/fan/R4Econ/'

# Group A
spt_summarize <- paste0(spt_root, 'summarize')
ls_path_group_a <- c(spt_summarize)

# Group B
spt_math <- paste0(spt_root, 'math')
spt_optimization <- paste0(spt_root, 'optimization')
ls_path_group_b <- c(spt_math, spt_optimization)

# Group To Use
ls_path_group_use <- ls_path_group_b
ls_path_group_use <- spt_summarize

# Get Path
ls_sfls  <- list.files(path=ls_path_group_use, recursive=T, pattern=".Rmd", full.names=T)

# print
for (spt_file in ls_sfls) {
  print(paste0('spt_file:',spt_file))
  rmarkdown::render(spt_file, 'pdf_document')
}

#
# rmarkdown::render("C:/Users/fan/R4Econ/summarize/aggregate/fs_group_unique_agg.Rmd", "html_document")
