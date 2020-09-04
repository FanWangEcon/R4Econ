## ----global_options, include = FALSE-----------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Serch Folder and skip list
spt_roots <- c('C:/Users/fan/R4Econ/amto', 'C:/Users/fan/R4Econ/summarize')
spn_skip <- c('summarize', 'panel', 'support')

# Search and get all Path
ls_sfls <- list.files(path=spt_roots, recursive=T, pattern=".Rmd", full.names=T)

# Skip path if contains words in skip list
if(!missing(spn_skip)) {
  ls_sfls <- ls_sfls[!grepl(paste(spn_skip, collapse = "|"), ls_sfls)]
}

# Loop and print
for (spt_file in ls_sfls) {
    st_fullpath_nosufx <- tail(strsplit(spt_file, "/")[[1]],n=1)
    print(paste0(spt_file, '---', st_fullpath_nosufx))
}


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Serch Folder and skip list
spt_roots <- c('C:/Users/fan/R4Econ/amto', 'C:/Users/fan/R4Econ/development')
spn_skip <- c('summarize', 'panel', 'support')
ls_sfls <- list.files(path=spt_roots, recursive=T, pattern=".Rmd", full.names=T)
if(!missing(spn_skip)) {
  ls_sfls <- ls_sfls[!grepl(paste(spn_skip, collapse = "|"), ls_sfls)]
}

# Loop and print
for (spt_file in ls_sfls) {
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
}


## ---- include=TRUE, eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------
## # Serch Folder and skip list
## spt_roots <- c('C:/Users/fan/R4Econ/development/inout/')
## spn_skip <- c('_main', '_file')
## ls_sfls <- list.files(path=spt_roots, recursive=T, pattern=".Rmd", full.names=T)
## if(!missing(spn_skip)) {
##   ls_sfls <- ls_sfls[!grepl(paste(spn_skip, collapse = "|"), ls_sfls)]
## }
## 
## # Loop and print
## for (spt_file in ls_sfls) {
##   spt_new <- paste0('_file/rmd/')
##   spn_new <- paste0(spt_new, sub('\\.Rmd$', '', basename(spt_file)), '_mod.Rmd')
##   print(spt_new)
##   print(spn_new)
## 
##   fileConn_rd <- file(spt_file, "r")
##   st_file_read <- readLines(fileConn_rd)
## 
##   fileConn_sr <- file(spn_new)
##   writeLines(st_file_read, fileConn_sr)
## 
##   close(fileConn_rd)
##   close(fileConn_sr)
## }


## ---- include=TRUE, eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------
## spn_file = '_file/rmd/fs_rmd_pdf_html_mod.Rmd'
## fileConn_sr <- file(spn_file)
## st_file <- readLines(fileConn_sr)
## # print(st_file)
## 
## st_search <- "html_document:
##     toc: true
##     number_sections: true
##     toc_float:
##       collapsed: false
##       smooth_scroll: false
##       toc_depth: 3
##     toc: true
##     number_sections: true
##     toc_float:
##       collapsed: false
##       smooth_scroll: false
##       toc_depth: 3"
## st_replace <- paste0("html_document:
##     toc: true
##     number_sections: true
##     toc_float:
##       collapsed: false
##       smooth_scroll: false
##       toc_depth: 3
##     toc: true
##     number_sections: true
##     toc_float:
##       collapsed: false
##       smooth_scroll: false
##       toc_depth: 3\n",
##                      "    toc: true\n",
##                      "    number_sections: true\n",
##                      "    toc_float:\n",
##                      "      collapsed: false\n",
##                      "      smooth_scroll: false\n",
##                      "      toc_depth: 3")
## st_file_updated <- gsub(x = st_file,
##                         pattern = st_search,
##                         replacement = st_replace)
## 
## st_search <- "../../"
## st_replace <- paste0("../../../../")
## st_file_updated <- gsub(x = st_file_updated,
##                         pattern = st_search,
##                         replacement = st_replace)
## 
## st_file_updated <- gsub(x = st_file_updated, pattern = '# ', replacement = '# ')
## st_file_updated <- gsub(x = st_file_updated, pattern = '## ', replacement = '## ')
## st_file_updated <- gsub(x = st_file_updated, pattern = '# ', replacement = '# ')
## 
## spn_file = '_file/rmd/fs_rmd_pdf_html_mod.Rmd'
## fileConn_sr <- file(spn_file)
## st_file <- writeLines(st_file_updated, fileConn_sr)
## 


## ---- include=TRUE, eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------
## # Specify Parameters
## ar_spt_root = c('C:/Users/fan/R4Econ/amto/array/', 'C:/Users/fan/R4Econ/math/integration')
## bl_recursive = TRUE
## st_rmd_suffix_pattern = "*.Rmd"
## ar_spn_skip <- c('basics', 'integrate', 'main', 'mesh')
## ls_bool_convert <- list(bl_pdf=TRUE, bl_html=TRUE, bl_R=TRUE)
## spt_out_directory <- 'C:/Users/fan/Downloads/_data'
## bl_verbose <- TRUE
## 
## # Get Path
## ls_sfls  <- list.files(path=ar_spt_root,
##                        recursive=bl_recursive,
##                        pattern=st_rmd_suffix_pattern,
##                        full.names=T)
## 
## # Exclude Some Files given ar_spn_skip
## if(!missing(ar_spn_skip)) {
##   ls_sfls <- ls_sfls[!grepl(paste(ar_spn_skip, collapse = "|"), ls_sfls)]
## }
## 
## # Loop over files
## for (spn_file in ls_sfls) {
## 
##   # Parse File Name
##   spt_file <- dirname(spn_file)
##   sna_file <- tools::file_path_sans_ext(basename(spn_file))
## 
##   # Output FIles
##   spn_file_pdf <- paste0(spt_file, sna_file, '.pdf')
##   spn_file_html <- paste0(spt_file, sna_file, '.html')
##   spn_file_R <- paste0(spt_file, sna_file, '.R')
## 
##   # render to PDF
##   if (ls_bool_convert$bl_pdf) {
##     if (bl_verbose) message(paste0('spn_file_pdf:',spn_file_pdf, ', PDF started'))
##     rmarkdown::render(spn_file, output_format='pdf_document',
##                       output_dir = spt_out_directory, output_file = sna_file)
##     if (bl_verbose) message(paste0('spn_file_pdf:',spn_file_pdf, ', PDF finished'))
##     spn_pdf_generated <- paste0(spt_out_directory, '/', spn_file_pdf)
##   }
## 
##   # render to HTML
##   if (ls_bool_convert$bl_html) {
##     if (bl_verbose) message(paste0('spth_html:',spn_file_html, ', HTML started.'))
##     rmarkdown::render(spn_file, output_format='html_document',
##                       output_dir = spt_out_directory, output_file = sna_file)
##     if (bl_verbose) message(paste0('spth_html:',spn_file_html, ', HTML finished.'))
##     spn_html_generated <- paste0(spt_out_directory, '/', spn_file_html)
##   }
## 
##   # purl to R
##   if (ls_bool_convert$bl_R) {
##     if (bl_verbose) message(paste0('purl_to:', paste0(spn_file_R, ".R")))
##     knitr::purl(spn_file, output=paste0(spt_out_directory, '/', sna_file, '.R'), documentation = 1)
##     spn_R_generated <- paste0(spt_out_directory, '/', sna_file, '.R')
##   }
## 
##   # return(list(ls_spt_pdf_generated=ls_spt_pdf_generated,
##   #             ls_spt_html_generated=ls_spt_html_generated,
##   #             ls_spt_R_generated=ls_spt_R_generated))
## 
## 
## }

