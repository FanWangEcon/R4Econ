## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---- amto.array.fs_ary_string.replace, eval=FALSE-----------------------------------------------------
## # String replacement
## gsub(x = paste0(unique(df.slds.stats.perc$it.inner.counter), ':',
##                 unique(df.slds.stats.perc$z_n_a_n), collapse = ';'),
##      pattern = "\n",
##      replacement = "")
## gsub(x = var,  pattern = "\n", replacement = "")
## gsub(x = var.input,  pattern = "\\.", replacement = "_")


## ------------------------------------------------------------------------------------------------------
st_tex_text <- "\n% Lat2ex Comments\n\\newcommand{\\exa}{\\text{from external file: } \\alpha + \\beta}\n% More LaLatex Comments\n"
st_clean_a1 <- gsub("\\%.*?\\\n", "", st_tex_text)
st_clean_a2 <- gsub("L.*?x", "[LATEX]", st_tex_text)
print(paste0('st_tex_text:', st_tex_text))
print(paste0('st_clean_a1:', st_clean_a1))
print(paste0('st_clean_a2:', st_clean_a2))


## ------------------------------------------------------------------------------------------------------
st_tex_text <- "\\end{equation}\n}\n% Even more comments from Latex preamble"
st_clean_a1 <- gsub("\\\n%.*","", st_tex_text)
print(paste0('st_tex_text:', st_tex_text))
print(paste0('st_clean_a1:', st_clean_a1))


## ----support dtype string if contains------------------------------------------------------------------
st_example_a <- 'C:/Users/fan/R4Econ/amto/tibble/fs_tib_basics.Rmd'
st_example_b <- 'C:/Users/fan/R4Econ/amto/tibble/_main.html'
grepl('_main', st_example_a)
grepl('_main', st_example_b)


## ------------------------------------------------------------------------------------------------------
ls_spn <- c("C:/Users/fan/R4Econ//panel/basic/fs_genpanel.Rmd",
            "C:/Users/fan/R4Econ//summarize/aggregate/main.Rmd",
            "C:/Users/fan/R4Econ//summarize/index/fs_index_populate.Rmd")
ls_str_if_contains <- c("aggregate", "index")
str_if_contains <- paste(ls_str_if_contains, collapse = "|")
grepl(str_if_contains, ls_spn)


## ------------------------------------------------------------------------------------------------------
# Extract 0.216 and 0.500 as lower and upper bounds
st_cut_cate <- '(0.216,0.500]'
# Extract Lower Part
substring(strsplit(st_cut_cate, ",")[[1]][1], 2)
# Extract second part except final bracket Option 1
intToUtf8(rev(utf8ToInt(substring(intToUtf8(rev(utf8ToInt(strsplit(st_cut_cate, ",")[[1]][2]))), 2))))
# Extract second part except final bracket Option 2
gsub(strsplit(st_cut_cate, ",")[[1]][2],  pattern = "]", replacement = "")


## ----amto.array.fs_ary_string.concatenate--------------------------------------------------------------
# Simple Collapse
vars.group.by <- c('abc', 'efg')
paste0(vars.group.by, collapse='|')


## ----amto.array.fs_ary_string.leadingzero--------------------------------------------------------------
# Add Leading zero for integer values to allow for sorting when
# integers are combined into strings
it_z_n <- 1
it_a_n <- 192
print(sprintf("%02d", it_z_n))
print(sprintf("%04d", it_a_n))


## ------------------------------------------------------------------------------------------------------
snm_full <- "20100701"
snm_year <-substr(snm_full,0,4)
snm_month <-substr(snm_full,5,6)
snm_day <-substr(snm_full,7,8)
print(paste0('full:', snm_full,
             ', year:', snm_year,
             ', month:', snm_month,
             ', day:', snm_day))

