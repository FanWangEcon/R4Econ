## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---- amto.array.fs_ary_string.replace, eval=FALSE-------------------------------------------------------------------------------------------------------
## # String replacement
## gsub(x = paste0(unique(df.slds.stats.perc$it.inner.counter), ':',
##                 unique(df.slds.stats.perc$z_n_a_n), collapse = ';'),
##      pattern = "\n",
##      replacement = "")
## gsub(x = var,  pattern = "\n", replacement = "")
## gsub(x = var.input,  pattern = "\\.", replacement = "_")


## ----support dtype string if contains--------------------------------------------------------------------------------------------------------------------
st_example_a <- 'C:/Users/fan/R4Econ/amto/tibble/fs_tib_basics.Rmd'
st_example_b <- 'C:/Users/fan/R4Econ/amto/tibble/_main.html'
grepl('_main', st_example_a)
grepl('_main', st_example_b)


## ----amto.array.fs_ary_string.concatenate----------------------------------------------------------------------------------------------------------------
# Simple Collapse
vars.group.by <- c('abc', 'efg')
paste0(vars.group.by, collapse='|')


## ----amto.array.fs_ary_string.leadingzero----------------------------------------------------------------------------------------------------------------
# Add Leading zero for integer values to allow for sorting when
# integers are combined into strings
it_z_n <- 1
it_a_n <- 192
print(sprintf("%02d", it_z_n))
print(sprintf("%04d", it_a_n))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
st_example <- 'C:/Users/fan/R4Econ/amto/tibble/fs_tib_basics.Rmd'
st_file_wth_suffix <- tail(strsplit(st_example, "/")[[1]],n=1)
st_file_wno_suffix <- sub('\\.Rmd$', '', basename(st_example))
st_fullpath_nosufx <- sub('\\.Rmd$', '', st_example)
st_lastpath_noname <- (dirname(st_example))
st_fullpath_noname <- dirname(st_example)

print(strsplit(st_example, "/"))
print(st_file_wth_suffix)
print(st_file_wno_suffix)
print(st_fullpath_nosufx)
print(st_lastpath_noname)
print(st_fullpath_noname)
