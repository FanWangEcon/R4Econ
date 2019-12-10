#' ---
#' title: "R Example DPLYR Generated Sorted Index and Expand Value from an Index to All Rows"
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
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F-----------------------------------------------------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

#' 
## ----loadlib, echo = T, results = 'hide', message=F, warning=F-----------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(R4Econ)
# file name
st_file_name = 'fs_index_populate'
# Generate R File
purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2)
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/summarize/index/fs_index_populate.Rmd", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/summarize/index/fs_index_populate.Rmd", "html_document")

#' 
#' # Generate Sorted Index within Group and Spread
#' 
#' ## Generate Sorted Index within Group with Repeating Values
#' 
#' There is a variable, sort by this variable, then generate index from 1 to N representing sorted values of this index. If there are repeating values, still assign index, different index each value. 
#' 
#' - r generate index sort
#' 
## ------------------------------------------------------------------------------------------------------------------------
# dataset subsetting
df_iris <- iris %>% arrange(Sepal.Length) %>% mutate(Sepal.Length.Index = row_number())

# Show results Head 10
df_iris %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#' 
#' ## Aggrgate Groups only Unique Group Show up With Means
#' 
#' Several variables that are grouping identifiers. Several variables that are values which mean be unique for each group members. For example, a Panel of income for N households over T years with also household education information that is invariant over time. Want to generate a dataset where the unit of observation are households, rather than household years. Take average of all numeric variables that are household and year specific.
#' 
#' A complicating factor potentially is that the number of observations differ within group, for example, income might be observed for all years for some households but not for other households.
#' 
#' - r dplyr aggregate group average
#' - Aggregating and analyzing data with dplyr
#' - column can't be modified because it is a grouping variable
#' - see also: [Aggregating and analyzing data with dplyr](https://datacarpentry.org/dc_zurich/R-ecology/04-dplyr.html)
#' 
## ------------------------------------------------------------------------------------------------------------------------
# In the df_hgt_wgt from R4Econ, there is a country id, village id,
# and individual id, and various other statistics
vars.group <- c('S.country', 'vil.id', 'indi.id')
vars.values <- c('hgt', 'momEdu')

# dataset subsetting
df_use <- df_hgt_wgt %>% select(!!!syms(c(vars.group, vars.values)))

# Group, count and generate means for each numeric variables
df.group <- df_use %>% group_by(!!!syms(vars.group)) %>%
            arrange(!!!syms(vars.group)) %>%
            summarise_if(is.numeric,
                         funs(mean = mean(., na.rm = TRUE),
                              sd = sd(., na.rm = TRUE),
                              n = sum(is.na(.)==0)))

# Show results Head 10
df.group %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
# Show results Head 10
df.group %>% tail(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

