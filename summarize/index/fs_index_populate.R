#' ---
#' title: "R Example DPLYR Generate Sorted Index, Ordinal Deviation Negative and Positive Index, and Expand Value from Lowest Index to All Rows"
#' author: Fan Wang
#' output:
#'   pdf_document: default
#'   word_document: default
#'   html_document: default
#'   html_notebook: default
#' urlcolor: blue
#' always_allow_html: yes
#' ---
#' 
#' Go back to [fan](http://fanwangecon.github.io/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository, or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository.
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F----------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

#' 
## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(REconTools)
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
#' - dplyr mutate equals index
#' 
## -----------------------------------------------------------------------------
# Sort and generate variable equal to sorted index
df_iris <- iris %>% arrange(Sepal.Length) %>% 
              mutate(Sepal.Len.Index = row_number()) %>%
              select(Sepal.Length, Sepal.Len.Index, everything())

# Show results Head 10
df_iris %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#' 
#' ## Populate Value from Lowest Index to All other Rows
#' 
#' We would like to calculate for example the ratio of each individual's highest to the the person with the lowest height in a dataset. We first need to generated sorted index from lowest to highest, and then populate the lowest height to all rows, and then divide. 
#' 
#' 
#' *Search Terms*:
#' 
#' - r spread value to all rows from one row
#' - r other rows equal to the value of one row
#' - Conditional assignment of one variable to the value of one of two other variables
#' - dplyr mutate conditional
#' - dplyr value from one row to all rows
#' - dplyr mutate equal to value in another cell
#' 
#' *Links*:
#' 
#' - see: dplyr [rank](https://dplyr.tidyverse.org/reference/ranking.html)
#' - see: dplyr [case_when](https://dplyr.tidyverse.org/reference/case_when.html)
#' 
#' ### Short Method: mutate and min
#' 
#' We just want the lowest value to be in its own column, so that we can compute various statistics using the lowest value variable and the original variable.
#' 
## -----------------------------------------------------------------------------
# 1. Sort
df_iris_m1 <- iris %>% mutate(Sepal.Len.Lowest.all = min(Sepal.Length)) %>%
                select(Sepal.Length, Sepal.Len.Lowest.all, everything())
              

# Show results Head 10
df_iris_m1 %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#' 
#' ### Long Method: row_number and case_when
#' 
#' This is the long method, using row_number, and case_when. The benefit of this method is that it generates several intermediate variables that might be useful. And the key final step is to set a new variable (A=*Sepal.Len.Lowest.all*) equal to another variable's (B=*Sepal.Length*'s) value at the index that satisfies condition based a third variable (C=*Sepal.Len.Index*).
#' 
## -----------------------------------------------------------------------------
# 1. Sort
# 2. generate index
# 3. value at lowest index (case_when)
# 4. spread value from lowest index to other rows
# Note step 4 does not require step 3
df_iris_m2 <- iris %>% arrange(Sepal.Length) %>% 
              mutate(Sepal.Len.Index = row_number()) %>%
              mutate(Sepal.Len.Lowest.one = 
                       case_when(row_number()==1 ~ Sepal.Length)) %>%
              mutate(Sepal.Len.Lowest.all = 
                       Sepal.Length[Sepal.Len.Index==1]) %>%
              select(Sepal.Length, Sepal.Len.Index, 
                     Sepal.Len.Lowest.one, Sepal.Len.Lowest.all)
              

# Show results Head 10
df_iris_m2 %>% head(10) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#' # Generate Sorted Index based on Deviations
#' 
#' ## Generate Positive and Negative Index based on Ordered Deviation from some Number
#' 
#' There is a variable that is continuous, substract a number from this variable, and generate index based on deviations. Think of the index as generating intervals indicating where the value lies. 0th index indicates the largest value in sequence that is smaller than or equal to number $x$, 1st index indicates the smallest value in sequence that is larger than number $x$.
#' 
#' The solution below is a little bit convoluated and long, there is likely a much quicker way. The process below shows various intermediary outputs that help arrive at deviation index *Sepal.Len.Devi.Index* from initial sorted index *Sepal.Len.Index*.
#' 
#' *search*:
#' 
#' - dplyr arrange ignore na
#' - dplyr index deviation from order number sequence
#' - dplyr index below above
#' - dplyr index order below above value
#' 
## -----------------------------------------------------------------------------
# 1. Sort and generate variable equal to sorted index
# 2. Plus or minus deviations from some value
# 3. Find the zero, which means, the number closests to zero including zero from the negative side
# 4. Find the index at the highest zero and below deviation point
# 5. Difference of zero index and original sorted index
sc_val_x <- 4.65
df_iris_deviate <- iris %>% arrange(Sepal.Length) %>% 
              mutate(Sepal.Len.Index = row_number()) %>%
              mutate(Sepal.Len.Devi = (Sepal.Length - sc_val_x)) %>%
              mutate(Sepal.Len.Devi.Neg = 
                       case_when(Sepal.Len.Devi <= 0 ~ (-1)*(Sepal.Len.Devi))) %>%
              arrange((Sepal.Len.Devi.Neg), desc(Sepal.Len.Index)) %>%
              mutate(Sepal.Len.Index.Zero = 
                       case_when(row_number() == 1 ~ Sepal.Len.Index)) %>%
              mutate(Sepal.Len.Devi.Index = 
                       Sepal.Len.Index - Sepal.Len.Index.Zero[row_number() == 1]) %>%
              arrange(Sepal.Len.Index) %>%
              select(Sepal.Length, Sepal.Len.Index, Sepal.Len.Devi, 
                     Sepal.Len.Devi.Neg, Sepal.Len.Index.Zero, Sepal.Len.Devi.Index)
              
                
# Show results Head 10
df_iris_deviate %>% head(20) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

