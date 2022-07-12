## ----global_options, include = FALSE------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------
ls_spn_paths <- .libPaths()
print(ls_spn_paths)
# C:/Users/fan/Documents/R/win-library/3.6
# C:/Program Files/R/R-3.6.1/library


## # Show All installed environments

## conda info --envs


## # Exit Conda

## conda deactivate

## # where is R installed outside of Conda

## which R

## # /usr/bin/R

## # To remove all

## sudo apt-get remove r-base

## sudo apt-get remove r-base-core

## 

## # Inside Conda base

## conda activate

## # Conda r_env

## conda activate r_envr

## # Where is it installed?

## which R

## # /home/wangfanbsg75/anaconda3/bin/R

## conda uninstall r-base


## # Go to get latesdebian latest r sources.list

## cat /etc/apt/sources.list

## # Install this First (should already be installed)

## sudo apt install dirmngr

## 

## # Debian R is maintained by Johannes Ranke, copied from https://cran.r-project.org/bin/linux/debian/:

## apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'

## # Add to source.list, for debian stretch (9)

## # sudo su added for security issue as super-user

## sudo su -c "sudo echo 'deb http://cloud.r-project.org/bin/linux/debian stretch-cran35/' >> /etc/apt/sources.list"

## # if added wrong lines, delete 3rd line

## sudo sed '3d' /etc/apt/sources.list

## 

## # Update and Install R, should say updated from cloud.r

## sudo apt-get update

## sudo apt-get install r-base r-base-dev

## 

## # Also install these, otherwise r-packages do not install

## # libxml2 seems need for tidymodels

## sudo apt-get install libcurl4-openssl-dev

## sudo apt-get install libssl-dev

## sudo apt-get install libxml2-dev


## ---- eval = FALSE------------------------------------------------------------------
## # https://www.r-project.org/nosvn/pandoc/installr.html
## install.packages('installr')
## # update R from inside R (not Rstudio)
## require(installr)
## # this will open dialog boxes to take you through the steps.
## updateR()
## # Set Rstudio to the Latest R


## ---- eval = FALSE------------------------------------------------------------------
## ls_spn_paths <- .libPaths()
## print(ls_spn_paths)
## # "C:/Users/fan/AppData/Local/R/win-library/4.2"
## # "C:/Program Files/R/R-4.2.1/library"


## ---- eval = FALSE------------------------------------------------------------------
## # To exit command line:
## q()


## ---- eval = FALSE------------------------------------------------------------------
## ls_spn_paths <- .libPaths()
## print(ls_spn_paths)
## # [1] "C:/Users/fan/AppData/Local/R/win-library/4.2" "C:/Program Files/R/R-4.2.1/library"
## ls_spn_paths <- c(ls_spn_paths[2], ls_spn_paths[1])
## .libPaths(ls_spn_paths)
## ls_spn_paths <- .libPaths()
## print(ls_spn_paths)
## # [1] "C:/Program Files/R/R-4.2.1/library"           "C:/Users/fan/AppData/Local/R/win-library/4.2"


## ---- eval = FALSE------------------------------------------------------------------
## # Install RTools First!
## # https://cran.r-project.org/bin/windows/Rtools/
## 
## # Install system tools
## install.packages(c("backports"))
## 
## # Install tidyverse
## install.packages(c("tidyverse", "tidymodels", "vroom"))
## 
## # Install Packaging tools
## install.packages(c("devtools", "pkgdown", "roxygen2", "bookdown", "knitr", "kableExtra", "formatR", "revealjs"))
## 
## # Install Statistics models
## install.packages(c("AER", "minpack.lm"))
## install.packages(c("quantreg"))
## 
## # Install Tools to Work with Other Packages
## # matconv: converts matlab programs to R
## install.packages(c("reticulate", "JuliaCall", "matconv"))
## install.packages(c("matconv"))
## # for reticulate errors, install directly from: devtools::install_github("rstudio/reticulate")
## 
## # Install Paralell Tools
## install.packages(c("parallel", "doParallel", "foreach"))
## 
## # Install personal Packages
## devtools::install_github("fanwangecon/REconTools")
## devtools::install_github("fanwangecon/PrjOptiAlloc")
## 
## # Stata in Rmd
## # devtools::install_github("Hemken/Statamarkdown")
## 
## # VScode integration and also sublime r-ide
## install.packages("languageserver")


## ---- eval = FALSE------------------------------------------------------------------
## # 2020-10-19
## # Temp install development version due to but
## # https://github.com/rstudio/reticulate/issues/831
## devtools::install_github("rstudio/reticulate")


## ---- eval = FALSE------------------------------------------------------------------
## # # A simple file with summary statistics using tidyverse
## # source('C:/Users/fan/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.R')
## # source('G:/repos/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.R')
## # # Another simple file with summary statistics using tidyverse
## # source('C:/Users/fan/R4Econ/support/tibble/htmlpdfr/fs_tib_basics.R')
## # source('G:/repos/R4Econ/support/tibble/htmlpdfr/fs_tib_basics.R')
## # # A file involving estimation
## # source('C:/Users/fan/R4Econ/optimization/cesloglin/htmlpdfr/fst_ces_plan_linlog.R')
## #
## # C:/Users/fan/R4Econ/summarize/dist/fst_hist_onevar.Rmd
## # C:/Users/fan/R4Econ/support/tibble/fs_tib_basics.Rmd
## # C:/Users/fan/R4Econ/optimization/cesloglin/fst_ces_plan_linlog.Rmd

