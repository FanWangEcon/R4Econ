install.packages("installr")
library(installr)
updateR()

install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

# Install these Pacakges in R-Studio
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# devtools::install_github("r-lib/devtools")

# also install this package for testing etc generating docs etc
install.packages("pkgdown")
devtools::install_github("r-lib/pkgdown")

# Short-cuts
in rStudio
on function name, F2 to go to function
ctrl + F9 to go back

# Formatting
install.packages("formatR")

# in rstudio:
library(formatR)
sessionInfo()
tidy_source(width.cutoff = 50)
tidy_dir('C:/Users/fan/R4Econ/R')
tidy_dir('C:/Users/fan/R4Econ/R')
formatR::tidy_dir("C:/Users/fan/R4Econ/R", width.cutoff = 75)

# no R4Econ directory yet, create one, then put files in
devtools::create("C:/Users/fan/R4Econ")
devtools::create("C:/Users/fan/R4Econ")

# if there is already an existing repository: then use devtools to create a fake repository to generate DESCRIPTIon, NAMESPACE etc files, then replace folder name, and move over.


# put r files in /R folder
# to use files in /R folder3
devtools::load_all("C:/Users/fan/R4Econ")
devtools::document("C:/Users/fan/R4Econ")
devtools::install("C:/Users/fan/R4Econ")

pkgdown::build_site()
pkgdown::template_navbar()

require(R4Econ)
library(REconTools)

ff_summ_percentiles()

build_manual("C:/Users/fan/R4Econ")

devtools::build_vignettes(R4Econ)

devtools::release()

# https://github.com/r-lib/devtools/issues/587

There are three main ways to run roxygen:
roxygen2::roxygenise(), or
devtools::document(), if you’re using devtools, or
Ctrl + Shift + D, if you’re using RStudio.


to add package dependency:
# for piping
usethis::use_package("dplyr")
# for gather
usethis::use_package("tidyr")
# column_to_rownames
usethis::use_package("tibble")
# negate function to select only numeric columns
usethis::use_package("purrr")
# to read csv files
usethis::use_package("readr")

# In rStudio go to directory, set as working
setwd('C:/Users/fan/R4Econ')
setwd('C:/Users/fan/R4Econ/')
# Press ctrl + shift + L

untar("C:/Users/fan/R4Econ_0.0.0.9000.tar.gz", list=TRUE)


if want vignetttes, have to write a RMD file for that!

#------------ Having created package
1. open existing packagein Rstudio, File, Open Project
2. top right, Environment, History Connections, and Build.
    - under build, more, clean and rebuilt
    - ask to install additional programs.

- Write and modify files, then:
    + Ctrl + Shift + D
- Generate Documentations
    + devtools::check(manual=TRUE)
    + devtools::build(vignettes = T, manual = T)
    # R CMD check is the name of the command you run from the terminal. I don’t recommend calling it directly. Instead, run devtools::check(), or press Ctrl/Cmd + Shift + E in RStudio. In contrast to R CMD check, devtools::check():
    #


Someone with knowledge of how devtools does this will have to answer then. If you are comfortable just using the command line, you can go to the directory above the package directory, and do R CMD build mypackage, then R CMD check mypackage_1.0.0.tar.gz and then R CMD install mypackage_1.0.0.tar.gz and assuming there are no errors at each step, you should have what you want. – Bryan Hanson May 1 '15 at 8:28


R CMD Rd2pdf R4Econ
