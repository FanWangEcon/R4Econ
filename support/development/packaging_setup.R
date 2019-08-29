install.packages("installr")
library(installr)
updateR()


install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

# Install these Pacakges in R-Studio
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
devtools::install_github("r-lib/devtools")

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
tidy_dir('C:/Users/fan/faneconr/R')
formatR::tidy_dir("C:/Users/fan/R4Econ/R", width.cutoff = 75)

# no faneconr directory yet, create one, then put files in
devtools::create("C:/Users/fan/faneconr")

# put r files in /R folder
# to use files in /R folder3
devtools::load_all("C:/Users/fan/faneconr")
devtools::document("C:/Users/fan/faneconr")
devtools::install("C:/Users/fan/faneconr")
library(devtools)
build_manual("C:/U`sers/fan/faneconr")

There are three main ways to run roxygen:
roxygen2::roxygenise(), or
devtools::document(), if you’re using devtools, or
Ctrl + Shift + D, if you’re using RStudio.



# In rStudio go to directory, set as working
setwd('C:/Users/fan/R4Econ')
setwd('C:/Users/fan/faneconr/')
# Press ctrl + shift + L

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


R CMD Rd2pdf faneconr
