# Install these Pacakges in R-Studio
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

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
devtools::load_all()
devtools::document()
devtools::install()

# In rStudio go to directory, set as working
setwd('C:/Users/fan/R4Econ')
setwd('C:/Users/fan/faneconr/')
# Press ctrl + shift + L
