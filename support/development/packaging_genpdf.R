# Should be able to do this directly in Rstudio, can not for some reason.
#
# So do this command line.
#


# 1. go to directory in Rstudio
setwd("C:/Users/fan/faneconr/")

# 2. clean and compile the man folder .Rd Files
Press Ctrl Shift D

# 3a. generate pdf documentation on command line
cd "C:/Users/fan/"
R CMD Rd2pdf faneconr
R CMD Rd2pdf "C:/Users/fan/faneconr"

# 3b. generate pdf documentation inside RStudio
devtools::check(manual=TRUE)
