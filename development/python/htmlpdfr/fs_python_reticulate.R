## ----global_options, include = FALSE---------------------------------------------------------------
try(source("../../.Rprofile"))


## 1+1


## --------------------------------------------------------------------------------------------------
Sys.which('python')


## --------------------------------------------------------------------------------------------------
# Sys.setenv(RETICULATE_PYTHON = "C:/programdata/Anaconda3/python.exe")
# Sys.setenv(RETICULATE_PYTHON = "C:/ProgramData/Anaconda3/envs/wk_pyfan/python.exe")
library(reticulate)
# What is the python config
py_config()
# set python
# use_python("C:/programdata/Anaconda3/python.exe")
# use_python("C:/ProgramData/Anaconda3/envs/wk_pyfan/python.exe")
use_condaenv('wk_pyfan')
# Sys.which('python')
py_run_string('print(1+1)')

