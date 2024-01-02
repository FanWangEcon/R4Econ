## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## ---- eval=F, echo=T------------------------------------------------------------------
## install.packages(c("parallel", "doParallel", "foreach"))


## ---- eval = FALSE--------------------------------------------------------------------
## # Load libraries
## library(dplyr)
## library(readr)
## library(tibble)
## library(iterators)
## library(parallel)
## library(foreach)
## library(doParallel)
## 
## # Check number of cores
## it_n_cores_computer <- parallel::detectCores()
## glue::glue("Number of cores on computers:{it_n_cores_computer}")
## 
## # OUTPUT
## ## Number of cores on computers:20


## ---- eval = FALSE--------------------------------------------------------------------
## # Start cluster
## ob_cluster <- parallel::makeCluster(
##   it_n_cores_computer - 2,
##   type = "PSOCK"
##   )
## # Register cluster
## doParallel::registerDoParallel(cl = ob_cluster)


## ---- eval=FALSE----------------------------------------------------------------------
## # c(a,b,c,d) outputs together with combine
## ar_test_parallel <- foreach(
##   it_power = seq(1, 10), .combine = 'c'
## ) %dopar% {
##   return(10^(it_power))
## }
## glue::glue("dopar outputs: {ar_test_parallel}")
## 
## # Output
## ## dopar outputs: 10
## ## dopar outputs: 100
## ## dopar outputs: 1000
## ## dopar outputs: 10000
## ## dopar outputs: 1e+05
## ## dopar outputs: 1e+06
## ## dopar outputs: 1e+07
## ## dopar outputs: 1e+08
## ## dopar outputs: 1e+09
## ## dopar outputs: 1e+10


## ---- eval=FALSE----------------------------------------------------------------------
## parallel::stopCluster(cl = ob_cluster)


## ---- eval=FALSE----------------------------------------------------------------------
## ffi_rand2csv <- function(
##     spt_path_out,
##     it_nrow = 3,
##     st_file_prefix = "prefix") {
## 
##     # Generate a matrix and tibble
##     mt_rnorm_a <- matrix(
##       rnorm(it_nrow*3, mean=0, sd=1),
##       nrow=it_nrow, ncol=3)
##     tb_test <- tibble::as_tibble(mt_rnorm_a)
## 
##     # File output path
##     spn_output_file <- file.path(
##       spt_path_out,
##       paste0(st_file_prefix, '_nrow', it_nrow, '.csv'),
##       fsep = .Platform$file.sep)
## 
##     # Write file out
##     readr::write_csv(tb_test, spn_output_file)
##     print(glue::glue(
##       "File saved successfully: ", spn_output_file))
## 
## }


## ---- eval=FALSE----------------------------------------------------------------------
## # Get the number of cores
## it_n_cores_computer <- parallel::detectCores()
## glue::glue("Number of cores on computers:{it_n_cores_computer}")
## # Start cluster
## ob_cluster <- parallel::makeCluster(
##   it_n_cores_computer - 2,
##   type = "PSOCK"
##   )
## # Register cluster
## doParallel::registerDoParallel(cl = ob_cluster)
## 
## # OUTPUT
## ## Number of cores on computers:20


## ---- eval=FALSE----------------------------------------------------------------------
## # Define shared Path
## 
## spt_root <- "C:/Users/fan/"
## spt_rmd <- "R4Econ/development/parallel/_file/"
## spt_path_out <- file.path(spt_root, spt_rmd, fsep = .Platform$file.sep)
## 
## # Parallel Run
## foreach(
##   it_nrow = seq(2, 4)
## ) %dopar% {
##   # Run function
##   ffi_rand2csv(
##     spt_path_out,
##     it_nrow = it_nrow,
##     st_file_prefix = "ffi_para_test")
## }
## 
## # Output
## ## [[1]]
## ## File saved successfully: C:/Users/fan//R4Econ/development/parallel/_file/ffi_para_test_nrow2.csv
## ##
## ## [[2]]
## ## File saved successfully: C:/Users/fan//R4Econ/development/parallel/_file/ffi_para_test_nrow3.csv
## ##
## ## [[3]]
## ## File saved successfully: C:/Users/fan//R4Econ/development/parallel/_file/ffi_para_test_nrow4.csv


## ---- eval=F, echo=T------------------------------------------------------------------
## # Some path
## spt_path_data <- "C:/Users/fan/"
## # Parallel Run
## foreach(
##   fl_temp_bound = seq(-40, 40, by=1)
## ) %dopar% {
##   # Run function
##   ffp_demo_loc_env_inequality(
##     spt_path_data,
##     fl_temp_bound=fl_temp_bound)
## }

