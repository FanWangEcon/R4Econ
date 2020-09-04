#################################################
# ------------ Parameters
#################################################

# Where to store everything
spt_root <- "C:/Users/fan/Downloads/_data/"
spth_conda_env <- "C:/ProgramData/Anaconda3/envs/wk_ecmwf/python.exe"
# nc name prefix
st_era5_prefix <- "ECMWF_ERA5_"

ar_days <- c('01', '02')
ar_hours <- c('02:00', '12:00')
# # Whole of China File Very Large
# ls_coordiantes_area <- '53.31, 73, 4.15, 135'
# This is Guangdong only?
ls_coordiantes_area <- '23.50, 113.00, 22.25, 114.50'

# Years list
# ar_years <- 2001:2019
ar_years <- c(2005, 2015)
# ar_months_g1 <- c('01','02','03','04','05','06')
ar_months_g1 <- c('01', '03')
# ar_months_g2 <- c('07','08','09','10','11','12')
ar_months_g2 <- c('07', '09')


# folder to download any nc zips to
gribpath <- spt_root
# we are changing the python api file with different requests stirngs and storing it here
pyapipath <- spt_root
# output directory for AGGREGATE CSV with all DATES from this search
csvpath <- spt_root


#################################################
# ------------ Hard Coded Variables to Acquire
#################################################

# do not change the list, this is hard coded in below
# the variables have 3 letter acronyms: u10, v10, d2m, t2m, msl, sp, these are hard-coded
ls_st_era5_single_level_var <- c('10m_u_component_of_wind', '10m_v_component_of_wind', '2m_dewpoint_temperature',
                                 '2m_temperature', 'mean_sea_level_pressure', 'surface_pressure')

#################################################
# ------------ Packages
#################################################

library("tibble")
library("dplyr")
Sys.setenv(RETICULATE_PYTHON = spth_conda_env)
library("reticulate")

#################################################
# ------------ Define Loops
#################################################
for (it_yr in ar_years) {
  for (it_mth_group in c(1,2)) {
    if(it_mth_group == 1) {
      ar_months = ar_months_g1
    }
    if(it_mth_group == 2) {
      ar_months = ar_months_g2
    }

    #################################################
    # ------------ Define Python API Call
    #################################################

    st_file <- paste0("import os
import cdsapi
import cfgrib
import xarray as xr
import pandas as pd

# A. Folders
spt_root = '", gribpath, "'
snm_data = 'china_temp'
snm_data_grib = snm_data + '.grib'
snm_data_csv_raw = snm_data + '_raw.csv'
snm_data_csv = snm_data + '.csv'
spn_dl_test_grib = spt_root + snm_data_grib

os.chdir(spt_root)

# B. API Request
c = cdsapi.Client()
res = c.retrieve(
  'reanalysis-era5-single-levels',
  {
      'product_type': 'reanalysis',
      'variable': [
          ", paste("'", ls_st_era5_single_level_var, "'", sep = "", collapse = ", "), "
      ],
      'year': '",it_yr, "',
      'month': [
          ", paste("'", ar_months, "'", sep = "", collapse = ", "), "
      ],
      'day': [
          ", paste("'", ar_days, "'", sep = "", collapse = ", "), "
      ],
      'time': [
          ", paste("'", ar_hours, "'", sep = "", collapse = ", "), "
      ],
	'area'  : [",ls_coordiantes_area,"],
	'grid'  : [0.25, 0.25],
	'format': 'grib',
  },
spn_dl_test_grib)

# C. show Progress
print('print results')
print(res)
print(type(res))

# D. use cfgrib to read downloaded grib file
dsxr = xr.load_dataset(snm_data_grib, engine='cfgrib')
pd.concat([dsxr['u10'].to_series(), dsxr['v10'].to_series(),
           dsxr['d2m'].to_series(), dsxr['t2m'].to_series(),
           dsxr['msl'].to_series(), dsxr['sp'].to_series()],
          axis=1).to_csv(snm_data_csv, index=True)
")

    # Store Python Api File
    fl_test_tex <- paste0(pyapipath, "api.py")
    fileConn <- file(fl_test_tex)
    writeLines(st_file, fileConn)
    close(fileConn)

    #################################################
    # ------------ API Call
    #################################################
    # Set Path
    # conda activate wk_ecmwf
    # python "C:/Users/fan/Downloads/_data/api.py"
    setwd(pyapipath)
    # Run py file, api.py name just defined
    use_python(spth_conda_env)
    source_python('api.py', convert=FALSE)
    print('API call complete')

    # #################################################
    # # ------------ CSV File Long To Wide
    # #################################################
    # # Load in CSV File and Convert Variables Long to Wide to reduce storage costs
    # df_china_temp <- as_tibble(read.csv(paste0(spt_root, 'china_temp.csv')))
    # df_china_temp_wider <- df_china_temp %>%
    #   pivot_wider(names_from = variable, values_from = val) %>%
    #   mutate(hours = hours/100) %>%
    #   select(lon, lat, hours, date, everything())
    #
    # ar_date <- df_china_temp_wider %>% pull(date)
    # snm_start_date <- min(ar_date)
    # snm_end_date <- max(ar_date)
    #
    # # Save File
    # st_fname <- paste0(paste0(st_era5_prefix,
    #                        snm_start_date, "_to_", snm_end_date,
    #                        ".csv"))
    # csvfile <- paste0(csvpath, st_fname)
    # write.table(na.omit(df_china_temp_wider), csvfile, row.names = FALSE, sep = ",")

  # end loop months groups
  }
# end loop year
}

# Delete folders
unlink(paste0(spt_root, 'china_temp.csv'), force=TRUE)
unlink(paste0(spt_root, 'china_temp_raw.csv'), force=TRUE)
unlink(paste0(spt_root, 'china_temp.grib'), force=TRUE)

