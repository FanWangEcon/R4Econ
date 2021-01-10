## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# can search in multiple paths, second path here has no relevant contents
spt_roots <- c('C:/Users/fan/R4Econ/panel/basic/_file/csv',
               'C:/Users/fan/R4Econ/panel/basic/_file/tex')
# can skip file names with certain strings
spn_skip <- c('A3420')
# prefix search patther,
st_search_str <- 'solu_19E1NEp99r99x_ITG_PE_cev_*'

# Search and get all Path
ls_sfls <- list.files(path=spt_roots,
                      recursive=T,
                      pattern=st_search_str,
                      full.names=T)

# Skip path if contains words in skip list
if(!missing(spn_skip)) {
  ls_sfls <- ls_sfls[!grepl(paste(spn_skip, collapse = "|"), ls_sfls)]
}


## ------------------------------------------------------------------------------------------------------
# Loop and print found files
it_folders_names_to_keep = 2
for (spt_file in ls_sfls) {
    ls_srt_folders_name_keep <- tail(strsplit(spt_file, "/")[[1]], n=it_folders_names_to_keep+1)
    snm_file_name <- tail(ls_srt_folders_name_keep, 1)
    ls_srt_folders_keep <- head(ls_srt_folders_name_keep, it_folders_names_to_keep)
    print(paste0('path:', spt_file))
    print(snm_file_name)
    print(ls_srt_folders_keep)
}


## ------------------------------------------------------------------------------------------------------
# String matrix empty
mt_st_paths_names <- matrix(data=NA, nrow=length(ls_sfls), ncol=4)

# Loop and print found files
it_folders_names_to_keep = 2
it_file_counter = 0
for (spt_file in ls_sfls) {
    # row counter
    it_file_counter = it_file_counter + 1

    # get file paths
    ls_srt_folders_name_keep <- tail(strsplit(spt_file, "/")[[1]], n=it_folders_names_to_keep+1)
    snm_file_name <- tail(ls_srt_folders_name_keep, 1)
    ls_srt_folders_keep <- head(ls_srt_folders_name_keep, it_folders_names_to_keep)

    # store
    # tools::file_path_sans_ext to drop suffix
    mt_st_paths_names[it_file_counter,1] = tools::file_path_sans_ext(snm_file_name)
    mt_st_paths_names[it_file_counter,2] = ls_srt_folders_keep[1]
    mt_st_paths_names[it_file_counter,3] = ls_srt_folders_keep[2]
    mt_st_paths_names[it_file_counter,4] = spt_file
}

# Column Names
ar_st_varnames <- c('fileid','name','folder1','folder2', 'fullpath')

# Combine to tibble, add name col1, col2, etc.
tb_csv_info <- as_tibble(mt_st_paths_names) %>%
  rowid_to_column(var = "id") %>%
  rename_all(~c(ar_st_varnames))

# Display
kable(tb_csv_info[,1:4]) %>% kable_styling_fc()


## ------------------------------------------------------------------------------------------------------
# Generate a list of dataframes
ls_df_loaded_files =
  apply(tb_csv_info,
        1,
        function(row) {
          # Loading file
          spn_full_path <- row[5]
          mt_csv = read.csv(file = spn_full_path)
          # dataframe
          it_fileid <- row[1]
          snm_filename <- row[2]
          srt_folder_level2 <- row[3]
          srt_folder_level1 <- row[4]

          tb_combine = as_tibble(mt_csv) %>%
            na.omit %>%
            rowid_to_column(var = "statesid") %>%
            mutate(fileid = it_fileid,
                   filename = snm_filename,
                   folder_lvl1 = srt_folder_level1,
                   folder_lvl2 = srt_folder_level2) %>%
            select(fileid, filename, folder_lvl1, folder_lvl2,
                   statesid, everything())
          # return
          return(tb_combine)
        })

# Stack dataframes together
df_all_files = do.call(bind_rows, ls_df_loaded_files)

# show stacked table
kable(df_all_files[seq(1,601,50),1:6]) %>% kable_styling_fc_wide()


## ------------------------------------------------------------------------------------------------------
# separate last eleemtnafter underscore
df_all_files_finalA <- df_all_files %>%
   separate(filename, into = c("filename_main", "prod_type_st"),
            sep="_(?=[^_]+$)",
            remove = FALSE) %>%
   select(fileid, filename, filename_main, prod_type_st, folder_lvl1, folder_lvl2,
          statesid, everything())
# show stacked table
kable(df_all_files_finalA[seq(1,601,50),1:10]) %>% kable_styling_fc_wide()


## ------------------------------------------------------------------------------------------------------
# string and number separation
df_all_files_finalB <- df_all_files_finalA %>%
  separate(prod_type_st,
           into = c("prod_type_st_prefix", "prod_type_lvl"),
           sep="(?<=[A-Za-z])(?=[-0-9])", # positive or negative numbers
           remove=FALSE) %>%
  separate(folder_lvl1,
           into = c("cev_prefix", "cev_lvl"),
           sep="(?<=[A-Za-z])(?=[-0-9])", # positive or negative numbers
           remove=FALSE) %>%
  mutate(cev_st = folder_lvl1,
         prod_type_lvl = as.numeric(prod_type_lvl),
         cev_lvl = as.numeric(cev_lvl)/10000) %>%
  select(fileid,
         prod_type_st, prod_type_lvl,
         cev_st, cev_lvl,
         statesid, EjV,
         filename, folder_lvl1, folder_lvl2)
# Ordering, sort by cev_lvl, then prod_type_lvl, then stateid
df_all_files_finalB <- df_all_files_finalB %>%
  arrange(cev_lvl, prod_type_lvl, statesid)
# show stacked table
kable(df_all_files_finalB[seq(1,49*16,49),1:7]) %>% kable_styling_fc_wide()

