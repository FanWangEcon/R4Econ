## ----global_options, include = FALSE---------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------
# folder
spt_root <- c('C:/Users/fan/R4Econ/panel/join/_file/csv')
# cev surface file, the V file
snm_cev_surface <- 'e_19E1NEp99r99_ITG_PE_cev_subsettest.csv'
mt_cev_surface <- read.csv(file = file.path(spt_root, snm_cev_surface))
tb_cev_surface <- as_tibble(mt_cev_surface) %>%
  rename(EjVcev = EjV)


## --------------------------------------------------------------------------------------------------------------------
ls_tb_cev_surfhat = vector(mode = "list", length = 4)
for (it_simu_counter in c(1,2,3,4)) {

    # conditionally change file names
    if (it_simu_counter == 1) {
        st_counter <- '19E1NEp99r99'
    } else if (it_simu_counter == 2) {
        st_counter <- '19E1NEp02r99'
    } else if (it_simu_counter == 3) {
        st_counter <- '19E1NEp02per02ger99'
    } else if (it_simu_counter == 4) {
        st_counter <- '19E1NEp02r02'
    }
    snm_v_hat <- paste0('e_', st_counter, '_ITG_PE_subsettest.csv')
    
    # Overall path to files
    mt_v_hat <- read.csv(file = file.path(spt_root, snm_v_hat))
    tb_v_hat <- as_tibble(mt_v_hat) %>%
      select(prod_type_lvl, statesid, EjV)
    
    # Merge file using key 
    tb_cev_surfhat <- tb_cev_surface %>%
      left_join(tb_v_hat, by=(c('prod_type_lvl'='prod_type_lvl', 
                                'statesid'='statesid'))) %>%
      arrange(statesid, prod_type_lvl, cev_lvl) %>%
      mutate(counter_policy = st_counter)
    
    # Store to list
    ls_tb_cev_surfhat[[it_simu_counter]] <- tb_cev_surfhat
}

# Display
kable(ls_tb_cev_surfhat[[1]][seq(1, 40, 5),]) %>% kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------
ls_tb_cev_matched = vector(mode = "list", length = 4)
for (it_simu_counter in c(1,2,3,4)) {

    # Load merged file
    tb_cev_surfhat <- ls_tb_cev_surfhat[[it_simu_counter]]

    # Difference Column
    tb_cev_surfhat <- tb_cev_surfhat %>% 
      mutate(EjVcev_gap = abs(EjVcev - EjV))
    
    # Group by, Arrange and Slice, get lowest gap
    tb_cev_matched <- tb_cev_surfhat %>% 
      arrange(statesid, prod_type_lvl, EjVcev_gap) %>%
      group_by(statesid, prod_type_lvl) %>%
      slice_head(n=1)

    # Store to list
    ls_tb_cev_matched[[it_simu_counter]] <- tb_cev_matched
}

# Display
kable(ls_tb_cev_matched[[2]][seq(1, 30, 1),]) %>% kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------
# Single dataframe with all results
tb_cev_matched_all_counter <- do.call(bind_rows, ls_tb_cev_matched)
# check size
print(dim(tb_cev_matched_all_counter))


## --------------------------------------------------------------------------------------------------------------------
# select four from the productivity types
ar_prod_type_lvl_unique <- unique(tb_cev_matched_all_counter %>% pull(prod_type_lvl))
ar_prod_type_lvl_selected <- ar_prod_type_lvl_unique[round(seq(1, length(ar_prod_type_lvl_unique), length.out=4))]
# graph
lineplot <- tb_cev_matched_all_counter %>%
    filter(prod_type_lvl %in% ar_prod_type_lvl_selected) %>%
    group_by(prod_type_st, cash_tt) %>%
    ggplot(aes(x=cash_tt, y=cev_lvl,
               colour=counter_policy, linetype=counter_policy, shape=counter_policy)) +
        facet_wrap( ~ prod_type_st) +
        geom_line() +
        geom_point() +
        labs(title = 'Visualizing the positions of matched values',
             x = 'Resource Levels',
             y = 'CEV',
             caption = paste0('https://fanwangecon.github.io/',
                              'R4Econ/panel/join/htmlpdfr/fs_join_compare.html')) 
print(lineplot)

