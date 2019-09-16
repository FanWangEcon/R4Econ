ff_summ_count_unique_by_groups <- function(df,
                                  ar_svr_group = c('S.country', 'vil.id'),
                                  svr_unique_identifier = 'indi.id') {
    #' By Multiple Groups, Count the Number of Unique Observations (Individuals) within Group
    #'
    #' @description
    #' We have multiple groups (country, village), we want to know the number of unique observations within these groups.
    #' In addition, we also want to generate the total number of observations for each variable within these
    #' country/village groups, these total observations includes multiple values for each unique individual.
    #'
    #' @param df dataframe input dataframe of interest
    #' @param ar_svr_group array string array of variables to group by
    #' @param svr_unique_identifier string variable that has the unique key of interest
    #' @return a dataframe with stats outputs.
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references \url{https://fanwangecon.github.io/R4Econ/reference/ff_summ_count_unique_by_groups.html}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' df_uniques_count_by_vil <- ff_summ_count_unique_by_groups(df_hgt_wgt,
    #'                          ar_svr_group=c('S.country', 'vil.id'),
    #'                          svr_unique_identifier = 'indi.id')
    #' print(df_uniques_count_by_vil, n=50)
    #' df_uniques_count_by_mth <- ff_summ_count_unique_by_groups(df_hgt_wgt,
    #'                          ar_svr_group=c('S.country', 'svymthRound'),
    #'                          svr_unique_identifier = 'indi.id')
    #' print(df_uniques_count_by_mth, n=50)
    #' df_uniques_count_by_country <- ff_summ_count_unique_by_groups(df_hgt_wgt,
    #'                          ar_svr_group=c('S.country'),
    #'                          svr_unique_identifier = 'indi.id')
    #' print(df_uniques_count_by_country)

    ar_svr_vars_all <- names(df)
    tb_group_unique <- df %>% group_by(!!!syms(ar_svr_group)) %>%
          arrange(!!!syms(ar_svr_group)) %>%
          mutate_if(is.numeric, funs(n=sum(is.na(.)==0))) %>%
          mutate(unique_indi = n_distinct(!!sym(svr_unique_identifier))) %>%
          slice(1L) %>%
          select(!!!syms(ar_svr_group), unique_indi, everything(), -!!svr_unique_identifier,
                -one_of(ar_svr_vars_all))

    return(tb_group_unique)
}
