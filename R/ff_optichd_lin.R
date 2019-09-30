ff_panel_longandwide <- function(df, svr_id_t, svr_id_i, svr_data) {
    #' Append Group Mean Lagged Values as Additional Panel Variables
    #'
    #' @description
    #' There is a variable recorded for a panel, and a variable with date info, and another with obs info
    #' Compute averages over sub-groups of dates for each variable, with different ways of specifying date subgroups. average(svr_data) over svr
    #' Reshape data so each date is a variable for selected subset of key variables
    #' Merge results from 2 back to main, so that each indi/date observation has as variables all lagged and forward information as additional variables. Append not n lag m forward, but full history as additional variables
    #' Doing this allows for lagged intereaction that are time specific in an arbitrary way.
    #'
    #' @param svr_id_t string time variable name
    #' @param svr_id_i string individual ID name
    #' @param svr_data string variable name
    #' @return a long panel frame with wide expansion of group mean lagged vars
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/R4Econ/reference/ff_panel_longandwide.html}
    #' \url{https://fanwangecon.github.io/R4Econ/panel/expand/fst_panel_lag_expand.html}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' library(dplyr)
    #' df_hw_cebu <- df_hgt_wgt %>% filter(S.country == 'Cebu' & svymthRound <= 24 & svymthRound > 0)
    #' df_hw_cebu <- df_hw_cebu %>% mutate(mth6 = recode(svymthRound,
    #'                                `0`="m00t06", `2`="m00t06", `4`="m00t06", `6`="m00t06",
    #'                                `8`="m08t12", `10`="m08t12", `12`="m08t12",
    #'                                `14`="m14t18", `16`="m14t18", `18`="m14t18",
    #'                                `20`="m20t24", `22`="m20t24", `24`="m20t24"))
    #' df_longandwide <- ff_panel_longandwide(df_hw_cebu,
    #'                                          svr_id_t = 'mth6',
    #'                                          svr_id_i = 'indi.id',
    #'                                          svr_data = 'cal')
    #' print(df_longandwide %>% select(indi.id, svymthRound, mth6, cal, matches('mth6'), everything()), n=100)

    # Select vars to keep for spreading
    ls_svr_keep <- c(svr_id_i, svr_id_t, svr_data)
    df_widespread <- df %>% select(!!!syms(ls_svr_keep))

    # Aggregate
    svr_data_mean <- paste(svr_data,svr_id_t,'mean',sep='_')
    df_widespread <- df_widespread %>%
      group_by(!!sym(svr_id_i), !!sym(svr_id_t)) %>%
      summarise(!!sym(svr_data_mean) := mean(!!sym(svr_data)))

    # Spread
    df_wide <- df_widespread %>% spread(!!sym(svr_id_t), !!sym(svr_data_mean), sep = paste0('_', svr_data, '_'))

    # Merge Back, now dataframe is both wide and long
    df_widelong <- df %>% left_join(df_wide)

    # return
    return(df_widelong)

}
