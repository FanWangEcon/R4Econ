ff_reg_mbyn <- function(list.vars.y, list.vars.x,
                        vars.c, vars.z, df,
                        return_all = FALSE,
                        stats_ends = 'value') {

    # regf.iv() function is from C:\Users\fan\R4Econ\linreg\ivreg\ivregdfrow.R

    if (return_all) {
        df.reg.out.all <- bind_rows(lapply(list.vars.x,
                              function(x) (
                                  bind_rows(lapply(list.vars.y, regf.iv, vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df))
                              )))

    } else {
        df.reg.out.all <- (lapply(list.vars.x,
                              function(x) (
                                  bind_rows(lapply(list.vars.y, regf.iv, vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df)) %>%
                                      select(vars_var.y, starts_with(x)) %>%
                                      select(vars_var.y, ends_with(stats_ends))
                              ))) %>% reduce(full_join)
    }

    return(df.reg.out.all)
}                          
