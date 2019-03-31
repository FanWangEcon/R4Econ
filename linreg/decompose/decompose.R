# # Define Left Hand Side Variables
# var.y1 <- c('hgt')
# var.y2 <- c('wgt')
# vars.y <- c(var.y1, var.y2)
# # Define Right Hand Side Variables
# vars.x <- c('prot')
# vars.c <- c('male', 'wgt0', 'hgt0', 'svymthRound')
# # vars.z <- c('p.A.prot')
# vars.z <- c('vil.id')
# # vars.z <- NULL
# vars.xc <- c(vars.x, vars.c)
#
# # Other variables to keep
# vars.other.keep <- c('S.country', 'vil.id', 'indi.id', 'svymthRound')
#
# # Decompose sequence
# vars.tomean.first <- c('male', 'hgt0')
# var.tomean.first.name.suffix <- '_mh02m'
# vars.tomean.second <- c(vars.tomean.first, 'hgt0', 'wgt0')
# var.tomean.second.name.suffix <- '_mh0me2m'
# vars.tomean.third <- c(vars.tomean.second, 'prot')
# var.tomean.third.name.suffix <- '_mh0mep2m'
# vars.tomean.fourth <- c(vars.tomean.third, 'svymthRound')
# var.tomean.fourth.name.suffix <- '_mh0mepm2m'
# list.vars.tomean = list(
# #                         vars.tomean.first,
#                         vars.tomean.second,
#                         vars.tomean.third,
#                         vars.tomean.fourth
#                         )
# list.vars.tomean.name.suffix <- list(
# #                                     var.tomean.first.name.suffix,
#                                      var.tomean.second.name.suffix,
#                                      var.tomean.third.name.suffix,
#                                      var.tomean.fourth.name.suffix
#                                     )

ff_lr_decompose <- function(df, vars.y, vars.x, vars.c, vars.z, vars.other.keep,
                            list.vars.tomean, list.vars.tomean.name.suffix,
                            graph=FALSE, graph.nrow=2) {

    vars.xc <- c(vars.x, vars.c)

    # Regressions
    # regf.iv from C:\Users\fan\R4Econ\linreg\ivreg\ivregdfrow.R
    df.reg.out <- as_tibble(bind_rows(lapply(vars.y, regf.iv,
                                             vars.x=vars.x, vars.c=vars.c, vars.z=vars.z, df=df)))

    # Select Variables
    str.esti.suffix <- '_Estimate'
    arr.esti.name <- paste0(vars.xc, str.esti.suffix)
    str.outcome.name <- 'vars_var.y'
    arr.columns2select <- c(arr.esti.name, str.outcome.name)
    # arr.columns2select

    # Generate dataframe for coefficients
    df.coef <- df.reg.out[,c(arr.columns2select)] %>% mutate_at(vars(arr.esti.name), as.numeric) %>% column_to_rownames(str.outcome.name)
    # df.coef
    # str(df.coef)

    # Decomposition Step 1: gather
    df.decompose <- df %>%
                            filter(svymthRound %in% c(12, 18, 24)) %>%
                            select(one_of(c(vars.other.keep, vars.xc, vars.y))) %>%
                            drop_na() %>%
                            gather(variable, value, -one_of(c(vars.other.keep, vars.xc)))

    # Decomposition Step 2: mutate_at(vars, funs(mean = mean(.)))
    # the xc averaging could have taken place earlier, no difference in mean across variables
    df.decompose <- df.decompose %>%
                          group_by(variable) %>%
                          mutate_at(vars(c(vars.xc, 'value')), funs(mean = mean(.))) %>%
                          ungroup()

    # Decomposition Step 3 With Loop
    for (i in 1:length(list.vars.tomean)) {
        var.decomp.cur <- (paste0('value', list.vars.tomean.name.suffix[[i]]))
        vars.tomean <- list.vars.tomean[[i]]
        var.decomp.cur
        df.decompose <- df.decompose %>% mutate((!!var.decomp.cur) := ff_lr_decompose_valadj(., df.coef, vars.tomean, str.esti.suffix))
    }

    # Additional Statistics
    df.decompose.var.frac <- df.decompose %>%
            select(variable, contains('value')) %>%
            group_by(variable) %>%
            summarize_all(funs(mean = mean, var = var)) %>%
            select(matches('value')) %>% select(ends_with("_var")) %>%
            mutate_if(is.numeric, funs( frac = (./value_var))) %>%
            mutate_if(is.numeric, round, 3)

    # Graph
    g.graph.dist <- NULL
    if (graph) {
      g.graph.dist <- df.decompose %>%
          select(variable, contains('value'), -value_mean) %>%
          rename(outcome = variable) %>%
          gather(variable, value, -outcome) %>%
          ggplot(aes(x=value, color = variable, fill = variable)) +
              geom_line(stat = "density") +
              facet_wrap(~ outcome, scales='free', nrow=graph.nrow)
    }

    # Return
    return(list(dfmain = df.decompose,
                dfsumm = df.decompose.var.frac,
                graph = g.graph.dist))

}

# Support Function
ff_lr_decompose_valadj <- function(df, df.coef, vars.tomean, str.esti.suffix) {
    new_value <- (df$value +
                  rowSums((df[paste0(vars.tomean, '_mean')] - df[vars.tomean])
                          *df.coef[df$variable, paste0(vars.tomean, str.esti.suffix)]))
    return(new_value)
}
