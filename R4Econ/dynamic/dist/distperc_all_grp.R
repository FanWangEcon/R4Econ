# By some grouping variable, simulate for all groups, and generate summary statistics percentiles.
# df <- df.slds
# vars.group.by = c('m_pm_i')
# var.prob <- 'prob'
# it.sample.draws <- 20000
# bl.gather <- TRUE
# bl.graph.print <- FALSE
# ff_dyna_simu_slds_all_grps(df, vars.group.by = vars.group.by, var.prob = var.prob,
#                            it.sample.draws = it.sample.draws, bl.gather = TRUE,
#                            bl.graph.print = bl.graph.print)
ff_dyna_simu_slds_all_grps <- function(df.slds,
                                       vars.group.by = c('m_pm_i'),
                                       var.prob = 'prob',
                                       it.sample.draws = 20000,
                                       bl.gather = TRUE,
                                       bl.graph.print = FALSE){

    # vars.group.by = c('m_pm_i')
    # var.prob <- 'prob'
    # it.sample.draws <- 20000
    # bl.graph.print <- FALSE
    # transpose = FALSE
    # fmt_mean = '%.1f'
    # fmt_sd = '%.1f'
    # options(warn=-1)

    start_time <- Sys.time()
    df.slds.stats <- df.slds %>%
                arrange(!!!syms(vars.group.by)) %>%
                        group_by(!!!syms(vars.group.by)) %>%
                        do(data.frame(ff_dyna_simu_slds(.,
                                                        var.prob=var.prob,
                                                        it.sample.draws=it.sample.draws,
                                                        bl.graph.print=bl.graph.print)))

    end_time <- Sys.time()
    print(paste0('Simulation took:', end_time - start_time))

    # if(bl.graph.print) {
    # options(repr.matrix.max.rows=10, repr.matrix.max.cols=20)
    # print(df.slds.stats)
    # }

    if (bl.gather) {
       df.slds.stats <- df.slds.stats %>% gather(variable, value, -var, -one_of(vars.group.by)) %>%  
                              filter(variable != 'n' & variable != 'NAobs') %>% ungroup()
    }

    return(df.slds.stats)
}
