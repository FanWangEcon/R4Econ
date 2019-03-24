# dependencies:
# source('C:/Users/fan/R4Econ/optimization/support/fraction.R')
# source('C:/Users/fan/R4Econ/optimization/planer/ces/cesplanerobj.R')

# Single Parameter Optimization Functions
obj_planer <- function(x, param.ces, f.subsidy.y.str, list.subsidy.y.params.other, boo.out.y = FALSE) {

    # Input list
    # Convert from Estimation x to Actual Fraction between 0 and 1
    list.subsidy.y.params.maximand <- list(vec.subsidy.frac = f_frac_asymp_vec(x))
    list.subsidy.y.params <- append(list.subsidy.y.params.other, list.subsidy.y.params.maximand)

    # Call Function
    list.df.y.subsidized <- do.call(f.subsidy.y.str, list.subsidy.y.params)

    df.y.subsidized <- list.df.y.subsidized$dfmain
    var.y.subsidy <- list.df.y.subsidized$ysubsidy

    # C:\Users\fan\R4Econ\optimization\planer\ces\cesplanerobj.R
    if (boo.out.y) {
        return(list.df.y.subsidized)
    } else {
        obj <- (-1)*f_planer_obj(vec.y=df.y.subsidized[[var.y.subsidy]], param.ces=param.ces)
        return(obj)
    }

}

# invoked obj_planer to generate full prediction dataframe with summary statistics
obj_planer_predict <- function(x, param.ces, f.subsidy.y.str, list.subsidy.y.params.other) {

    # Add Statistics for Outcome Variable With Subsidies
    # Generate Full Data Prediction frame
    list.df.y.subsidized <- obj_planer(x, param.ces, f.subsidy.y.str, list.subsidy.y.params.other, boo.out.y = TRUE)

    # FROM: summarize\summ\ByGroupSummOne.R
    # Summary statistics
    list.group.stats <- ff_summ_by_group_summ_one(list.df.y.subsidized$dfmain,
                                                  vars.group = list.subsidy.y.params.other$var.grp.idx,
                                                  var.numeric = list.df.y.subsidized$ysubsidy,
                                                  str.stats.specify = c('mean', 'sd', 'min'),
                                                  boo.overall.stats = TRUE)

    list.optim.predict <- append(list.df.y.subsidized, list.group.stats)


    # Return
    return(list.optim.predict)
}

# Subsidy Function
# var.grp.idx: name of index group variable
# subsidy.total: total subsidy
# vec.subsidy.frac: fraction of subsidy each group
# vec.subsidy.grpsize: number of people in each subsidy group.
f_subsidy_vec <- function(df, var.grp.idx, subsidy.total, vec.subsidy.frac, vec.subsidy.grpsize,
                          review=FALSE, var.i='i', var.t='t',
                          var.new.subsidy ='subsidy',
                          var.new.subsidy.grp ='subsidy_grp') {

    # var.grp.idx <- 'subsidy.grp'
    # subsidy_total <- 2
    # df.subsidy.frac <- c(0.1, 0.9)
    # vec.subsidy.grpsize <- c(1, 1)
    df.with.subsidy <- df %>% mutate(!!(var.new.subsidy.grp) := paste0(vec.subsidy.frac, collapse=','),
                                     !!(var.new.subsidy) := ((subsidy.total*
                                                 vec.subsidy.frac[df[[var.grp.idx]]])/
                                                vec.subsidy.grpsize[df[[var.grp.idx]]]))

    if (review) {
      df.with.subsidy.review <- df.with.subsidy %>%
                                    select(!!sym(var.i), !!sym(var.t), !!sym(var.grp.idx),
                                           contains('subsidy'), everything()) %>%
                                group_by(!!sym(var.grp.idx)) %>%
                                mutate_at(vars(c('subsidy')), funs(subsidy_mean_it = mean,
                                                                   subsidy_sd_it = sd,
                                                                   subsidy_n_it = n()))  %>%
                 ungroup() %>% group_by(!!sym(var.i)) %>% slice(1L) %>% group_by(!!sym(var.grp.idx)) %>%
                 mutate_at(vars(c('subsidy')), funs(subsidy_mean_i = mean,
                                                    subsidy_sd_i = sd,
                                                    subsidy_n_i = n())) %>%
                summarize_if(is.numeric, mean) %>%
                select(!!sym(var.grp.idx), contains('subsidy'), everything()) %>%
                mutate_if(is.numeric, round, 3)

        print(t(df.with.subsidy.review))

    } else {

        df.with.subsidy.review <- c('')
    }

    return(list(dfmain=df.with.subsidy,
                dfreview=df.with.subsidy.review,
                varsubsidy=var.new.subsidy))
}


# Optimization Wrapper
# sca.subsidy.frac.init.default <- numeric((sca.subsidy.groups-1))+1
# list.subsidy.y.params.other must have: vec.subsidy.grpsize
# Optimization Function
optim_wrapper <- function(sca.subsidy.frac.init, param.ces, f.subsidy.y.str, list.subsidy.y.params.other) {

    # Optimization
    res.opti <- optim(sca.subsidy.frac.init, obj_planer,
                      param.ces = param.ces,
                      f.subsidy.y.str = f.subsidy.y.str,
                      list.subsidy.y.params.other = list.subsidy.y.params.other)

    # Generate Named LIst
    sca.subsidy.lengthm1 = length(sca.subsidy.frac.init)
    list.sca.subsidy.frac.init <- setNames(sca.subsidy.frac.init,
                                           paste0('sca.subsidy.frac.init.v', 1:sca.subsidy.lengthm1))
    list.par <- setNames(res.opti$par, paste0('par.v',
                                              1:sca.subsidy.lengthm1))
    list.par.frac <- setNames(f_frac_asymp_vec(res.opti$par),
                              paste0('par.frac.v', 1:(sca.subsidy.lengthm1+1)))

    # Number of Individuals Each Subsidy Group, Normalize Relative Subidy Across Groups
    list.vec.subsidy.grpsize <- setNames(list.subsidy.y.params.other$vec.subsidy.grpsize,
                                         paste0('par.frac.grpsize.v', 1:(sca.subsidy.lengthm1+1)))
    list.vec.frac.sub.lvl <- ((f_frac_asymp_vec(res.opti$par))/(list.subsidy.y.params.other$vec.subsidy.grpsize))
    list.vec.frac.norm <- list.vec.frac.sub.lvl/sum(list.vec.frac.sub.lvl)
    list.vec.subsidy.indi.lvl <- setNames(list.vec.frac.sub.lvl*list.subsidy.y.params.other$subsidy.total,
                                         paste0('par.frac.lvl.v', 1:(sca.subsidy.lengthm1+1)))
    list.vec.subsidy.grpsize.norm <- setNames(list.vec.frac.norm,
                                         paste0('par.frac.norm.v', 1:(sca.subsidy.lengthm1+1)))

    # Collect Results
    list.esti.res <- list(param.ces = param.ces,
                          subsidy.total = list.subsidy.y.params.other[['subsidy.total']],
                          par.frac.sum = sum(f_frac_asymp_vec(res.opti$par)),
                          value = res.opti$value,
                          counts.function = (res.opti$counts)[['function']],
                          counts.gradient = (res.opti$counts)[['gradient']],
                          convergence = res.opti$convergence)

    list.esti.res <- append(list.esti.res, list.sca.subsidy.frac.init)
    list.esti.res <- append(list.esti.res, list.par)
    list.esti.res <- append(list.esti.res, list.par.frac)
    list.esti.res <- append(list.esti.res, list.vec.subsidy.grpsize)
    list.esti.res <- append(list.esti.res, list.vec.subsidy.indi.lvl)
    list.esti.res <- append(list.esti.res, list.vec.subsidy.grpsize.norm)

    # Add Statistics for Outcome Variable With Subsidies
    # FROM: summarize\summ\ByGroupSummOne.R
    list.optim.predict <- obj_planer_predict(res.opti$par, param.ces, f.subsidy.y.str, list.subsidy.y.params.other)

    list.esti.res <- append(list.esti.res, list.optim.predict$df_row_stats_all)
    
    # Return
    return(list.esti.res)
}


# Graphically
# x-axis different CES parameters
graphf.ces.opti.subsidy <- function(df.ces.opti, sca.subsidy.groups, vec.subsidy.grpsize,
                                    str.title, str.captions, geom_text_format='%.3f') {
    df.ces.opti %>%
        gather(variable, value, -param.ces)  %>%
        ggplot(aes(x=factor(param.ces), y=value,
                   fill=variable,
                   label=sprintf(geom_text_format, value))) +
        geom_bar(stat = 'identity', position='dodge2') +
        geom_text(size=3, hjust=0.5, vjust=0, angle=0,
                  fontface = 'bold', color='black',
                  position = position_dodge(width = 1)) +
        labs(title = paste0(paste0(str.title, '\nOptimal Subsidy ',
                                   sca.subsidy.groups,' Groups, n per group=(',
                                   paste0(vec.subsidy.grpsize, collapse=',')
                              ,')\n0=Cobb-Douglas, 1=Perfect Substitutes, -Inf=Leontiff')),
               x = 'CES Parameters',
               y = 'Subsidies',
               caption = str.captions) +
        theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust = 1))
}

# Generate Graphs in plannerN2cessub:
# subfigure for different CES Parameters
# x-axis different subsidy levels
graphf.ces.totsubsidy.opti.subsidy <- function(df.ces.subsidy.opti,
                                               sca.subsidy.groups, vec.subsidy.grpsize,
                                               str.title, str.captions,
                                               facet_ncol = 2,
                                               geom_text_format='%.3f') {
  # df.opti.ces.subsidy.reed.h0 %>%
  #    select(param.ces, subsidy.total, matches('par.frac.norm.v')) %>%
  #    arrange(param.ces, subsidy.total)  %>%

  df.ces.subsidy.opti %>%
    gather(variable, value, -param.ces, -subsidy.total)  %>%
        ggplot(aes(x=factor(subsidy.total), y=value,
                   fill=variable,
                   label=sprintf(geom_text_format, value))) +
        geom_bar(stat = 'identity', position='dodge2') +
        geom_text(size=3, hjust=0.5, vjust=0, angle=0,
                  fontface = 'bold', color='black',
                  position = position_dodge(width = 1)) +
        facet_wrap( ~ param.ces, ncol = 2) +
        labs(title = paste0(paste0(str.title, '\nOptimal Subsidy ',
                                   sca.subsidy.groups,' Groups, n per group=(',
                                   paste0(vec.subsidy.grpsize, collapse=',')
                              ,')\n0=Cobb-Douglas, 1=Perfect Substitutes, -Inf=Leontiff')),
               x = 'Total Subsidies',
               y = 'Group Subsidies',
               caption = str.captions) +
        theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust = 1))
}
