# dependencies:
# source('C:/Users/fan/R4Econ/optimization/support/fraction.R')
# source('C:/Users/fan/R4Econ/optimization/planer/ces/cesplanerobj.R')

# Single Parameter Optimization Functions
obj_planer <- function(x, param.ces, f.subsidy.y.str, list.subsidy.y.params.other) {

  # 1. x is the unconstrained transformed fraction from (optimization/support/fraction.R)
  # 2. param.ces is the ces parameter, not that this parameter does not enter the f.subsidy.y.str function
  # 3. f.subsidy.y.str is the name of the estimation prediction function (Step 3) in string
  # 4. list.subsidy.y.params.other contains a list of parameters needed for f.subsidy.y.str in addition to the

    # Input list
    # Convert from Estimation x to Actual Fraction between 0 and 1
    list.subsidy.y.params.maximand <- list(vec.subsidy.frac = f_frac_asymp_vec(x))
    list.subsidy.y.params <- append(list.subsidy.y.params.other, list.subsidy.y.params.maximand)

    # Call Function
    df.y.subsidized <- do.call(f.subsidy.y.str, list.subsidy.y.params)

    # C:\Users\fan\R4Econ\optimization\planer\ces\cesplanerobj.R
    obj <- (-1)*f_planer_obj(vec.y=df.y.subsidized$y_subsidy, param.ces=param.ces)

    return(obj)
}


# Subsidy Function
# var.grp.idx: name of index group variable
# subsidy.total: total subsidy
# vec.subsidy.frac: fraction of subsidy each group
# vec.subsidy.grpsize: number of people in each subsidy group.
f_subsidy_vec <- function(df, var.grp.idx, subsidy.total, vec.subsidy.frac, vec.subsidy.grpsize) {
    # var.grp.idx <- 'subsidy.grp'
    # subsidy_total <- 2
    # df.subsidy.frac <- c(0.1, 0.9)
    # vec.subsidy.grpsize <- c(1, 1)
    return(df %>% mutate(subsidy_grp = paste0(vec.subsidy.frac, collapse=','),
                         subsidy = ((subsidy.total*
                                     vec.subsidy.frac[df[[var.grp.idx]]])/
                                     vec.subsidy.grpsize[df[[var.grp.idx]]])))
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
    list.vec.frac.norm <- ((f_frac_asymp_vec(res.opti$par))/(list.subsidy.y.params.other$vec.subsidy.grpsize))
    list.vec.frac.norm <- list.vec.frac.norm/min(list.vec.frac.norm)
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
    list.esti.res <- append(list.esti.res, list.vec.subsidy.grpsize.norm)

    # Return
    return(list.esti.res)
}


# Graphically
graphf.ces.opti.subsidy <- function(df.ces.opti, sca.subsidy.groups, vec.subsidy.grpsize,
                                    str.title, str.captions) {
    df.ces.opti %>%
        gather(variable, value, -param.ces)  %>%
        ggplot(aes(x=factor(param.ces), y=value,
                   fill=variable,
                   label=sprintf('%.3f', value))) +
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
