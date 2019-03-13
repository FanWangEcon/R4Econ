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

# Optimization Wrapper
# sca.subsidy.frac.init.default <- numeric((sca.subsidy.groups-1))+1
# Optimization Function
optim_wrapper <- function(sca.subsidy.frac.init, param.ces, f.subsidy.y.str, list.subsidy.y.params.other) {

  # Basically same parameters as above, except that the first parameter here are the starting values for estimation.
  #
  # 1. sca.subsidy.frac.init: unconstrained fraction subsidy transformed x initial starting estimation values
  # 2. param.ces:
  # 3. f.subsidy.y.str: name of the estimation prediction function (Step 3) in string
  # 4. list.subsidy.y.params.other: contains a list of parameters needed for f.subsidy.y.str.

    # Optimization
    res.opti <- optim(sca.subsidy.frac.init, obj_planer,
                      param.ces = param.ces,
                      f.subsidy.y.str = f.subsidy.y.str,
                      list.subsidy.y.params.other = list.subsidy.y.params.other)

    # Generate Named LIst
    sca.subsidy.lengthm1 = length(sca.subsidy.frac.init)
    list.sca.subsidy.frac.init <- setNames(sca.subsidy.frac.init,
                                           paste0('sca.subsidy.frac.init.v', 1:sca.subsidy.lengthm1))
    list.par <- setNames(res.opti$par, paste0('par.v', 1:sca.subsidy.lengthm1))
    list.par.frac <- setNames(f_frac_asymp_vec(res.opti$par), paste0('par.frac.v', 1:(sca.subsidy.lengthm1+1)))

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
    # Return
    return(list.esti.res)
}
