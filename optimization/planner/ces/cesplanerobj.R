parmam.ces.min <- -160
# Planer's objective function
# Constant Elasticity of Substitution over All Individuals
f_planer_obj  <- function(vec.y = c(80, 85, 90), param.ces = 0.5) {

    # Can not Process NA Values. Count NA values in vec.y and give alert if there are NAs
    sca.NA.count <- sum(is.na(vec.y))
    if (sca.NA.count > 0 ) {
        warning(sprintf(paste0("In f_planer_obj, vec.y (len=%s) has NA (len=%s) values\n",
                               "Ignored NAs for Planer Objective"),
                                length(vec.y), sca.NA.count))
        # Remove NA
        vec.y <- na.omit(vec.y)
    }

    # CES Objective Function
    if (param.ces == 0) {
        obj <- prod(vec.y^(1/length(vec.y)))
    } else if (param.ces <= parmam.ces.min) {
        obj <- min(vec.y)
    } else {
        obj <- (mean(vec.y^param.ces))^(1/param.ces)
    }

    # Return
    return(obj)
}

# Vector of CES Parameters
f_ces_params_vec <- function(sca.len.right = 11, sca.len.left = 9, ces.max = 0.99, ces.min = parmam.ces.min) {
    vec.param.ces.right <- round(seq(-1, ces.max, length.out = sca.len.right), 2)
    vec.param.ces.left <- (-1)*unique(round(exp(log(10)*seq(log10(1),
                                                       log10((-1)*ces.min),
                                                       length.out=sca.len.left))))
    vec.param.ces <- sort(unique(c(vec.param.ces.left, vec.param.ces.right)))
    return(vec.param.ces)
}

# Planer Test Function
f_planer_obj_tib_cesvec <- function(vec.y, vec.ces.params) {
    if(missing(vec.ces.params)) {
        vec.ces.params <- f_ces_params_vec(3, 3)
    }
    df.planer.obj.cesparams <- tibble(ces.param=vec.ces.params,
                                      planer.value=unlist(lapply(vec.ces.params,
                                                                 f_planer_obj, vec.y = vec.y))) %>%
                                    mutate(vec.y = paste0(vec.y, collapse=','))


    return(df.planer.obj.cesparams)
}

# Graphing Function
graphf_planer_obj_tib_cesvec <- function(df.vec.out.all.planer) {
df.vec.out.all.planer %>%
    ggplot(aes(x=factor(ces.param),
               y=planer.value,
               fill=vec.y)) +
    geom_bar(stat = 'identity', position = "dodge2") +
    labs(title = paste0(paste0('CES Parameters, Various Y Vectors')
                          ,'\n0=Cobb-Douglas, 1=Perfect Substitutes, -Inf=Leontiff'),
           x = 'CES Parameters',
           y = 'Planer Object/Welfare',
           caption = paste0('Equal Weights')) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust = 1))
}
