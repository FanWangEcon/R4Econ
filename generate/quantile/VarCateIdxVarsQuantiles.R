#################################################
### Support
#################################################

# Quantiles for any variable
gen_quantiles <- function(var, df, prob=c(0.25, 0.50, 0.75)) {
    enframe(quantile(as.numeric(df[[var]]), prob, na.rm=TRUE), 'quant.perc', var)
}
# Support Functions for Variable Suffix
f_Q_suffix <- function(seq.quantiles) {
    quantile.suffix <- paste0('Qs', min(seq.quantiles),
                              'e', max(seq.quantiles),
                              'n', (length(seq.quantiles)-1))
}
# Support Functions for Quantile Labeling
f_Q_label <- function(arr.quantiles,
                      arr.sort.unique.quantile,
                      seq.quantiles) {
    paste0('(',
           paste0(which(arr.quantiles %in% arr.sort.unique.quantile), collapse=','),
           ') of ', f_Q_suffix(seq.quantiles))
}
# Generate New Variable Names with Quantile Suffix
f_var_rename <- function(name, seq.quantiles) {
    quantile.suffix <- paste0('_', f_Q_suffix(seq.quantiles))
    return(sub('_q', quantile.suffix, name))
}

#################################################
### Data Cutting 1: inner cutting function
################################################# 
# First Step, given groups, generate quantiles based on group characteristics
# vars.cts2quantile <- c('wealthIdx', 'hgt0', 'wgt0')
# seq.quantiles <- c(0, 0.3333, 0.6666, 1.0)
# vars.group_by <- c('indi.id')
# vars.arrange <- c('indi.id', 'svymthRound')
# vars.continuous <- c('wealthIdx', 'hgt0', 'wgt0')
df_sliced_quantiles <- function(df, vars.cts2quantile, seq.quantiles,
                                vars.group_by, vars.arrange) {

    # Slicing data
    df.grp.L1 <- df %>% group_by(!!!syms(vars.group_by)) %>% arrange(!!!syms(vars.arrange)) %>% slice(1L) %>% ungroup()

    # Quantiles based on sliced data
    df.sliced.quantiles <- lapply(vars.cts2quantile, gen_quantiles, df=df.grp.L1, prob=seq.quantiles) %>% reduce(full_join)

    return(list(df.sliced.quantiles=df.sliced.quantiles,
                df.grp.L1=df.grp.L1))
}

#################################################
### Data Cutting 2.A
#################################################

# Cutting Function, Cut Continuous Variables into Quantiles with labeing
f_cut <- function(var, df.sliced.quantiles, include.lowest=TRUE, fan.labels=TRUE, print=FALSE) {

    # unparsed string variable name
    var.str <- substitute(var)

    # Breaks
    arr.quantiles <- df.sliced.quantiles[[var.str]]
    arr.sort.unique.quantiles <- sort(unique(arr.quantiles))
    if (print) {
        print(arr.sort.unique.quantiles)
    }

    # Regular cutting With Standard Labels
    # TRUE, means the lowest group has closed bracket left and right
    var.quantile <- cut(var, breaks=arr.sort.unique.quantiles, include.lowest=include.lowest)

    # Use my custom labels
    if (fan.labels) {
        levels.suffix <- lapply(arr.sort.unique.quantiles[1:(length(arr.sort.unique.quantiles)-1)],
                                f_Q_label,
                                arr.quantiles=arr.quantiles,
                                seq.quantiles=seq.quantiles)
        if (print) {
            print(levels.suffix)
        }
        levels(var.quantile) <- paste0(levels(var.quantile), '; ', levels.suffix)
    }

    # Return
    return(var.quantile)
}

#################################################
### Data Cutting 2.B
#################################################

# Combo Quantile Function
# vars.cts2quantile <- c('wealthIdx', 'hgt0', 'wgt0')
# seq.quantiles <- c(0, 0.3333, 0.6666, 1.0)
# vars.group_by <- c('indi.id')
# vars.arrange <- c('indi.id', 'svymthRound')
# vars.continuous <- c('wealthIdx', 'hgt0', 'wgt0')
df_cut_by_sliced_quantiles <- function(df, vars.cts2quantile, seq.quantiles,
                                       vars.group_by, vars.arrange) {

    # First Step Slicing
    df.sliced <- df_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange)

    # Second Step Generate Categorical Variables of Quantiles
    df.with.cut.quant <- df %>% mutate_at(vars.cts2quantile,
                               funs(q=f_cut(., df.sliced$df.sliced.quantiles,
                                           include.lowest=TRUE, fan.labels=TRUE))) %>%
                rename_at(vars(contains('_q')),
                          funs(f_var_rename(., seq.quantiles=seq.quantiles)))
    # Return
    return(list(df.with.cut.quant = df.with.cut.quant,
                df.sliced.quantiles=df.sliced$df.sliced.quantiles,
                df.grp.L1=df.sliced$df.grp.L1))

}
