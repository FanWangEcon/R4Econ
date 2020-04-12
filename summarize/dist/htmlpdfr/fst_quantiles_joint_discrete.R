## ----global_options, include = FALSE--------------------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------------------
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

# Check Are Values within Group By Unique? If not, STOP
f_check_distinct_ingroup <- function(df, vars.group_by, vars.values_in_group) {

    df.uniqus.in.group <- df %>% group_by(!!!syms(vars.group_by)) %>%
            mutate(quant_vars_paste = paste(!!!(syms(vars.values_in_group)), sep='-')) %>%
            mutate(unique_in_group = n_distinct(quant_vars_paste)) %>%
            slice(1L) %>%
            ungroup() %>%
            group_by(unique_in_group) %>%
            summarise(n=n())

    if (sum(df.uniqus.in.group$unique_in_group) > 1) {
        print(df.uniqus.in.group)
        print(paste('vars.values_in_group', vars.values_in_group, sep=':'))
        print(paste('vars.group_by', vars.group_by, sep=':'))
        stop("The variables for which quantiles are to be taken are not identical within the group variables")
    }
}


## -------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------
# Cutting Function, Cut Continuous Variables into Quantiles with labeing
f_cut <- function(var, df.sliced.quantiles, seq.quantiles, include.lowest=TRUE, fan.labels=TRUE, print=FALSE) {

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


## -------------------------------------------------------------------------------------------------
# Combo Quantile Function
# vars.cts2quantile <- c('wealthIdx', 'hgt0', 'wgt0')
# seq.quantiles <- c(0, 0.3333, 0.6666, 1.0)
# vars.group_by <- c('indi.id')
# vars.arrange <- c('indi.id', 'svymthRound')
# vars.continuous <- c('wealthIdx', 'hgt0', 'wgt0')
df_cut_by_sliced_quantiles <- function(df, vars.cts2quantile, seq.quantiles,
                                       vars.group_by, vars.arrange) {


    # Check Are Values within Group By Unique? If not, STOP
    f_check_distinct_ingroup(df, vars.group_by, vars.values_in_group=vars.cts2quantile)

    # First Step Slicing
    df.sliced <- df_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange)

    # Second Step Generate Categorical Variables of Quantiles
    df.with.cut.quant <- df %>% mutate_at(vars.cts2quantile,
                               funs(q=f_cut(., df.sliced$df.sliced.quantiles,
                                            seq.quantiles=seq.quantiles,
                                            include.lowest=TRUE, fan.labels=TRUE)))

    if (length(vars.cts2quantile) > 1) {
        df.with.cut.quant <- df.with.cut.quant %>%
                              rename_at(vars(contains('_q')),
                                        funs(f_var_rename(., seq.quantiles=seq.quantiles)))
    } else {
        new.var.name <- paste0(vars.cts2quantile[1], '_', f_Q_suffix(seq.quantiles))
        df.with.cut.quant <- df.with.cut.quant %>% rename(!!new.var.name := q)
    }

    # Newly Generated Quantile-Cut Variables
    vars.quantile.cut <- df.with.cut.quant %>%
                select(matches(paste0(vars.cts2quantile, collapse='|'))) %>%
                select(matches(f_Q_suffix(seq.quantiles)))

    # Return
    return(list(df.with.cut.quant = df.with.cut.quant,
                df.sliced.quantiles=df.sliced$df.sliced.quantiles,
                df.grp.L1=df.sliced$df.grp.L1,
                vars.quantile.cut=vars.quantile.cut))

}


## -------------------------------------------------------------------------------------------------
# Function to handle list inputs with different quantiles vars and probabilities
df_cut_by_sliced_quantiles_grps <- function(quantile.grp.list, df, vars.group_by, vars.arrange) {
   vars.cts2quantile <- quantile.grp.list$vars
   seq.quantiles <- quantile.grp.list$prob
   return(df_cut_by_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange))
}
# Show Results
df_cut_by_sliced_quantiles_joint_results_grped <- function(df.with.cut.quant.all, vars.cts2quantile, vars.group_by, vars.arrange,
                                                           vars.quantile.cut.all, var.qjnt.grp.idx) {
    # Show ALL
    df.group.panel.cnt.mean <- df.with.cut.quant.all %>% group_by(!!!syms(vars.quantile.cut.all), !!sym(var.qjnt.grp.idx)) %>%
            summarise_at(vars.cts2quantile, funs(mean, n()))

    # Show Based on SLicing first
    df.group.slice1.cnt.mean <- df.with.cut.quant.all %>% group_by(!!!syms(vars.group_by)) %>% arrange(!!!syms(vars.arrange)) %>% slice(1L) %>%
            group_by(!!!syms(vars.quantile.cut.all), !!sym(var.qjnt.grp.idx)) %>%
            summarise_at(vars.cts2quantile, funs(mean, n()))

    return(list(df.group.panel.cnt.mean=df.group.panel.cnt.mean,
                df.group.slice1.cnt.mean=df.group.slice1.cnt.mean))
}


## -------------------------------------------------------------------------------------------------
# # Joint Quantile Group Name
# var.qjnt.grp.idx <- 'group.index'
# # Generate Categorical Variables of Quantiles
# vars.group_by <- c('indi.id')
# vars.arrange <- c('indi.id', 'svymthRound')
# # Quantile Variables and Quantiles
# vars.cts2quantile.wealth <- c('wealthIdx')
# seq.quantiles.wealth <- c(0, .5, 1.0)
# vars.cts2quantile.wgthgt <- c('hgt0', 'wgt0')
# seq.quantiles.wgthgt <- c(0, .3333, 0.6666, 1.0)
# drop.any.quantile.na <- TRUE
# # collect to list
# list.cts2quantile <- list(list(vars=vars.cts2quantile.wealth,
#                                prob=seq.quantiles.wealth),
#                           list(vars=vars.cts2quantile.wgthgt,
#                                prob=seq.quantiles.wgthgt))

df_cut_by_sliced_quantiles_joint <- function(df, var.qjnt.grp.idx,
                                             list.cts2quantile,
                                             vars.group_by, vars.arrange,
                                             drop.any.quantile.na = TRUE,
                                             toprint = TRUE) {

  #  Original dimensions
  if(toprint) {
   print(dim(df))
  }

  # All Continuous Variables from lists
  vars.cts2quantile <- unlist(lapply(list.cts2quantile, function(elist) elist$vars))
  vars.cts2quantile

  # Keep only if not NA for all Quantile variables
  if (drop.any.quantile.na) {
   df.select <- df %>% drop_na(c(vars.group_by, vars.arrange, vars.cts2quantile))
  } else {
   df.select <- df
  }

  if(toprint) {
   print(dim(df.select))
  }

  # Apply qunatile function to all elements of list of list
  df.cut.list <- lapply(list.cts2quantile, df_cut_by_sliced_quantiles_grps,
                        df=df.select, vars.group_by=vars.group_by, vars.arrange=vars.arrange)

  # Reduce Resulting Core Panel Matrix Together
  df.with.cut.quant.all <- lapply(df.cut.list, function(elist) elist$df.with.cut.quant) %>% reduce(left_join)
  df.sliced.quantiles.all <- lapply(df.cut.list, function(elist) elist$df.sliced.quantiles)

  if(toprint) {
    print(dim(df.with.cut.quant.all))
  }

  # Obrain Newly Created Quantile Group Variables
  vars.quantile.cut.all <- unlist(lapply(df.cut.list, function(elist) names(elist$vars.quantile.cut)))
  if(toprint) {
    print(vars.quantile.cut.all)
    print(summary(df.with.cut.quant.all %>% select(one_of(vars.quantile.cut.all))))
  }

  # Generate Joint Quantile Index Variable
  df.with.cut.quant.all <- df.with.cut.quant.all %>% mutate(!!var.qjnt.grp.idx := group_indices(., !!!syms(vars.quantile.cut.all)))

  # Quantile Groups
  arr.group.idx <- t(sort(unique(df.with.cut.quant.all[[var.qjnt.grp.idx]])))

  # Results Display
  df.group.print <- df_cut_by_sliced_quantiles_joint_results_grped(df.with.cut.quant.all, vars.cts2quantile,
                                                 vars.group_by, vars.arrange,
                                                 vars.quantile.cut.all, var.qjnt.grp.idx)

  # list to Return
  # These returns are the same as returns earlier: df_cut_by_sliced_quantiles
  # Except that they are combined together
  return(list(df.with.cut.quant = df.with.cut.quant.all,
              df.sliced.quantiles = df.sliced.quantiles.all,
              df.grp.L1 = (df.cut.list[[1]])$df.grp.L1,
              vars.quantile.cut = vars.quantile.cut.all,
              df.group.panel.cnt.mean = df.group.print$df.group.panel.cnt.mean,
              df.group.slice1.cnt.mean = df.group.print$df.group.slice1.cnt.mean))

}


## -------------------------------------------------------------------------------------------------
# Library
library(tidyverse)

# Load Sample Data
setwd('C:/Users/fan/R4Econ/_data/')
df <- read_csv('height_weight.csv')


## -------------------------------------------------------------------------------------------------
# Joint Quantile Group Name
var.qjnt.grp.idx <- 'group.index'
list.cts2quantile <- list(list(vars=c('hgt0'), prob=c(0, .3333, 0.6666, 1.0)))
results <- df_cut_by_sliced_quantiles_joint(df, var.qjnt.grp.idx, list.cts2quantile,
                                            vars.group_by = c('indi.id'), vars.arrange = c('indi.id', 'svymthRound'),
                                            drop.any.quantile.na = TRUE, toprint = FALSE)
# Show Results
results$df.group.slice1.cnt.mean


## -------------------------------------------------------------------------------------------------
# Joint Quantile Group Name
var.qjnt.grp.idx <- 'wltQuintle.index'
list.cts2quantile <- list(list(vars=c('wealthIdx'), prob=seq(0, 1.0, 0.20)))
results <- df_cut_by_sliced_quantiles_joint((df %>% filter(S.country == 'Guatemala')),
                                            var.qjnt.grp.idx, list.cts2quantile,
                                            vars.group_by = c('indi.id'), vars.arrange = c('indi.id', 'svymthRound'),
                                            drop.any.quantile.na = TRUE, toprint = FALSE)
# Show Results
results$df.group.slice1.cnt.mean


## -------------------------------------------------------------------------------------------------
# Joint Quantile Group Name
var.qjnt.grp.idx <- 'group.index'
list.cts2quantile <- list(list(vars=c('hgt0', 'wgt0'), prob=c(0, .5, 1.0)))
results <- df_cut_by_sliced_quantiles_joint(df, var.qjnt.grp.idx, list.cts2quantile,
                                            vars.group_by = c('indi.id'), vars.arrange = c('indi.id', 'svymthRound'),
                                            drop.any.quantile.na = TRUE, toprint = FALSE)
# Show Results
results$df.group.slice1.cnt.mean


## -------------------------------------------------------------------------------------------------
# Joint Quantile Group Name
var.qjnt.grp.idx <- 'group.index'
list.cts2quantile <- list(list(vars=c('wealthIdx'), prob=c(0, .5, 1.0)), list(vars=c('hgt0'), prob=c(0, .333, 0.666, 1.0)))
results <- df_cut_by_sliced_quantiles_joint((df %>% filter(S.country == 'Cebu')),
                                             var.qjnt.grp.idx, list.cts2quantile,
                                             vars.group_by = c('indi.id'), vars.arrange = c('indi.id', 'svymthRound'),
                                             drop.any.quantile.na = TRUE, toprint = FALSE)
# Show Results
results$df.group.slice1.cnt.mean


## -------------------------------------------------------------------------------------------------
# Joint Quantile Group Name
var.qjnt.grp.idx <- 'wltHgt0Wgt0.index'
list.cts2quantile <- list(list(vars=c('wealthIdx'), prob=c(0, .5, 1.0)), list(vars=c('hgt0', 'wgt0'), prob=c(0, .5, 1.0)))
results <- df_cut_by_sliced_quantiles_joint((df %>% filter(S.country == 'Cebu')),
                                            var.qjnt.grp.idx, list.cts2quantile,
                                            vars.group_by = c('indi.id'), vars.arrange = c('indi.id', 'svymthRound'),
                                            drop.any.quantile.na = TRUE, toprint = FALSE)
# Show Results
results$df.group.slice1.cnt.mean


## -------------------------------------------------------------------------------------------------
# Selected Variables, many Percentiles
vars.group_by <- c('indi.id')
vars.arrange <- c('indi.id', 'svymthRound')
vars.cts2quantile <- c('wealthIdx', 'hgt0', 'wgt0')
seq.quantiles <- c(0, 0.3333, 0.6666, 1.0)
df.sliced <- df_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange)
df.sliced.quantiles <- df.sliced$df.sliced.quantiles
df.grp.L1 <- df.sliced$df.grp.L1


## -------------------------------------------------------------------------------------------------
df.sliced.quantiles


## -------------------------------------------------------------------------------------------------
# Quantiles all Variables
suppressMessages(lapply(names(df), gen_quantiles, df=df.grp.L1, prob=seq(0.1,0.9,0.10)) %>% reduce(full_join))


## -------------------------------------------------------------------------------------------------
# Function Testing
arr.quantiles <- df.sliced.quantiles[[substitute('wealthIdx')]]
arr.quantiles
arr.sort.unique.quantiles <- sort(unique(df.sliced.quantiles[[substitute('wealthIdx')]]))
arr.sort.unique.quantiles
f_Q_label(arr.quantiles, arr.sort.unique.quantiles[1], seq.quantiles)
f_Q_label(arr.quantiles, arr.sort.unique.quantiles[2], seq.quantiles)
lapply(arr.sort.unique.quantiles[1:(length(arr.sort.unique.quantiles)-1)],
       f_Q_label,
       arr.quantiles=arr.quantiles,
       seq.quantiles=seq.quantiles)


## -------------------------------------------------------------------------------------------------
# Generate Categorical Variables of Quantiles
vars.group_by <- c('indi.id')
vars.arrange <- c('indi.id', 'svymthRound')
vars.cts2quantile <- c('wealthIdx', 'hgt0', 'wgt0')
seq.quantiles <- c(0, 0.3333, 0.6666, 1.0)
df.cut <- df_cut_by_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange)
vars.quantile.cut <- df.cut$vars.quantile.cut
df.with.cut.quant <- df.cut$df.with.cut.quant
df.grp.L1 <- df.cut$df.grp.L1


## -------------------------------------------------------------------------------------------------
# Cut Variables Generated
names(vars.quantile.cut)
summary(vars.quantile.cut)


## -------------------------------------------------------------------------------------------------
# options(repr.matrix.max.rows=50, repr.matrix.max.cols=20)
# df.with.cut.quant


## -------------------------------------------------------------------------------------------------
# Group By Results
f.count <- function(df, var.cts, seq.quantiles) {
    df %>% select(S.country, indi.id, svymthRound, matches(paste0(var.cts, collapse='|'))) %>%
        group_by(!!sym(f_var_rename(paste0(var.cts,'_q'), seq.quantiles))) %>%
        summarise_all(funs(n=n()))
}


## -------------------------------------------------------------------------------------------------
# Full Panel Results
lapply(vars.cts2quantile, f.count, df=df.with.cut.quant, seq.quantiles=seq.quantiles)


## -------------------------------------------------------------------------------------------------
# Results Individual Slice
lapply(vars.cts2quantile, f.count,
       df=(df.with.cut.quant %>% group_by(!!!syms(vars.group_by)) %>% arrange(!!!syms(vars.arrange)) %>% slice(1L)),
       seq.quantiles = seq.quantiles)


## -------------------------------------------------------------------------------------------------
# Generate Categorical Variables of Quantiles
vars.group_by <- c('indi.id')
vars.arrange <- c('indi.id', 'svymthRound')


## -------------------------------------------------------------------------------------------------
# Quantile Variables and Quantiles
vars.cts2quantile.wealth <- c('wealthIdx')
seq.quantiles.wealth <- c(0, .5, 1.0)
vars.cts2quantile.wgthgt <- c('hgt0', 'wgt0')
seq.quantiles.wgthgt <- c(0, .3333, 0.6666, 1.0)
drop.any.quantile.na <- TRUE
# collect to list
list.cts2quantile <- list(list(vars=vars.cts2quantile.wealth,
                               prob=seq.quantiles.wealth),
                          list(vars=vars.cts2quantile.wgthgt,
                               prob=seq.quantiles.wgthgt))


## -------------------------------------------------------------------------------------------------
vars.cts2quantile <- unlist(lapply(list.cts2quantile, function(elist) elist$vars))
f_check_distinct_ingroup(df, vars.group_by, vars.values_in_group=vars.cts2quantile)


## -------------------------------------------------------------------------------------------------
# Original dimensions
dim(df)
# All Continuous Variables from lists
vars.cts2quantile <- unlist(lapply(list.cts2quantile, function(elist) elist$vars))
vars.cts2quantile
# Keep only if not NA for all Quantile variables
if (drop.any.quantile.na) {
    df.select <- df %>% drop_na(c(vars.group_by, vars.arrange, vars.cts2quantile))
}
dim(df.select)


## -------------------------------------------------------------------------------------------------
# Dealing with a list of quantile variables
df.cut.wealth <- df_cut_by_sliced_quantiles(df.select, vars.cts2quantile.wealth, seq.quantiles.wealth, vars.group_by, vars.arrange)
summary(df.cut.wealth$vars.quantile.cut)
# summary((df.cut.wealth$df.with.cut.quant)[['wealthIdx_Qs0e1n2']])
# df.cut.wealth$df.with.cut.quant %>% filter(is.na(wealthIdx_Qs0e1n2))
# df.cut.wealth$df.with.cut.quant %>% filter(indi.id == 500)


## -------------------------------------------------------------------------------------------------
df.cut.wgthgt <- df_cut_by_sliced_quantiles(df.select, vars.cts2quantile.wgthgt, seq.quantiles.wgthgt, vars.group_by, vars.arrange)
summary(df.cut.wgthgt$vars.quantile.cut)


## -------------------------------------------------------------------------------------------------
# Function to handle list inputs with different quantiles vars and probabilities
df_cut_by_sliced_quantiles_grps <- function(quantile.grp.list, df, vars.group_by, vars.arrange) {
    vars.cts2quantile <- quantile.grp.list$vars
    seq.quantiles <- quantile.grp.list$prob
    return(df_cut_by_sliced_quantiles(df, vars.cts2quantile, seq.quantiles, vars.group_by, vars.arrange))
}


## -------------------------------------------------------------------------------------------------
# Apply function
df.cut.list <- lapply(list.cts2quantile, df_cut_by_sliced_quantiles_grps,
                      df=df.select, vars.group_by=vars.group_by, vars.arrange=vars.arrange)


## -------------------------------------------------------------------------------------------------
# Reduce Resulting Matrixes Together
df.with.cut.quant.all <- lapply(df.cut.list, function(elist) elist$df.with.cut.quant) %>% reduce(left_join)
dim(df.with.cut.quant.all)


## -------------------------------------------------------------------------------------------------
# Obrain Newly Created Quantile Group Variables
vars.quantile.cut.all <- unlist(lapply(df.cut.list, function(elist) names(elist$vars.quantile.cut)))
vars.quantile.cut.all


## -------------------------------------------------------------------------------------------------
summary(df.with.cut.quant.all %>% select(one_of(vars.quantile.cut.all)))


## -------------------------------------------------------------------------------------------------
# df.with.cut.quant.all %>%
#     group_by(!!!syms(vars.quantile.cut.all)) %>%
#     summarise_at(vars.cts2quantile, funs(mean, n()))


## -------------------------------------------------------------------------------------------------
# Generate Joint Quantile Index Variable
var.qjnt.grp.idx <- 'group.index'
df.with.cut.quant.all <- df.with.cut.quant.all %>% mutate(!!var.qjnt.grp.idx := group_indices(., !!!syms(vars.quantile.cut.all)))


## -------------------------------------------------------------------------------------------------
arr.group.idx <- t(sort(unique(df.with.cut.quant.all[[var.qjnt.grp.idx]])))
arr.group.idx


## -------------------------------------------------------------------------------------------------
df.with.cut.quant.all %>% group_by(!!!syms(vars.quantile.cut.all), !!sym(var.qjnt.grp.idx)) %>%
        summarise_at(vars.cts2quantile, funs(mean, n()))


## -------------------------------------------------------------------------------------------------
df.with.cut.quant.all  %>% group_by(!!!syms(vars.group_by)) %>% arrange(!!!syms(vars.arrange)) %>% slice(1L) %>%
        group_by(!!!syms(vars.quantile.cut.all), !!sym(var.qjnt.grp.idx)) %>%
        summarise_at(vars.cts2quantile, funs(mean, n()))


## -------------------------------------------------------------------------------------------------
# arr.group.idx.subsidy <- arr.group.idx*2 - ((arr.group.idx)^2)*0.01
arr.group.idx.subsidy <- arr.group.idx*2
df.with.cut.quant.all %>%
        mutate(more_prot = prot + arr.group.idx.subsidy[!!sym(var.qjnt.grp.idx)]) %>%
        group_by(!!!syms(vars.quantile.cut.all), !!sym(var.qjnt.grp.idx))  %>%
        summarise_at(c('more_prot', 'prot'), funs(mean(., na.rm=TRUE)))

