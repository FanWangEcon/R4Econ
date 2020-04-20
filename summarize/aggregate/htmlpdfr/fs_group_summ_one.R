## ----global_options, include = FALSE-----------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------
# Single Variable Group Statistics (also generate overall statistics)
ff_summ_by_group_summ_one <- function(
  df, vars.group, var.numeric, str.stats.group = 'main',
  str.stats.specify = NULL, boo.overall.stats = TRUE){
  
  # List of statistics
  # https://rdrr.io/cran/dplyr/man/summarise.html
  strs.center <- c('mean', 'median')
  strs.spread <- c('sd', 'IQR', 'mad')
  strs.range <- c('min', 'max')
  strs.pos <- c('first', 'last')
  strs.count <- c('n_distinct')
  
  # Grouping of Statistics
  if (missing(str.stats.specify)) {
    if (str.stats.group == 'main') {
      strs.all <- c('mean', 'min', 'max', 'sd')
    }
    if (str.stats.group == 'all') {
      strs.all <- c(strs.center, strs.spread, strs.range, strs.pos, strs.count)
    }
  } else {
    strs.all <- str.stats.specify
  }
  
  # Start Transform
  df <- df %>% drop_na() %>% 
    mutate(!!(var.numeric) := as.numeric(!!sym(var.numeric)))
  
  # Overall Statistics
  if (boo.overall.stats) {
    df.overall.stats <- df %>% 
      summarize_at(vars(var.numeric), funs(!!!strs.all))
    if (length(strs.all) == 1) {
      # give it a name, otherwise if only one stat, name of stat not saved
      df.overall.stats <- df.overall.stats %>% 
        rename(!!strs.all := !!sym(var.numeric))
    }
    names(df.overall.stats) <- 
      paste0(var.numeric, '.', names(df.overall.stats))
  }
  
  # Group Sort
  df.select <- df %>%
    group_by(!!!syms(vars.group)) %>%
    arrange(!!!syms(c(vars.group, var.numeric)))
  
  # Table of Statistics
  df.table.grp.stats <- df.select %>% 
    summarize_at(vars(var.numeric), funs(!!!strs.all))
  
  # Add Stat Name
  if (length(strs.all) == 1) {
    # give it a name, otherwise if only one stat, name of stat not saved
    df.table.grp.stats <- df.table.grp.stats %>% 
      rename(!!strs.all := !!sym(var.numeric))
  }
  
  
  # Row of Statistics
  str.vars.group.combine <- paste0(vars.group, collapse='_')
  if (length(vars.group) == 1) {
    df.row.grp.stats <- df.table.grp.stats %>%
      mutate(!!(str.vars.group.combine) := 
               paste0(var.numeric, '.',
                      vars.group, '.g',
                      (!!!syms(vars.group)))) %>%
      gather(variable, value, -one_of(vars.group)) %>%
      unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
      spread(str.vars.group.combine, value)
  } else {
    df.row.grp.stats <- df.table.grp.stats %>% 
      mutate(vars.groups.combine := 
               paste0(paste0(vars.group, collapse='.')),
             !!(str.vars.group.combine) := 
               paste0(interaction(!!!(syms(vars.group))))) %>%
      mutate(!!(str.vars.group.combine) := 
               paste0(var.numeric, '.', vars.groups.combine, '.',
                      (!!sym(str.vars.group.combine)))) %>%
      ungroup() %>%
      select(-vars.groups.combine, -one_of(vars.group)) %>%
      gather(variable, value, -one_of(str.vars.group.combine)) %>%
      unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
      spread(str.vars.group.combine, value)
  }
  
  # Clean up name strings
  names(df.table.grp.stats) <- 
    gsub(x = names(df.table.grp.stats),pattern = "_", replacement = "\\.")
  names(df.row.grp.stats) <- 
    gsub(x = names(df.row.grp.stats),pattern = "_", replacement = "\\.")
  
  # Return
  list.return <- 
    list(df_table_grp_stats = df.table.grp.stats, 
         df_row_grp_stats = df.row.grp.stats)
  
  # Overall Statistics, without grouping
  if (boo.overall.stats) {
    df.row.stats.all <- c(df.row.grp.stats, df.overall.stats)
    list.return <- append(list.return, 
                          list(df_overall_stats = df.overall.stats,
                               df_row_stats_all = df.row.stats.all))
  }
  
  # Return
  return(list.return)
  
}


## ----------------------------------------------------------------------------------------------------
# Library
library(tidyverse)

# Load Sample Data
setwd('C:/Users/fan/R4Econ/_data/')
df <- read_csv('height_weight.csv')


## ----------------------------------------------------------------------------------------------------
vars.group <- 'sex'
var.numeric <- 'hgt'


## ----------------------------------------------------------------------------------------------------
df.select <- df %>% select(one_of(vars.group, var.numeric)) %>% drop_na()


## ----------------------------------------------------------------------------------------------------
# Single Variable Group Statistics
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.group = 'main')$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.specify = c('mean', 'sd'))$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.specify = c('mean'))$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------
vars.group <- c('S.country', 'sex')
var.numeric <- 'hgt'


## ----------------------------------------------------------------------------------------------------
df.select <- df %>% select(one_of(vars.group, var.numeric)) %>% drop_na()


## ----------------------------------------------------------------------------------------------------
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.group = 'main')$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.specify = c('mean', 'sd'))$df_table_grp_stats


## ----------------------------------------------------------------------------------------------------
ff_summ_by_group_summ_one(
  df.select, vars.group = vars.group, var.numeric = var.numeric, 
  str.stats.specify = c('mean'))$df_table_grp_stats

