# Single Variable Group Statistics
ff_summ_by_group_summ_one <- function(df, vars.group, var.numeric, str.stats.group = 'main', str.stats.specify = NULL){

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

    # Group Sort
    df.select <- df %>%
                    drop_na() %>%
                    group_by(!!!syms(vars.group)) %>%
                    arrange(!!!syms(c(vars.group, var.numeric)))

    # Table of Statistics
    df.table.grp.stats <- df.select %>% summarize_at(vars(var.numeric), funs(!!!strs.all))

    # Add Stat Name
    if (length(strs.all) == 1) {
        # give it a name, otherwise if only one stat, name of stat not saved
        df.table.grp.stats <- df.table.grp.stats %>% rename(!!strs.all := !!sym(var.numeric))
    }


    # Row of Statistics
    str.vars.group.combine <- paste0(vars.group, collapse='_')
    if (length(vars.group) == 1) {
        df.row.grp.stats <- df.table.grp.stats %>%
                mutate(!!(str.vars.group.combine) := paste0(var.numeric, '.',
                                               vars.group, '.g',
                                               (!!!syms(vars.group)))) %>%
                gather(variable, value, -one_of(vars.group)) %>%
                unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
                spread(str.vars.group.combine, value)
    } else {
        df.row.grp.stats <- df.table.grp.stats %>%
                                mutate(vars.groups.combine := paste0(paste0(vars.group, collapse='.')),
                                       !!(str.vars.group.combine) := paste0(interaction(!!!(syms(vars.group))))) %>%
                                mutate(!!(str.vars.group.combine) := paste0(var.numeric, '.', vars.groups.combine, '.',
                                                                           (!!sym(str.vars.group.combine)))) %>%
                                ungroup() %>%
                                select(-vars.groups.combine, -one_of(vars.group)) %>%
                gather(variable, value, -one_of(str.vars.group.combine))  %>%
                unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
                spread(str.vars.group.combine, value)

    }

    # Clean up name strings
    names(df.table.grp.stats) <- gsub(x = names(df.table.grp.stats),pattern = "_", replacement = "\\.")
    names(df.row.grp.stats) <- gsub(x = names(df.row.grp.stats),pattern = "_", replacement = "\\.")

    # Return
    return(list(df_table_grp_stats = df.table.grp.stats,
                df_row_grp_stats = df.row.grp.stats))
}
