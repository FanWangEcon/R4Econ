# This calls:
#   C:\Users\fan\R4Econ\summarize\summ\SummPercentiles.R
# uses do.frame to generate by group all percentile results

# options(warn=-1)
# vars.group.by <- c(var.country, var.mth)
# vars.generated <- c(var.dhgt, var.input.cumu, var.log.h.div.lagh, var.input.log, var.input.log.cumu)
# df.summ <- ff.by.groups.summ.percentiles(df.reg %>% select(one_of(vars.group.by,vars.generated)),
#                                          vars.group.by, it.round=4, col2varname = FALSE)
# options(repr.matrix.max.rows=Inf, repr.matrix.max.cols=Inf)
# for ( var.gen in vars.generated) {
#    print(var.gen)
#    df.summ %>% filter(var == var.input.cumu)
# }

ff.by.groups.summ.percentiles <- function(df, vars.group.by, it.round=4, col2varname = FALSE) {

    df.summ <- df %>% arrange(!!!syms(vars.group.by)) %>%
                  group_by(!!!syms(vars.group.by)) %>%
                  do(data.frame(f.summ.percentiles(., col2varname=col2varname) %>%
                         mutate_if(is.numeric, round, it.round))) %>%
                  filter(!grepl(paste0(vars.group.by, collapse='|'), var))

    return(df.summ)
}
