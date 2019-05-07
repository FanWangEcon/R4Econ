f.summ.percentiles <- function(df, col2varname = FALSE) {
#' Summary stats FuunctionPercentile based summaries
#' @description
#' https://fanwangecon.github.io/R4Econ/summarize/summ/SummPercentiles.html
#' @examples
#' f.summ.percentiles(df.main.guat) %>% mutate_if(is.numeric, round, 2)

    names(df) <- gsub("_", ".", names(df))

    summ.stats <- df %>%
      ungroup() %>%
      summarise_if(is.numeric,
                   funs(n = n(),
                        NAobs = sum(is.na(.)==1),
                        ZEROobs = sum(.==0),
                        mean = mean(., na.rm = TRUE),
                        min = min(., na.rm = TRUE),
                        max = max(., na.rm = TRUE),
                        sd = sd(., na.rm = TRUE),
                        cv = sd(., na.rm = TRUE)/mean(., na.rm = TRUE),
                        p01 = quantile(., probs = c(0.01), na.rm = TRUE),
                        p05 = quantile(., probs = c(0.05), na.rm = TRUE),
                        p10 = quantile(., probs = c(0.10), na.rm = TRUE),
                        p25 = quantile(., probs = c(0.25), na.rm = TRUE),
                        p50 = quantile(., probs = c(0.50), na.rm = TRUE),
                        p75 = quantile(., probs = c(0.75), na.rm = TRUE),
                        p90 = quantile(., probs = c(0.90), na.rm = TRUE),
                        p95 = quantile(., probs = c(0.95), na.rm = TRUE),
                        p99 = quantile(., probs = c(0.99), na.rm = TRUE))) %>%
      gather(variable, value) %>%
      separate(variable, c('var', 'stats'), sep = "_") %>%
      spread(stats, value) %>%
      select(var, n, NAobs, ZEROobs, mean, sd, cv, min, p01, p05, p10, p25, p50, p75, p90, p95, p99, max)

    if (col2varname) {
      summ.stats <- summ.stats %>% column_to_rownames(var="var")
    }

    return(summ.stats)

}


ff.by.groups.summ.percentiles <- function(df, vars.group.by, it.round=4, col2varname = FALSE) {
#' Percentiles mean and sd for as columns, variables as rows, groups vars super-rows
#'
#' @description This calls:
#'   C:\Users\fan\R4Econ\summarize\summ\SummPercentiles.R
#'   uses do.frame to generate by group all percentile results
#'
#' @references C:\Users\fan\R4Econ\summarize\summ\SummPercentiles.R
#' @examples
#' options(warn=-1)
#' vars.group.by <- c(var.country, var.mth)
#' vars.generated <- c(var.dhgt, var.input, var.input.cumu, var.log.h.div.lagh, var.input.log, var.input.log.cumu)
#' df.summ <- ff.by.groups.summ.percentiles(df.reg %>% select(one_of(vars.group.by,vars.generated)),
#'                                          vars.group.by, it.round=4, col2varname = FALSE)
#' options(repr.matrix.max.rows=Inf, repr.matrix.max.cols=Inf)
#' lapply(vars.generated, function(var.gen) df.summ %>% filter(var == var.gen))

    df.summ <- df %>% arrange(!!!syms(vars.group.by)) %>%
                  group_by(!!!syms(vars.group.by)) %>%
                  do(data.frame(f.summ.percentiles(., col2varname=col2varname) %>%
                         mutate_if(is.numeric, round, it.round))) %>%
                  filter(!grepl(paste0(vars.group.by, collapse='|'), var))

    return(df.summ)
}
