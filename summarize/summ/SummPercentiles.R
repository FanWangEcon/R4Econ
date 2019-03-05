# Summary stats FuunctionPercentile based summaries
# https://fanwangecon.github.io/R4Econ/summarize/summ/SummPercentiles.html
f.summ.percentiles <- function(df, col2varname = FALSE) {

    names(df) <- gsub("_", ".", names(df))

    summ.stats <- df %>%
      ungroup() %>%
      summarise_if(is.numeric,
                   funs(n = n(),
                        NAobs = sum(is.na(.)==1),
                        mean = mean(., na.rm = TRUE),
                        min = min(., na.rm = TRUE),
                        max = max(., na.rm = TRUE),
                        sd = sd(., na.rm = TRUE),
                        p1 = quantile(., probs = c(0.01), na.rm = TRUE),
                        p5 = quantile(., probs = c(0.05), na.rm = TRUE),
                        p25 = quantile(., probs = c(0.25), na.rm = TRUE),
                        p50 = quantile(., probs = c(0.50), na.rm = TRUE),
                        p75 = quantile(., probs = c(0.75), na.rm = TRUE),
                        p95 = quantile(., probs = c(0.95), na.rm = TRUE),
                        p99 = quantile(., probs = c(0.99), na.rm = TRUE))) %>%
      gather(variable, value) %>%
      separate(variable, c('var', 'stats'), sep = "_") %>%
      spread(stats, value) %>%
      select(var, n, NAobs, mean, sd, min, p1, p5, p25, p50, p75, p95, p99, max)

    if (col2varname) {
      summ.stats <- summ.stats %>% column_to_rownames(var="var")
    }

    return(summ.stats)

}
