# By One Group Mean Sd and Count
# https://fanwangecon.github.io/R4Econ/summarize/summ/ByGroupSumm.html
# f.by.group.vars.mnsdobs(df.main.guat, vars.group.by=c('S.country', 'svymthRound'))
f.by.group.vars.mnsdobs <- function(df,
                                    vars.group.by = c('S.country', 'svymthRound'),
                                    transpose = FALSE) {

  # names(raw.selected.df)
  # -matches(paste0(other.vars.list, collapse='|'))
  # # mean and sd by age
  # # f.by.group.vars.mnsdobs(raw.selected.df)

  # The Code below works when there are no under_scores in variables names
  names(df) <- gsub(x = names(df), pattern = "_", replacement = "\\.")

  df.meansdn <- df %>% group_by(!!!syms(vars.group.by)) %>%
        arrange(!!!syms(vars.group.by)) %>%
        summarise_if(is.numeric,
                 funs(mean = mean, sd = sd, count = sum(is.na(.)==0)),
                 na.rm = TRUE) %>%
        gather(variable, value, -one_of(vars.group.by)) %>%
        separate(variable, c('var', 'stats'), sep = "_") %>%
        spread(stats, value) %>%
        mutate(mean.sd.n = paste0(sprintf("%.1f", round(mean,1)),
                                ' (', sprintf("%.1f", round(sd,1)), ', ', count, ')')) %>%
        select(one_of(vars.group.by), var, mean.sd.n)  %>%
        spread(var, mean.sd.n)

  if (transpose) {

    df.meansdn <- df.meansdn %>%
        unite('group.var.unite', vars.group.by)  %>%
        column_to_rownames(var="group.var.unite")

    df.meansdn <- as_tibble(cbind(nms = names(df.meansdn), t(df.meansdn))) %>%
                    column_to_rownames(var = "nms")
  }

  return(df.meansdn)
}
