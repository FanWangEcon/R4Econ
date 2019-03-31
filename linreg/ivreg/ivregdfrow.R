# IV regression function
# The code below uses the AER library's regresison function
# All results are stored in a single row as data_frame
# The idea is to combine results from various IVs together row by row  or col by col.
# This functoin could work with dplyr do
# var.y is single outcome, vars.x, vars.c and vars.z are vectors of endogenous variables, controls and instruments.
regf.iv <- function(var.y, vars.x, vars.c, vars.z, df, transpose=TRUE) {
#     library(AER)
    # A. Set-Up Equation
    str.vars.x <- paste(vars.x, collapse='+')
    str.vars.c <- paste(vars.c, collapse='+')
    str.vars.z <- paste(vars.z, collapse='+')
    equa.iv <- paste(var.y,
                     paste(paste(str.vars.x, str.vars.c, sep='+'),
                           paste(str.vars.z, str.vars.c, sep='+'),
                           sep='|'),
                     sep='~')
#     print(equa.iv)

    # B. IV Regression
    ivreg.summ <- summary(ivreg(as.formula(equa.iv), data=df),
                          vcov = sandwich, df = Inf, diagnostics = TRUE)

    # C. Statistics from IV Regression
#     ivreg.summ$coef
#     ivreg.summ$diagnostics

    # D. Combine Regression Results into a Matrix
    df.results <- suppressMessages(as_tibble(ivreg.summ$coef, rownames='rownames') %>%
        full_join(as_tibble(ivreg.summ$diagnostics, rownames='rownames')) %>%
        full_join(tibble(rownames=c('vars'),
                         var.y=var.y,
                         vars.x=str.vars.x,
                         vars.z=str.vars.z,
                         vars.c=str.vars.c)))

    # E. Flatten Matrix, All IV results as a single tibble row to be combined with other IV results
    df.row.results <- df.results %>%
        gather(variable, value, -rownames) %>%
        drop_na() %>%
        unite(esti.val, rownames, variable) %>%
        mutate(esti.val = gsub(' ', '', esti.val))

    if (transpose) {
      df.row.results <- df.row.results %>% spread(esti.val, value)
    }

    # F. Return
    return(data.frame(df.row.results))
}
