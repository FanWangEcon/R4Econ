ff_dyna_sup_expand_grids <- function(ar.st.vars, list.ar.fl, list.ts.valpolmat) {

#     val <- array(c(1, 1.5, 0, 2, 0, 4, 0, 3), dim=c(2, 2, 2))
#     pol <- array(runif(8), dim=c(2, 2, 2))
#     list.ts.valpolmat <- list(val=val, pol=pol)

#     ar.fl.x <- c(1.1,2.3)
#     ar.fl.y <- c(2.1,3.3)
#     ar.fl.z <- c(3.1,4.3)
#     ar.st.vars <- c('vx', 'vy', 'vz')
#     list.ar.fl <- list(ar.fl.x, ar.fl.y, ar.fl.z)

    mt.fl.expanded <- do.call(expand.grid, list.ar.fl)
    colnames(mt.fl.expanded) <- ar.st.vars
    df.out <- as_tibble(bind_cols(mt.fl.expanded, list.ts.valpolmat))

    return(df.out)
}
