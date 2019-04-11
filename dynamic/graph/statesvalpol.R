# Graphing Function, Multiple a-axis variables
ff_dyna_sup_grid_out_graph_2states <- function(df, x.var,
                                               fill.var,
                                               out.var,
                                               it.fill.var.show.cnt = 3,
                                               round = 3,
                                               ncol = 3, print=TRUE){

    fill.var.unique <- unique(df[[fill.var]])
    seq.fill.var <- unique(round(seq(1, length(fill.var.unique), length.out = it.fill.var.show.cnt)))

    plot <- df %>%
        select(one_of(c(out.var, x.var, fill.var))) %>%
        arrange(!!sym(fill.var), !!sym(x.var), !!sym(out.var)) %>%
        filter((!!sym(fill.var)) %in% fill.var.unique[seq.fill.var])  %>%
        mutate(!!(fill.var) := as.factor(!!sym(fill.var))) %>%
        ggplot(aes(x=!!sym(x.var),
                   y=!!sym(out.var),
                   color=!!sym(fill.var),
                   shape=!!sym(fill.var))) +
        geom_line(size = 1) +
        geom_point() +
        labs(title = paste0('outcome=',out.var,', x=', x.var, ', color=', fill.var)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    if (print) {
      print(plot)
    }

    return(list(plot=plot))
}
