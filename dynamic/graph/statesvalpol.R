# Graphing Function, Multiple a-axis variables
ff_dyna_sup_grid_out_graph <- function(df, x.var,
                                       fill.var,
                                       out.var,
                                       subplot.var,
                                       it.fill.var.show.cnt = 3,
                                       it.subplot.var.show.cnt = 3,
                                       round = 3,
                                       ncol = 3, print=TRUE){

    fill.var.unique <- unique(df[[fill.var]])
    seq.fill.var <- unique(round(seq(1, length(fill.var.unique), length.out = it.fill.var.show.cnt)))

    if (!missing(subplot.var)){
        subplot.var.unique <- unique(df[[subplot.var]])
        seq.subplot.var <- unique(round(seq(1, length(subplot.var.unique), length.out = it.subplot.var.show.cnt)))

        df <- df %>%
            select(one_of(c(out.var, x.var, subplot.var, fill.var))) %>%
            arrange(!!sym(subplot.var), !!sym(fill.var), !!sym(x.var), !!sym(out.var)) %>%
            filter((!!sym(subplot.var)) %in% subplot.var.unique[seq.subplot.var])

    } else {
        df <- df %>%
            select(one_of(c(out.var, x.var, fill.var))) %>%
            arrange(!!sym(fill.var), !!sym(x.var), !!sym(out.var))
    }

    df <- df %>%
            filter((!!sym(fill.var)) %in% fill.var.unique[seq.fill.var]) %>%
            mutate(!!(fill.var) := as.factor(!!sym(fill.var)))


    plot <- df %>% ggplot(aes(x=!!sym(x.var),
                              y=!!sym(out.var),
                              color=!!sym(fill.var),
                              shape=!!sym(fill.var)))

    if (!missing(subplot.var)) {
        plot <- plot + facet_wrap(as.formula(paste0('~ ', subplot.var)), ncol = ncol)
    }

    plot <- plot + geom_line(size = 1) +
                    geom_point() +
                    labs(title = paste0('outcome=',out.var,', x=', x.var, ', color=', fill.var)) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))

    if (print) {
      print(plot)
    }

    return(list(plot=plot))
}
