# Graphing Function, Multiple a-axis variables
ff_dyna_sup_grid_out_graph <- function(df, x.var,
                                       fill.var,
                                       out.var,
                                       subplot.var,
                                       it.fill.var.show.cnt = 3,
                                       it.subplot.var.show.cnt = 3,
                                       round = 3,
                                       ncol = 3,
                                       geom_type = 'line',
                                       st.title = '',
                                       st.labs.x = '',
                                       st.labs.y = '',
                                       st.caption = '',
                                       bl.lines.axis = FALSE,
                                       bl.lines.45 = FALSE,
                                       print=TRUE){
  # df <- df.aprime.max_c.fibs
  # x.var <- 'aprime'
  # fill.var <- 'fl_r_inf'
  # subplot.var <- 'fl_for_br_block'
  # out.var <- 'fl_coh_add'
  # ff_dyna_sup_grid_out_graph(df, x.var, fill.var, out.var, subplot.var,
  #                           it.fill.var.show.cnt = 3,
  #                           it.subplot.var.show.cnt = 3,
  #                           round = 3, ncol = 3, print=TRUE)

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

    if (geom_type == 'line') {
        plot <- plot  + geom_line(size = 1) + geom_point()
    }

    if (bl.lines.axis) {
      plot <- plot + geom_hline(yintercept = 0, size=1, color='black', linetype=2, alpha=0.5) +
                     geom_vline(xintercept = 0, size=1, color='black', linetype=2, alpha=0.5)
    }
    if (bl.lines.45) {
      plot <- plot + geom_abline(intercept = 0, slope = 1, size=1, color='black', linetype=1, alpha=0.5)
    }

   plot <- plot  + labs(title = paste0(st.title, 'outcome=',out.var,', x=', x.var, ', color=', fill.var),
                        x = st.labs.x,
                        y = st.labs.y,
                        caption = st.caption)

    plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    if (print) {
      print(plot)
    }

    return(list(plot=plot))
}
