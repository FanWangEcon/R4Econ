f.by.group.unique.obs <- function(df,
                                  vars.group = c('S.country', 'vil.id'),
                                  var.unique.identifier = 'indi.id',
                                  st.unique.suffix = '_unique',
                                  graph=TRUE) {
#' Unique Observations
#' @examples
#' https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountUniqueIndi.html
#' ls.df <- f.by.group.unique.obs(df.counting %>% ungroup() %>%
#'                       select(one_of(vars.group.by, vars.arrange.by, var.countobs, var.countobs.nonNA.n)),
#'                       vars.group = vars.count.groups, var.unique.identifier = var.unique.identifier, graph = FALSE)
#' df.countobs.nonNA.n <- ls.df$df

    vars.all <- names(df)
    var.unique.indi <- paste0(var.unique.identifier, st.unique.suffix)

    df.group.unique <- df %>%
          mutate_at(vars.group, funs(as.factor(.))) %>%
          group_by(!!!syms(vars.group)) %>%
          arrange(!!!syms(vars.group)) %>%
          mutate_if(is.numeric, funs(n=sum(is.na(.)==0))) %>%
          mutate(!!var.unique.indi := n_distinct(!!sym(var.unique.identifier))) %>%
          slice(1L) %>%
          select(!!!syms(vars.group), var.unique.indi, everything(), -!!var.unique.identifier,
                -one_of(vars.all))

    if (graph){
        graph <- graphf.by.group.unique.obs(df.group.unique, var.unique.indi, vars.group)
        return(list(df=df.group.unique, graph=graph))
    } else {
        return(list(df=df.group.unique))
    }
}

graphf.by.group.unique.obs <- function(df.by.group,
                                       var.unique.indi,
                                       vars.group = c('S.country', 'vil.id')) {
#' Graphing Function
#' @examples
#' https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountUniqueIndi.html

    color.var <- vars.group[1]
    x.var <- vars.group[2]

    # x-var as factor
    df.by.group[[x.var]] <- factor(df.by.group[[x.var]])

    # Graph Size
    options(repr.plot.width = 8, repr.plot.height = 4)

    # Titling
    graph.title <- sprintf('Number of Unique %s By (%s and %s) Groups',
                           var.unique.identifier, color.var, x.var)
    graph.caption <- sprintf(
        paste0('Jitter/Random Hgt/Wgt Guat/Cebu Data\n',
               'see:https://fanwangecon.github.io/HeightProfile/'))
    graph.title.x <- sprintf('%s Groups', x.var)
    graph.title.y <- sprintf('Number of Unique %s', var.unique.identifier)


    # Graphing (unique_indi used earlier as name)
    graph <- df.by.group %>%
        select(one_of(vars.group, var.unique.indi)) %>%
        gather(variable, value, -one_of(vars.group)) %>%
        ggplot(aes(x=!!sym(x.var), y=value))  +
        geom_bar(stat = 'identity', position="dodge")  +
        facet_wrap(as.formula(paste0('~', color.var)), ncol=2, scales = "free")  +
        labs(title = graph.title,
             x = graph.title.x, y = graph.title.y,
             caption = graph.caption) +
        theme(axis.text.x = element_text(angle = 90))

    return(graph)
}
