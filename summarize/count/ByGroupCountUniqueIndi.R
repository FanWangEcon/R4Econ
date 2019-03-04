# Data Function
# https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountUniqueIndi.html
f.by.group.unique.obs <- function(df,
                                  vars.group = c('S.country', 'vil.id'),
                                  var.unique.identifier = 'indi.id',
                                  graph=TRUE) {
    
    vars.all <- names(df)
    df.group.unique <- df %>% group_by(!!!syms(vars.group)) %>%
          arrange(!!!syms(vars.group)) %>%
          mutate_if(is.numeric, funs(n=sum(is.na(.)==0))) %>%
          mutate(unique_indi = n_distinct(!!sym(var.unique.identifier))) %>%
          slice(1L) %>%
          select(!!!syms(vars.group), unique_indi, everything(), -!!var.unique.identifier,
                -one_of(vars.all))

    if (graph){
        graph <- graphf.by.group.unique.obs(df.group.unique, vars.group)
        return(list(df=df.group.unique, graph=graph))
    } else {
        return(list(df=df.group.unique))
    }
}

# Graphing Function
# https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountUniqueIndi.html
graphf.by.group.unique.obs <- function(df.by.group,
                                       vars.group = c('S.country', 'vil.id')) {
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
        select(one_of(vars.group), unique_indi) %>%
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
