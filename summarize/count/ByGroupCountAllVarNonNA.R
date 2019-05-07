# Data Function
# https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountAllVarNonNA.html
f.by.group.vars.obs <- function (df, vars.group = c('S.country', 'svymthRound'), graph=TRUE) {

    df.group <- df %>% group_by(!!!syms(vars.group)) %>%
            arrange(!!!syms(vars.group)) %>%
            summarise_if(is.numeric, funs(sum(is.na(.)==0)))

    if (graph){
        graph <- graphf.by.group.vars.obs(df.group, vars.group)
        return(list(df=df.group, graph=graph))
    } else {
        return(list(df=df.group))
    }
}

# Graphing Function
# https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountAllVarNonNA.html
graphf.by.group.vars.obs <- function(df.by.group, vars.group = c('S.country', 'svymthRound')) {
    color.var <- vars.group[1]
    x.var <- vars.group[2]

    # x-var as factor
    df.by.group[[x.var]] <- factor(df.by.group[[x.var]])

    # Graph Size
    options(repr.plot.width = 9, repr.plot.height = 10)

    # Titling
    graph.title <- sprintf('Number of Observations Each Variable By (%s and %s) Groups',
                           color.var, x.var)
    graph.caption <- sprintf(
        paste0('Jitter/Random Hgt/Wgt Guat/Cebu Data\n',
               'see:https://fanwangecon.github.io/HeightProfile/'))
    graph.title.x <- 'Month Groups'
    graph.title.y <- 'Number of Observations'

    # Graphing
    graph <- df.by.group %>%
        gather(variable, value, -one_of(vars.group)) %>%
        ggplot(aes(x=!!sym(x.var), y=value, fill=!!sym(color.var))) +
        geom_bar(stat = 'identity', position="dodge") +
        facet_wrap( ~ variable, ncol=2) +
        labs(title = graph.title,
             x = graph.title.x, y = graph.title.y,
             caption = graph.caption) +
        theme(axis.text.x = element_text(angle = 90))

    return(graph)
}
