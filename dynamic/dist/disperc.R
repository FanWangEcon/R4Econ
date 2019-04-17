ff_dyna_simu_slds <- function(df.slds.groupby.seg,
                              var.prob = 'prob',
                              it.sample.draws = 20000,
                              bl.graph.print = FALSE){

    ar.it.simu <- sample(dim(df.slds.groupby.seg)[1], it.sample.draws, replace = TRUE,
                         prob = (df.slds.groupby.seg[[var.prob]]/sum(df.slds.groupby.seg[[var.prob]])));

    df.sampled <- df.slds.groupby.seg[ar.it.simu, ]

    # z = hist(df.slds.groupby.seg$, plot=FALSE)
    if (bl.graph.print) {
        print(dim(df.slds.groupby.seg))
        print(dim(df.sampled))
        # options(repr.plot.width = 4, repr.plot.height = 2)
        # tibble(ar.it.simu=ar.it.simu) %>% ggplot(aes(x=ar.it.simu)) + geom_density()
    }

    df.sampled.percentiles <- f.summ.percentiles(df.sampled)

    return(df.sampled.percentiles)
}
