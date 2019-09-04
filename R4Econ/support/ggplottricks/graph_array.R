# Some array of data, graph it
options(repr.plot.width = 4, repr.plot.height = 4)
tibble(ar.it.simu=ar.it.simu) %>% ggplot(aes(x=ar.it.simu)) + geom_density()
