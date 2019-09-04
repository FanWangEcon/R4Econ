# Generate Log Grid Points
it.lower.bd.inc.cnt <- 3
fl.log.lower <- -10
fl.log.higher <- -9
fl.min.rescale <- 0.01
it.log.count <- 4

ar.fl.log.rescaled <- exp(log(10)*seq(log10(fl.min.rescale), log10(fl.min.rescale + (fl.log.higher-fl.log.lower)), length.out=it.log.count))
ar.fl.log <- ar.fl.log.rescaled + fl.log.lower - fl.min.rescale
ar.fl.log
