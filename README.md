
This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) for Panel Data Statistics/Econometrics Analyasis, produced by [Fan](https://fanwangecon.github.io/). Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R codes are used. The goal of this repository is to make it easier to re-use codes produced for various projects.

R is used. Packages from [Tidyverse](https://www.tidyverse.org/) are used. Materials are written in R using Jupyter notebook and shown as HTML files. To obtain codes and raw files, see [here](docs/gitsetup.md) for github set up. For HTML files, click on the links below.

Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

# 1. Summary Statistics

## 1.1 Counting

1. [By Groups, Count non-NA observations of All Variables](summarize/count/ByGroupCountAllVarNonNA.html)
  + **core**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))*
2. [By Groups, Count Unique Individuals and non-NA observations of other Variables](summarize/count/ByGroupCountUniqueIndi.html)
  + **core**: *group_by + mutate_if + mutate + n_distinct + slice(1L)*
