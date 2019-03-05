
This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) for Panel Data Statistics/Econometrics Analyasis, produced by [Fan](https://fanwangecon.github.io/). Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R codes are used. The goal of this repository is to make it easier to re-use codes produced for various projects.

R is used. Packages from [Tidyverse](https://www.tidyverse.org/) are used. Materials are written in R using Jupyter notebook and shown as HTML files. To obtain codes and raw files, see [here](docs/gitsetup.md) for github set up. For HTML files, click on the links below.

Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

# 1. Summary Statistics

## 1.1 Tabulate and Counting

1. Many-Category Categorical Variable, Tabulation shown as Matrix *[.R](summarize/tabulate/ListUniqueCateNAsMat.R)*
  + **core**: *group_by + summarise(freq = n()) + mutate + min(ceiling(sqrt(count))) + substring + dim/reshape*
2. [By Groups, Count non-NA observations of All Variables](summarize/count/ByGroupCountAllVarNonNA.html) *[.R](summarize/count/ByGroupCountAllVarNonNA.R)*
  + **core**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))*
3. [By Groups, Count Unique Individuals and non-NA observations of other Variables](summarize/count/ByGroupCountUniqueIndi.html) *[.R](summarize/count/ByGroupCountUniqueIndi.R)*
  + **core**: *group_by + mutate_if + mutate + n_distinct + slice(1L)*

## 1.2 Averaging

1. All Variables: N + NAcount + Mean + SD + Percentiles *[.R](summarize/summ/SummPercentiles.R)*
  + **core**: *summarise_if(is.numeric) + gather + separate + spread  + select*
2. All Numeric Variables Mean + SD + N by Groups *[.R](summarize/summ/ByGroupSumm.R)*
  + **core**: *group_by + summarise_if(is.numeric(fun)) + gather + separate + spread + mutate + select + spread + unite*
3. [By Multiple within Individual Groups Variables, Averages for All Numeric Variables within All Groups of All Group Variables (Long to very Wide)](summarize/summ/ByGroupsSummWide.html) *[.R](summarize/summ/ByGroupsSummWide.R)*
  + **core**: *gather + group_by + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + gather + unite + spread (note: gather twice, spread at end)*
