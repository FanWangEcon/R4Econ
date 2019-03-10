
This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) of support files for doing Panel Data Statistics/Econometrics Analyasis, produced by [Fan](https://fanwangecon.github.io/). Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R codes are used. The goal of this repository is to make it easier to find/re-use codes produced for various projects.

R files are linked below by section. Some files have examples/instructions created using Jupyter notebooks and are shown as HTML files. To obtain codes and raw files, see [here](docs/gitsetup.md) for github set up. For HTML files, click on the links below.

Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

# 1. Summary Statistics

## 1.1 Tabulate and Counting

1. Many-Category Categorical Variable, Tabulation shown as Matrix **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R)**
  + **core**: *group_by + summarise(freq = n()) + mutate + min(ceiling(sqrt(count))) + substring + dim/reshape*
2. [By Groups, Count non-NA observations of All Variables](summarize/count/ByGroupCountAllVarNonNA.html) **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.R)**
  + **core**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))*
3. [By Groups, Count Unique Individuals and non-NA observations of other Variables](summarize/count/ByGroupCountUniqueIndi.html) **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountUniqueIndi.R)**
  + **core**: *group_by + mutate_if + mutate + n_distinct + slice(1L)*

## 1.2 Averaging

1. All Variables: N + NAcount + Mean + SD + Percentiles **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/SummPercentiles.R)**
  + **core**: *summarise_if(is.numeric) + gather + separate + spread  + select*
2. All Numeric Variables Mean + SD + N by Groups **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSumm.R)**
  + **core**: *group_by + summarise_if(is.numeric(fun)) + gather + separate + spread + mutate + select + spread + unite*
3. [Average By Multiple within Individual Groups Variables](summarize/summ/ByGroupsSummWide.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.ipynb) \| [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.R) \|  [**html**](summarize/summ/ByGroupsSummWide.html) \| pdf
  + By Multiple within Individual Groups Variables
  + Averages for All Numeric Variables within All Groups of All Group Variables
  + Long to Wide to very Wide
  + **core**: *gather + group_by + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + gather + unite + spread (note: gather twice, spread at end)*


# 2. Data/Variable Generation
1. [Quantiles from Multiple Variables](generate/quantile/VarCateIdxVarsQuantiles.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.ipynb) \| [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.R) \|  [**html**](generate/quantile/VarCateIdxVarsQuantiles.html) \| pdf
  + Dataframe of Variables' Quantiles by Panel Groups
  + Quantile Categorical Variables for Panel within Group Observations
  + Quantile cut variable suffix and quantile labeling
  + Joint Quantile Categorical Variable with Linear Index
  + **core**: *group_by + slicke(1L) + lapply(quantiles()) + reduce(full_join) + mutate_at(funs(q=f_cut(.,cut)))) + levels() + rename_at*


# 3. Linear Regressions

## 3.1 Instrumental Variables

1. IV Regression store all Coefficients and Diagnostics as Dataframe Row **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/ivregdfrow.R)**
  + **core**: *library(aer) + ivreg(as.formula, diagnostics = TRUE) + gather + drop_na + unite*

# 4. Support

1. Conda R Package Installations **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/condainstalls.R)**
2. Controls for: Graph Sizing, Warnings, Table Col/Row Max Display **[.R](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/controls.R)**
