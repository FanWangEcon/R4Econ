This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) for [**Fan**](https://fanwangecon.github.io/)'s [R4Econ](https://github.com/FanWangEcon/R4Econ) repository consisting of support files for doing Panel Data Statistics/Econometrics Analysis. Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R codes are used.

# R Code from Various Projects using Panel Data

R files are linked below by section. Some R files are in the R4Econ package, others are outside of the R folder. To use all files, clone the repository. For functions not in R folder, source the [preamble.R](https://github.com/FanWangEcon/R4Econ/blob/master/preamble.R) file. Files in R folder have documentations and examples, see the reference link. Some files also have examples/instructions created using Jupyter notebooks and are shown as HTML files. See [here](gitsetup.md) for Github set up.

```
# To Install only the Programs in the R folder of the R4Econ Repository
devtools::install_github("fanwangecon/R4Econ")
```

Bullet points show which [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2), [tidyverse](https://www.tidyverse.org/) or other functions/commands are used to achieve various objectives. An effort is made to use only [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2) and [tidyverse](https://www.tidyverse.org/) packages whenever possible to reduce dependencies. The goal of this repository is to make it easier to find/re-use codes produced for various projects.

Please contact [FanWangEcon](https://github.com/FanWangEcon) for issues or problems.

# 1. Summary Statistics

## 1.1 Tabulate and Counting
1. [Tabulation Categorical as Matrix](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R) | html | pdf
    + Many-Category Categorical Variable, Tabulation shown as Matrix.
    + **core**: *group_by + summarise(freq = n()) + mutate + min(ceiling(sqrt(count))) + substring + dim/reshape*
2. [By Groups, Count Variables Observations](summarize/count/ByGroupCountAllVarNonNA.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.R) |  [**html**](summarize/count/ByGroupCountAllVarNonNA.html) | pdf
    + By Groups, Count non-NA observations of All Variables.
    + **core**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))*
3. [By Groups, Count Unique Individuals](https://fanwangecon.github.io/R4Econ/reference/ff_summ_count_unique_by_groups.html): [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/R/ff_count.R) | [html](https://fanwangecon.github.io/R4Econ/reference/ff_summ_count_unique_by_groups.html)
    + By Groups, Count Unique Individuals and non-NA observations of other Variables.
    + **core**: *group_by + mutate_if + mutate + n_distinct + slice(1L)*

## 1.2 Averaging

1. [All Variables Summary Stats](https://github.com/FanWangEcon/R4Econ/blob/master/R/ff_summ_percentiles.R): [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/R/ff_summ_percentiles.R) | [html](https://fanwangecon.github.io/R4Econ/reference/ff_summ_percentiles.html)
    + All Variables: N + NAcount + Mean + SD + Percentiles.
    + **core**: *summarise_if(is.numeric) + gather + separate + spread  + select*
2. [By Groups, One Variable All Statistics](summarize/summ/ByGroupSummOne.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSummOne.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSummOne.R) | [**html**](summarize/summ/ByGroupSummOne.html) | pdf
    + Pick stats, overall, and by multiple groups, stats as matrix or wide row with name=(ctsvar + catevar + catelabel).
    + **core**: *group_by + summarize_at(, funs()) + rename(!!var := !!sym(var)) + mutate(!!var := paste0(var,'str',!!!syms(vars))) + gather + unite + spread(varcates, value)*
3. [By Groups, Multiple Variables Mean + SD + N](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSumm.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSumm.R) | html | pdf
    + By Groups, All Numeric Variables Mean + SD + N, groups = rows, variables = columns
    + **core**: *group_by + summarise_if(is.numeric(fun)) + gather + separate + spread + mutate + select + spread + unite*
4. [By Groups, Multiple Variables Mean + SD + Percentiles](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummPercentiles.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummPercentiles.R) | html | pdf
    + By Groups, All Numeric Variables Mean + SD + Percentiles, groups = row-groups, variables = rows
    + **core**: *summarise_if(is.numeric) + gather + separate + spread  + select*        
5. [By within Individual Groups Variables, Averages](summarize/summ/ByGroupsSummWide.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.R) |  [**html**](summarize/summ/ByGroupsSummWide.html) | pdf
    + By Multiple within Individual Groups Variables; Averages for All Numeric Variables within All Groups of All Group Variables; Long to Wide to very Wide.
    + **core**: *gather + group_by + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + gather + unite + spread (note: gather twice, spread at end)*

# 2. Data/Variable Generation

1. [Quantiles from Multiple Variables](generate/quantile/VarCateIdxVarsQuantiles.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.R) |  [**html**](generate/quantile/VarCateIdxVarsQuantiles.html) | pdf
    + Dataframe of Variables' Quantiles by Panel Groups; Quantile Categorical Variables for Panel within Group Observations; Quantile cut variable suffix and quantile labeling; Joint Quantile Categorical Variable with Linear Index.
    + **core**: *group_by + slicke(1L) + lapply(enframe(quantiles())) + reduce(full_join) + mutate_at(funs(q=f_cut(.,cut)))) + levels() + rename_at + unlist(lapply) + mutate(!!var.qjnt.grp.idx := group_indices(., !!!syms(vars.quantile.cut.all)))*


# 3. Linear Regressions

1. [IV/OLS Regression](linreg/ivreg/ivregdfrow.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/ivregdfrow.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/ivregdfrow.R) |  [**html**](linreg/ivreg/ivregdfrow.html) | pdf
    + IV/OLS Regression store all Coefficients and Diagnostics as Dataframe Row.
    + **core**: *library(aer) + ivreg(as.formula, diagnostics = TRUE) + gather + drop_na + unite*
2. [M Outcomes and N RHS Alternatives](linreg/ivreg/regloop.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/regloop.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/regloop.R) |  [**html**](linreg/ivreg/regloop.html) | pdf
    + There are M outcome variables and N alternative explanatory variables. Regress all M outcome variables on N endogenous/independent right hand side variables one by one, with controls and/or IVs, collect coefficients.
    + **core**: *bind_rows(lapply(listx, function(x)(bind_rows(lapply(listy, regf.iv)))) + select/starts_with/ends_with + reduce(full_join)*
3. [Regression Decomposition](linreg/decompose/decompose.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/decompose.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/decompose.R) |  [**html**](linreg/decompose/decompose.html) | pdf
    + Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
    + **core**: *gather + group_by(variable) + mutate_at(vars, funs(mean = mean(.))) + rowSums(mat*mat) + mutate_if(is.numeric, funs(frac = (./value_var)))*


# 4. Optimization

## 4.1 Planner's Problem
1. [CES Objective Function](optimization/planner/ces/cesplannerobj.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesplannerobj.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesplannerobj.R) |  [**html**](optimization/planner/ces/cesplannerobj.html) | pdf
    + Constant Elasticity of Substitution Planner Welfare Objective Function.
    + **core**: *prod/mean/pow, logspace, geom_bar+identity+dodge*
2. [CES Subsidy Optimization Over Panel Groups](optimization/planner/ces/cesoptimizer.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesoptimizer.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesoptimizer.R) |  [**html**](optimization/planner/ces/cesoptimizer.html) | pdf
    + Constant Elasticity of Substitution Planner Welfare Subsidies Optimizer Over Quantile/Individual Groups.
    + **core**: *optim(x, obj, func.params), do.call(func_str, func.params); setNames+list+append*  

## 4.2 Optimization Support
1. [Constrained Share Parameters to Unconstrained Parameters](optimization/support/fraction.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/support/fraction.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/support/fraction.R) |  [**html**](optimization/support/fraction.html) | pdf
    + Constrained: a + b + c = Z, a >= 0, b >= 0, c >= 0; Unconstrained maximands of a and b for optimization.
    + **core**: *f - f/(1+exp(x)), while, runif + qexp + qnorm/dnorm*

# 5. Graphing

1. [Line Plot with Two Categories, as Color and Subplot](dynamic/graph/statesvalpol.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/dynamic/graph/statesvalpol.ipynb) | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/dynamic/graph/statesvalpol.R) |  [**html**](dynamic/graph/statesvalpol.html) | pdf
    + Optimal choices/value-function along states. Asset as X-axis, shocks as color, potentially another state as subplots.
    + **core**: *unique + mutate(var := as.factor(var)) + ggplot + facet_wrap + geom_line + geom_point + labs + theme(axis.text.x = element_text(angle = 90, hjust = 1))*


# 6. Tools

1. [List of List to Dataframe](https://github.com/FanWangEcon/R4Econ/blob/master/support/dplyrtricks/nestedlist2df.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/support/dplyrtricks/nestedlist2df.R) | html | pdf
    + Results stored as nested named list (with different keys in sub-lists).
    + **core**: *as.data.frame + gather + separate(sep(\\.), extra='merge') + spread + column_to_rownames*

# 7. Support

1. [Installations](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/condainstalls.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/condainstalls.R) | html | pdf
    + Conda R Package Installations.
2. [Controls](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/controls.R): ipynb | [**R**](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/controls.R) | html | pdf
    + Graph Sizing, Warnings, Table Col/Row Max Display, Timer.
