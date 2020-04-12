[![HitCount](http://hits.dwyl.io/fanwangecon/R4Econ.svg)](https://github.com/FanWangEcon/R4Econ)  [![Star](https://img.shields.io/github/stars/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/watchers)

This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) consisting of R panel data and optimization examples for Statistics/Econometrics/Economic Analysis. Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R code is used. Files are from [**Fan**](https://fanwangecon.github.io/)'s [R4Econ](https://github.com/FanWangEcon/R4Econ) repository. This is not a R package, but a list of examples in PDF/HTML/Rmd formats. [REconTools](https://fanwangcon.github.com/REconTools) is a package that can be installed with tools used in [projects](https://fanwangecon.github.io/research) involving R.

Bullet points show which [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2), [tidyverse](https://www.tidyverse.org/) or other functions/commands are used to achieve various objectives. An effort is made to use only [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2) and [tidyverse](https://www.tidyverse.org/) packages whenever possible to reduce dependencies. The goal of this repository is to make it easier to find/re-use codes produced for various projects.

From [Fan](https://fanwangecon.github.io/)'s other repositories: For dynamic borrowing and savings problems, see [Dynamic Asset Repository](https://fanwangecon.github.io/CodeDynaAsset/); For code examples, see also [Matlab Example Code](https://fanwangecon.github.io/M4Econ/) and [Stata Example Code](https://fanwangecon.github.io/Stata4Econ/); For intro econ with Matlab, see [Intro Mathematics for Economists](https://fanwangecon.github.io/Math4Econ/), and for intro stat with R, see [Intro Statistics for Undergraduates](https://fanwangecon.github.io/Stat4Econ/). See [here](https://github.com/FanWangEcon) for all of [Fan](https://fanwangecon.github.io/)'s public repositories.

Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

[![](https://img.shields.io/github/last-commit/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/pulls)

# 1. Data Wrangling

## 1.1 Aggregation

1. [Count Unique Groups and Mean within Groups](https://fanwangecon.github.io/R4Econ/summarize/aggregate/fs_group_unique_agg.html): r \| ref \| [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/fs_group_unique_agg.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/fs_group_unique_agg.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/fs_group_unique_agg.html)
    + Unique groups defined by multiple values and count obs within group
    + Mean, sd, observation count for non-NA within unique groups
    + **tidy**: *group_by() + summarise(n()); group_by() + summarise_if(is.numeric, funs(mean = mean(., na.rm = TRUE), n = sum(is.na(.)==0)))*

## 1.2 Sorting and Index

1. [Sorted Index, Interval Index and Expand Value from One Row](https://fanwangecon.github.io/R4Econ/summarize/index/fs_index_populate.html): r \| ref \| [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/fs_index_populate.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/fs_index_populate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/index/fs_index_populate.html)
    + Sort and generate index for rows
    + Generate negative and positive index based on deviations
    + Populate Values from one row to other rows
    + **tidy**: *arrange + row_number(); mutate(lowest = min(Sepal.Length)); case_when(row_number()==x ~ Septal.Length); mutate(Sepal.New = Sepal.Length[Sepal.Index == 1])*

## 1.3 Tabulate and Counting

1. [Counting Basics](https://fanwangecon.github.io/R4Econ/summarize/count/fs_count_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/fs_count_basics.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/fs_count_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/count/fs_count_basics.html)
    - uncount to generate panel skeleton from years in survey
    - **tidy**: *uncount(yrs_count) %>% group_by() %>% mutate(yr = row_number() + start_yr)*
2. [Tabulation Categorical as Matrix](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R) \| html \| pdf
    + Many-Category Categorical Variable, Tabulation shown as Matrix.
    + **tidy**: *group_by + summarise(freq = n()) + mutate + min(ceiling(sqrt(count))) + substring + dim/reshape*
3. [By Groups, Count Variables Observations](summarize/count/ByGroupCountAllVarNonNA.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.R) \|  [**html**](https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountAllVarNonNA.html) \| pdf
    + By Groups, Count non-NA observations of All Variables.
    + **tidy**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))*

## 1.4 Averaging

1. [By Groups, One Variable All Statistics](summarize/summ/ByGroupSummOne.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSummOne.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSummOne.R) \| [**html**](summarize/summ/ByGroupSummOne.html) \| pdf
    + Pick stats, overall, and by multiple groups, stats as matrix or wide row with name=(ctsvar + catevar + catelabel).
    + **tidy**: *group_by + summarize_at(, funs()) + rename(!!var := !!sym(var)) + mutate(!!var := paste0(var,'str',!!!syms(vars))) + gather + unite + spread(varcates, value)*
2. [By Groups, Multiple Variables Mean + SD + N](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSumm.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupSumm.R) \| html \| pdf
    + By Groups, All Numeric Variables Mean + SD + N, groups = rows, variables = columns
    + **tidy**: *group_by + summarise_if(is.numeric(fun)) + gather + separate + spread + mutate + select + spread + unite*
3. [By Groups, Multiple Variables Mean + SD + Percentiles](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummPercentiles.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummPercentiles.R) \| html \| pdf
    + By Groups, All Numeric Variables Mean + SD + Percentiles, groups = row-groups, variables = rows
    + **tidy**: *summarise_if(is.numeric) + gather + separate + spread  + select*
4. [By within Individual Groups Variables, Averages](summarize/summ/ByGroupsSummWide.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/summ/ByGroupsSummWide.R) \| [**html**](summarize/summ/ByGroupsSummWide.html) \| pdf
    + By Multiple within Individual Groups Variables; Averages for All Numeric Variables within All Groups of All Group Variables; Long to Wide to very Wide.
    + **tidy**: *gather + group_by + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + gather + unite + spread (note: gather twice, spread at end)*

# 2. Array, Matrix, Tibble, Data Manipulations

## 2.1 List and Array Manipulation

1. [Multi-dimensional Named Lists](https://fanwangecon.github.io/R4Econ/amto/list/fs_listr.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/fs_listr.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/fs_listr.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/list/fs_listr.html)
    - Initiate Empty List
    - Named one and two dimensional lists
    - **r**: *vector(mode = "list", length = it_N); names(list) <- paste0('e',seq()); dimnames(ls2d)[[1]] <- paste0('r',seq()); dimnames(ls2d)[[2]] <- paste0('c',seq())*
2. [Array Combinations as Matrix](https://fanwangecon.github.io/R4Econ/amto/array/fs_meshr.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/fs_meshr.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/fs_meshr.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/fs_meshr.html)
    - combinations of two arrays to matrix form (meshgrid)
    - **tidy**: *expand_grid, expand.grid, dim*

## 2.2 Matrix Manipulations

1. [Matrix Basics](https://fanwangecon.github.io/R4Econ/amto/matrix/fs_genmatrix.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/fs_genmatrix.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/fs_genmatrix.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/fs_genmatrix.html)
    - generate and combine fixed and random matrixes
    - **r**: *rbind, matrix*

## 2.3 Tibble Manipulations

1. [Tibble Basics](https://fanwangecon.github.io/R4Econ/amto/tibble/fs_tib_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/fs_tib_basics.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/fs_tib_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/fs_tib_basics.html)
    - generate tibbles, rename tibble variables, tibble row and column names
    - rename numeric sequential columns with string prefix and suffix
    - tibble statistics
    - **tidy**: *as_tibble(mt_combine) %>% rename_all(~c(ar_st_varnames)); rename_at(vars(starts_with("xx")), funs(str_replace(., "yy", "yyyy")); rename_at(vars(num_range('',ar_it)), funs(paste0(st,.))); rowid_to_column(); colnames, rownames*
2. [Input Data Text](https://fanwangecon.github.io/R4Econ/summarize/dist/fst_hist_onevar.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/fst_hist_onevar.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/fst_hist_onevar.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/dist/fst_hist_onevar.html)
    - input multiple variables with comma separated text strings
    - quantitative/continuous and categorical/discrete variables
    - histogram and summary statistics
    - **tidy**: *ar_one <- c(107.72,101.28);ar_two <- c(101.72,101.28);mt_data <- cbind(ar_one, ar_two); as_tibble(mt_data)*

## 2.4 Function over Arrays, Matrix and Tibble

1. [Evaluate Function each Row of Matrix](https://fanwangecon.github.io/R4Econ/function/noloop/fs_applysapplymutate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/fs_applysapplymutate.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/fs_applysapplymutate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/noloop/fs_applysapplymutate.html)
    - evaluate function f(x_i,y_i,c), where c is a constant and x and y vary over each row of a matrix, with index i indicating rows
    - get same results using apply, sapply, and dplyr mutate
    - **tidy**: *tb %>% rowwise() %>% do(out = cur_func(., fl)) %>% unnest(out); tb %>% pmap(func) %>% unlist(); apply(mt, 1, func); sapply(ls_ar, func, ar1, ar2); rowwise() %>% mutate(out = func(inputs))*
2. [Evaluate Nonlinear Function each Row of Matrix](https://fanwangecon.github.io/R4Econ/function/mutatef/fs_funceval.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/fs_funceval.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/fs_funceval.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/mutatef/fs_funceval.html)
    - evaluate nonlinear function f(x_i, y_i, ar_x, ar_y, c, d), where c and d are constants, and ar_x and ar_y are arrays, both fixed. x_i and y_i vary over each row of matrix.
    - get same results using apply, sapply, and dplyr mutate
    - **tidy**: *rowwise() %>% mutate(out = funct(inputs))*
3. [Evaluate Function Do Anything Group Stack Results](https://fanwangecon.github.io/R4Econ/function/dof/fs_funceval_group.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/fs_funceval_group.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/fs_funceval_group.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/fs_funceval_group.html)
    - Group dataframe by categories, compute category specific output scalar or arrays based on within category variable information
    - **tidy**: *df %>% group_by(ID) %>% do(inc = rnorm(.$N, mean=.$mn, sd=.$sd)) %>% unnest(c(inc)) %>% left_join(df, by="ID")*

## 2.5 Distributions

1. [Quantiles from Multiple Variables](generate/quantile/VarCateIdxVarsQuantiles.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/generate/quantile/VarCateIdxVarsQuantiles.R) \| [**html**](generate/quantile/VarCateIdxVarsQuantiles.html) \| pdf
    + Dataframe of Variables' Quantiles by Panel Groups; Quantile Categorical Variables for Panel within Group Observations; Quantile cut variable suffix and quantile labeling; Joint Quantile Categorical Variable with Linear Index.
    + **tidy**: *group_by + slice(1L) + lapply(enframe(quantiles())) + reduce(full_join) + mutate_at(funs(q=f_cut(.,cut)))) + levels() + rename_at + unlist(lapply) + mutate(!!var.qjnt.grp.idx := group_indices(., !!!syms(vars.quantile.cut.all)))*

# 3. Regressions

## 3.1 Linear Regressions

1. [IV/OLS Regression](linreg/ivreg/ivregdfrow.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/ivregdfrow.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/ivregdfrow.R) \| [**html**](linreg/ivreg/ivregdfrow.html) \| pdf
    + IV/OLS Regression store all Coefficients and Diagnostics as Dataframe Row.
    + **tidy**: *library(aer) + ivreg(as.formula, diagnostics = TRUE) + gather + drop_na + unite*
2. [M Outcomes and N RHS Alternatives](linreg/ivreg/regloop.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/regloop.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/regloop.R) \| [**html**](linreg/ivreg/regloop.html) \| pdf
    + There are M outcome variables and N alternative explanatory variables. Regress all M outcome variables on N endogenous/independent right hand side variables one by one, with controls and/or IVs, collect coefficients.
    + **tidy**: *bind_rows(lapply(listx, function(x)(bind_rows(lapply(listy, regf.iv)))) + select/starts_with/ends_with + reduce(full_join)*
3. [Regression Decomposition](linreg/decompose/decompose.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/decompose.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/decompose.R) \| [**html**](linreg/decompose/decompose.html) \| pdf
    + Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
    + **tidy**: *gather + group_by(variable) + mutate_at(vars, funs(mean = mean(.))) + rowSums(mat*mat) + mutate_if(is.numeric, funs(frac = (./value_var)))*

## 3.2 Nonlinear Regressions

1. [Logit Regression](https://fanwangecon.github.io/R4Econ/regnonlin/logit/fs_logit_birhs.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/fs_logit_birhs.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/fs_logit_birhs.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/logit/fs_logit_birhs.html)
    + Logit regression testing and prediction
    + **tidy**: *glm(as.formula(), data, family='binomial'); predict(rs, newdata, type = "response")*

# 4. Optimization

## 4.1 Bisection

1. [Concurrent Bisection over Dataframe Rows](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/fs_func_graph_eval.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/fs_func_graph_eval.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/fs_func_graph_eval.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/fs_func_graph_eval.html)
    + Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
    + **case_when**: *mutate(!!sym(abc) := case_when(efg < 0 ~ !!sym(opq), TRUE ~ iso))*
    + **pivot_long**: *df %>% pivot_longer(cols = starts_with('abc'), names_to = c('a', 'b'), names_pattern = paste0('prefix', "(.)_(.)"),values_to = val)*
    + **pivot_wider**: *df %>% pivot_wider(names_from = !!sym(name), values_from = val)*
    + **ggplot**: *geom_line, facet_wrap, geom_hline*

## 4.2 Planner's Problem

1. [CES Objective Function](optimization/planner/ces/cesplannerobj.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesplannerobj.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesplannerobj.R) \| [**html**](optimization/planner/ces/cesplannerobj.html) \| pdf
    + Constant Elasticity of Substitution Planner Welfare Objective Function.
    + **tidy**: *prod/mean/pow, logspace, geom_bar+identity+dodge*
2. [CES Subsidy Optimization Over Panel Groups](optimization/planner/ces/cesoptimizer.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesoptimizer.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/planner/ces/cesoptimizer.R) \| [**html**](optimization/planner/ces/cesoptimizer.html) \| pdf
    + Constant Elasticity of Substitution Planner Welfare Subsidies Optimizer Over Quantile/Individual Groups.
    + **tidy**: *optim(x, obj, func.params), do.call(func_str, func.params); setNames+list+append*

## 4.3 Optimization Support
1. [Constrained Share Parameters to Unconstrained Parameters](optimization/support/fraction.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/support/fraction.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/support/fraction.R) \| [**html**](optimization/support/fraction.html) \| pdf
    + Constrained: a + b + c = Z, a >= 0, b >= 0, c >= 0; Unconstrained maximands of a and b for optimization.
    + **tidy**: *f - f/(1+exp(x)), while, runif + qexp + qnorm/dnorm*

# 5. Mathmatical

1. [Aktinson Utility](https://fanwangecon.github.io/R4Econ/math/functions/fs_atkinson_ces.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/functions/fs_atkinson_ces.Rmd) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/functions/fs_atkinson_ces.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/functions/fs_atkinson_ces.html)
    - Aktinson Class Utility (CES) Derivatives

# 6. Additional Tools

1. [List of List to Dataframe](https://github.com/FanWangEcon/R4Econ/blob/master/support/dplyrtricks/nestedlist2df.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/support/dplyrtricks/nestedlist2df.R) \| html \| pdf
    + Results stored as nested named list (with different keys in sub-lists).
    + **tidy**: *as.data.frame + gather + separate(sep(\\.), extra='merge') + spread + column_to_rownames*

# 7. Support

1. [Installations](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/condainstalls.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/condainstalls.R) \| html \| pdf
    + Conda R Package Installations.
2. [Controls](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/controls.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/support/controls/controls.R) \| html \| pdf
    + Graph Sizing, Warnings, Table Col/Row Max Display, Timer.

----
Please contact [![](https://img.shields.io/github/followers/fanwangecon?label=FanWangEcon&style=social)](https://github.com/FanWangEcon) [![](https://img.shields.io/twitter/follow/fanwangecon?label=%20&style=social)](https://twitter.com/fanwangecon) for issues or problems.

![RepoSize](https://img.shields.io/github/repo-size/fanwangecon/R4Econ)
![CodeSize](https://img.shields.io/github/languages/code-size/fanwangecon/R4Econ)
![Language](https://img.shields.io/github/languages/top/fanwangecon/R4Econ)
![Release](https://img.shields.io/github/downloads/fanwangecon/R4Econ/total)
![License](https://img.shields.io/github/license/fanwangecon/R4Econ)
