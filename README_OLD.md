[![HitCount](http://hits.dwyl.com/fanwangecon/R4Econ.svg)](https://github.com/FanWangEcon/R4Econ)  [![Star](https://img.shields.io/github/stars/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/watchers)

This is a work-in-progress [website](https://fanwangecon.github.io/R4Econ/) consisting of R panel data and optimization examples for Statistics/Econometrics/Economic Analysis. Book version: [bookdown site](https://fanwangecon.github.io/R4Econ/bookdown) and [bookdown pdf](https://fanwangecon.github.io/R4Econ/bookdown/Panel-Data-and-Optimization-with-R.pdf). Materials gathered from various [projects](https://fanwangecon.github.io/research) in which R code is used. Files are from [**Fan**](https://fanwangecon.github.io/)'s [R4Econ](https://github.com/FanWangEcon/R4Econ) repository. This is not a R package, but a list of examples in PDF/HTML/Rmd formats. [REconTools](https://fanwangecon.github.io/REconTools/) is a package that can be installed with tools used in [projects](https://fanwangecon.github.io/research) involving R.

Bullet points show which [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2), [tidyverse](https://www.tidyverse.org/) or other functions/commands are used to achieve various objectives. An effort is made to use only [base R](https://www.rdocumentation.org/packages/base/versions/3.5.2) and [tidyverse](https://www.tidyverse.org/) packages whenever possible to reduce dependencies. The goal of this repository is to make it easier to find/re-use codes produced for various projects.

From [Fan](https://fanwangecon.github.io/)'s other repositories: For dynamic borrowing and savings problems, see [Dynamic Asset Repository](https://fanwangecon.github.io/CodeDynaAsset/); For code examples, see also [Matlab Example Code](https://fanwangecon.github.io/M4Econ/) and [Stata Example Code](https://fanwangecon.github.io/Stata4Econ/); For intro econ with Matlab, see [Intro Mathematics for Economists](https://fanwangecon.github.io/Math4Econ/), and for intro stat with R, see [Intro Statistics for Undergraduates](https://fanwangecon.github.io/Stat4Econ/). See [here](https://github.com/FanWangEcon) for all of [Fan](https://fanwangecon.github.io/)'s public repositories.

Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

[![](https://img.shields.io/github/last-commit/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/pulls)

# 1  Array, Matrix, Dataframe

## 1.1  List

1. [Multi-dimensional Named Lists](https://fanwangecon.github.io/R4Econ/amto/list/htmlpdfr/fs_lst_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list//fs_lst_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/htmlpdfr/fs_lst_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/htmlpdfr/fs_lst_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/list/htmlpdfr/fs_lst_basics.html)
	+ Initiate Empty List. Named one and two dimensional lists.
	+ **r**: *vector(mode = "list", length = it_N) + names(list) <- paste0('e',seq()) + dimnames(ls2d)[[1]] <- paste0('r',seq()) + dimnames(ls2d)[[2]] <- paste0('c',seq())*
	+ **tidyr**: *unnest()*

## 1.2  Array

1. [Arrays Operations in R](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_basics.html)
	+ Basic array operations in R.
	+ **r**: *head() + tail() + na_if()*
2. [Generate Special Arrays](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_generate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_generate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_generate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_generate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_generate.html)
	+ Generate special arrays: log spaced array
	+ **r**: *seq()*
3. [String Operations](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_string.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_string.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_string.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_string.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_string.html)
	+ Split, concatenate, subset strings
	+ **r**: *paste0() + sub() + gsub() + grepl() + sprintf() + tail() + strsplit() + basename() + dirname()*
4. [Array Combinations as Matrix](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_mesh.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_mesh.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_mesh.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_mesh.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_mesh.html)
	+ Combinations of two arrays to matrix form (meshgrid)
	+ **tidyr**: *expand_grid() + expand.grid()*

## 1.3  Matrix

1. [Matrix Basics](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_generate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix//fs_mat_generate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_generate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_generate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_generate.html)
	+ Generate and combine fixed and random matrixes
	+ **R**: *rbind() + matrix*
2. [Linear Algebra Operations](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_linear_algebra.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix//fs_mat_linear_algebra.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_linear_algebra.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_linear_algebra.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_linear_algebra.html)

## 1.4  Variables in Dataframes

1. [Tibble Basics](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_basics.html)
	+ generate tibbles, rename tibble variables, tibble row and column names
	+ rename numeric sequential columns with string prefix and suffix
	+ **dplyr**: *as_tibble(mt) + rename_all(~c(ar_names)) + rename_at(vars(starts_with("xx")), funs(str_replace(., "yy", "yyyy")) + rename_at(vars(num_range('',ar_it)), funs(paste0(st,.))) + rowid_to_column() + colnames + rownames*
2. [Label and Combine Factor Variables](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_factors.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_factors.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_factors.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_factors.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_factors.html)
	+ Convert numeric variables to factor variables, generate joint factors, and label factors.
	+ Graph MPG and 1/4 Miles Time (qsec) from the mtcars dataset over joint shift-type (am) and engine-type (vs) categories.
	+ **forcats**: *as_factor() + fct_recode() + fct_cross()*
3. [Examples of Random Draws in R](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_random_draws.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_random_draws.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_random_draws.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_random_draws.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_random_draws.html)
4. [R Tibble Dataframe NA Values](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_na.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_na.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_na.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html)
5. [R Tibble Dataframe String Manipulations](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_string.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_string.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_string.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_string.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_string.html)

# 2  Summarize Data

## 2.1  Counting Observation

1. [Counting Basics](https://fanwangecon.github.io/R4Econ/summarize/count/htmlpdfr/fs_count_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count//fs_count_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/htmlpdfr/fs_count_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/htmlpdfr/fs_count_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/count/htmlpdfr/fs_count_basics.html)
	+ uncount to generate panel skeleton from years in survey
	+ **dplyr**: *uncount(yr_n) + group_by() + mutate(yr = row_number() + start_yr)*

## 2.2  Sorting, Indexing, Slicing

1. [Sorted Index, Interval Index and Expand Value from One Row](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_index_populate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index//fs_index_populate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_index_populate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_index_populate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_index_populate.html)
	+ Sort and generate index for rows
	+ Generate negative and positive index based on deviations
	+ Populate Values from one row to other rows
	+ **dplyr**: *arrange() + row_number() + mutate(lowest = min(Sepal.Length)) + case_when(row_number()==x ~ Septal.Length) + mutate(Sepal.New = Sepal.Length[Sepal.Index == 1])*

## 2.3  Group Statistics

1. [Count Unique Groups and Mean within Groups](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_unique_agg.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_unique_agg.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_unique_agg.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_unique_agg.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_unique_agg.html)
	+ Unique groups defined by multiple values and count obs within group.
	+ Mean, sd, observation count for non-NA within unique groups.
	+ **dplyr**: *group_by() + summarise(n()) + summarise_if(is.numeric, funs(mean = mean(., na.rm = TRUE), n = sum(is.na(.)==0)))*
2. [By Groups, One Variable All Statistics](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_one.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_summ_one.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_one.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_one.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_one.html)
	+ Pick stats, overall, and by multiple groups, stats as matrix or wide row with name=(ctsvar + catevar + catelabel).
	+ **tidyr**: *group_by() + summarize_at(, funs()) + rename(!!var := !!sym(var)) + mutate(!!var := paste0(var,'str',!!!syms(vars))) + gather() + unite() + spread(varcates, value)*
3. [By within Individual Groups Variables, Averages](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_wide.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_summ_wide.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_wide.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_wide.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_wide.html)
	+ By Multiple within Individual Groups Variables.
	+ Averages for all numeric variables within all groups of all group variables. Long to Wide to very Wide.
	+ **tidyr**: *gather() + group_by() + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + unite() + spread()*

## 2.4  Distributional Statistics

1. [Tibble Basics](https://fanwangecon.github.io/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist//fst_hist_onevar.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/htmlpdfr/fst_hist_onevar.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/htmlpdfr/fst_hist_onevar.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.html)
	+ input multiple variables with comma separated text strings
	+ quantitative/continuous and categorical/discrete variables
	+ histogram and summary statistics
	+ **tibble**: *ar_one <- c(107.72,101.28) + ar_two <- c(101.72,101.28) + mt_data <- cbind(ar_one, ar_two) + as_tibble(mt_data)*

## 2.5  Summarize Multiple Variables

1. [R Example Apply the Same Function Over Multiple Variables](https://fanwangecon.github.io/R4Econ/summarize/multivar/htmlpdfr/fs_func_multivar.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar//fs_func_multivar.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar/htmlpdfr/fs_func_multivar.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar/htmlpdfr/fs_func_multivar.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/multivar/htmlpdfr/fs_func_multivar.html)

# 3  Functions

## 3.1  Dataframe Mutate

1. [Nonlinear Function over Rows](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_funceval.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef//fs_funceval.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_funceval.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_funceval.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_funceval.html)
	+ Evaluate nonlinear function f(x_i, y_i, ar_x, ar_y, c, d), where c and d are constants, and ar_x and ar_y are arrays, both fixed. x_i and y_i vary over each row of matrix.
	+ **dplyr**: *rowwise() + mutate(out = funct(inputs))*
2. [DPLYR Evaluate Functions at Many States and Choices Each State](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_func_choice_states.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef//fs_func_choice_states.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_func_choice_states.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_func_choice_states.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_func_choice_states.html)

## 3.2  Dataframe Do Anything

1. [Evaluate Function Do Anything Group Stack Results](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_group.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof//fs_funceval_group.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_group.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_group.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_group.html)
	+ Group dataframe by categories, compute category specific output scalar or arrays based on within category variable information.
	+ **dplyr**: *group_by(ID) + do(inc = rnorm(.$N, mean=.$mn, sd=.$sd)) + unnest(c(inc)) + left_join(df, by="ID")*
2. [DPLYR Expand Dataframe with Function](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_expand.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof//fs_funceval_expand.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_expand.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_expand.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_expand.html)

## 3.3  Apply and pmap

1. [Apply and Mutate over Rows](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_applysapplymutate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop//fs_applysapplymutate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_applysapplymutate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_applysapplymutate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_applysapplymutate.html)
	+ Evaluate function f(x_i,y_i,c), where c is a constant and x and y vary over each row of a matrix, with index i indicating rows.
	+ Get same results using apply, sapply, and dplyr mutate.
	+ **r**: *do.call() + apply(mt, 1, func) + sapply(ls_ar, func, ar1, ar2)*
	+ **purrr**: *rowwise() + unnest(out) + pmap(func) + unlist()*

# 4  Panel

## 4.1  Generate and Join

1. [TIDYVERSE Generate Panel Data Structures](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_genpanel.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic//fs_genpanel.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_genpanel.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_genpanel.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_genpanel.html)
	+ Build skeleton panel frame with N observations and T periods.
	+ **tidyr**: *rowid_to_column() + uncount() + group_by() + row_number() + ungroup()*
2. [R DPLYR Join Multiple Dataframes Together](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_joining.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic//fs_joining.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_joining.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_joining.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_joining.html)
	+ Join dataframes together with one or multiple keys. Stack dataframes together.
	+ **dplyr**: *filter() + rename(!!sym(vsta) := !!sym(vstb)) + mutate(var = rnom(n())) + left_join(df, by=(c('id'='id', 'vt'='vt'))) + left_join(df, by=setNames(c('id', 'vt'), c('id', 'vt'))) + bind_rows()*

## 4.2  Wide and Long

1. [TIDYR Pivot Wider and Pivot Longer Examples](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotwider.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong//fs_pivotwider.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotwider.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotwider.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotwider.html)
	+ Long roster to wide roster and cumulative sum attendance by date.
	+ **dplyr**: *mutate(var = case_when(rnorm(n()) < 0 ~ 1, TRUE ~ 0)) + rename_at(vars(num_range('', ar_it)), list(~paste0(st_prefix, . , ''))) + mutate_at(vars(contains(str)), list(~replace_na(., 0))) + mutate_at(vars(contains(str)), list(~cumsum(.)))*

# 5  Linear Regression

## 5.1  OLS and IV

1. [IV/OLS Regression](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg//fs_lin_ivregrow.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.html)
	+ R Instrumental Variables and Ordinary Least Square Regression store all Coefficients and Diagnostics as Dataframe Row.
	+ **aer**: *library(aer) + ivreg(as.formula, diagnostics = TRUE) *
2. [M Outcomes and N RHS Alternatives](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivloop.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg//fs_lin_ivloop.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivloop.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivloop.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivloop.html)
	+ There are M outcome variables and N alternative explanatory variables. Regress all M outcome variables on N endogenous/independent right hand side variables one by one, with controls and/or IVs, collect coefficients.
	+ **dplyr**: *bind_rows(lapply(listx, function(x)(bind_rows(lapply(listy, regf.iv))) + starts_with() + ends_with() + reduce(full_join)*

## 5.2  Decomposition

1. [Regression Decomposition](https://fanwangecon.github.io/R4Econ/linreg/decompose/htmlpdfr/fs_lin_decompose.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose//fs_lin_decompose.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/htmlpdfr/fs_lin_decompose.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/htmlpdfr/fs_lin_decompose.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/decompose/htmlpdfr/fs_lin_decompose.html)
	+ Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
	+ **dplyr**: *gather() + group_by(var) + mutate_at(vars, funs(mean = mean(.))) + rowSums(mat*mat) + mutate_if(is.numeric, funs(frac = (./value_var)))*

# 6  Nonlinear Regression

## 6.1  Logit Regression

1. [Logit Regression](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_birhs.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit//fs_logit_birhs.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_birhs.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_birhs.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_birhs.html)
	+ Logit regression testing and prediction.
	+ **stats**: *glm(as.formula(), data, family='binomial') + predict(rs, newdata, type = "response")*

# 7  Optimization

## 7.1  Bisection

1. [Concurrent Bisection over Dataframe Rows](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_bisec_joint.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect//fs_bisec_joint.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_bisec_joint.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_bisec_joint.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_bisec_joint.html)
	+ Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
	+ **tidyr**: *pivot_longer(cols = starts_with('abc'), names_to = c('a', 'b'), names_pattern = paste0('prefix', "(.)_(.)"), values_to = val) + pivot_wider(names_from = !!sym(name), values_from = val) + mutate(!!sym(abc) := case_when(efg < 0 ~ !!sym(opq), TRUE ~ iso))*
	+ **gglot2**: *geom_line() + facet_wrap() + geom_hline()*

# 8  Mathmatics and Statistics

## 8.1  Distributions

1. [Integrate Normal Shocks](https://fanwangecon.github.io/R4Econ/math/integration/htmlpdfr/fs_integrate_normal.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/integration//fs_integrate_normal.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/integration/htmlpdfr/fs_integrate_normal.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/integration/htmlpdfr/fs_integrate_normal.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/integration/htmlpdfr/fs_integrate_normal.html)
	+ Random Sampling (Monte Carlo) integrate shocks.
	+ Trapezoidal rule (symmetric rectangles) integrate normal shock.

## 8.2  Analytical Solutions

1. [linear solve x with f(x) = 0](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_solu_x_lin.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_solu_x_lin.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_solu_x_lin.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_solu_x_lin.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_solu_x_lin.html)
	+ Evaluate and solve statistically relevant problems with one equation and one unknown that permit analytical solutions.

## 8.3  Inequality Models

1. [Gini for Discrete Samples](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_gini_disc.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_gini_disc.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_gini_disc.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html)
	+ Given sample of data points that are discrete, compute the approximate gini coefficient.
	+ **r**: *sort() + cumsum() + sum()*
2. [CES abd Atkinson Utility](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_atkinson_ces.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_atkinson_ces.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_atkinson_ces.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_atkinson_ces.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_atkinson_ces.html)
	+ Analyze how changing individual outcomes shift utility given inequality preference parameters.
	+ Draw Cobb-Douglas, Utilitarian and Leontief indifference curve
	+ **r**: *apply(mt, 1, funct(x){}) + do.call(rbind, ls_mt)*
	+ **tidyr**: *expand_grid()*
	+ **ggplot2**: *geom_line() + facet_wrap()*

----
Please contact [![](https://img.shields.io/github/followers/fanwangecon?label=FanWangEcon&style=social)](https://github.com/FanWangEcon) [![](https://img.shields.io/twitter/follow/fanwangecon?label=%20&style=social)](https://twitter.com/fanwangecon) for issues or problems.

![RepoSize](https://img.shields.io/github/repo-size/fanwangecon/R4Econ)
![CodeSize](https://img.shields.io/github/languages/code-size/fanwangecon/R4Econ)
![Language](https://img.shields.io/github/languages/top/fanwangecon/R4Econ)
![Release](https://img.shields.io/github/downloads/fanwangecon/R4Econ/total)
![License](https://img.shields.io/github/license/fanwangecon/R4Econ)
