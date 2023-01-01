# (APPENDIX) Appendix {-}

# Index and Code Links

## Array, Matrix, Dataframe links

### [Section 1.1 List][List] links

1. [Multi-dimensional Named Lists](https://fanwangecon.github.io/R4Econ/amto/list/htmlpdfr/fs_lst_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list//fs_lst_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/htmlpdfr/fs_lst_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/list/htmlpdfr/fs_lst_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/list/htmlpdfr/fs_lst_basics.html)
	+ Initiate Empty List. Named one and two dimensional lists. List of Dataframes.
	+ Collapse named and unamed list to string and print input code.
	+ **r**: *deparse(substitute()) + vector(mode = "list", length = it_N) + names(list) <- paste0('e',seq()) + dimnames(ls2d)[[1]] <- paste0('r',seq()) + dimnames(ls2d)[[2]] <- paste0('c',seq())*
	+ **tidyr**: *unnest()*

### [Section 1.2 Array][Array] links

1. [Basic Arrays Operations in R](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_basics.html)
	+ Generate N-dimensional array of NA values, label dimension elements.
	+ Basic array operations in R, rep, head, tail, na, etc.
	+ E notation.
	+ Get N cuts from M points.
	+ **r**: *sum() + prod() + rep() + array(NA, dim=c(3, 3)) + array(NA, dim=c(3, 3, 3)) + dimnames(mn)[[3]] = paste0('k=', 0:4) + head() + tail() + na_if() + Re()*
2. [Generate Special Arrays](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_generate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_generate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_generate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_generate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_generate.html)
	+ Generate equi-distance, special log spaced array.
	+ Generate probability mass function with non-unique and non-sorted value and probability arrays.
	+ **r**: *seq() + sort() + runif() + ceiling()*
	+ **stats**: *aggregate()*
3. [String Operations](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_string.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_string.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_string.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_string.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_string.html)
	+ Split, concatenate, subset, replace, and substring strings.
	+ Convert number to string without decimal and negative sign.
	+ Concatenate numeric and string arrays as a single string.
	+ **r**: *paste0() + paste0(round(runif(3),3), collapse=',') + sub() + gsub() + grepl() + sprintf()*
4. [Meshgrid Matrices, Arrays and Scalars](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_mesh.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array//fs_ary_mesh.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_mesh.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/array/htmlpdfr/fs_ary_mesh.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/array/htmlpdfr/fs_ary_mesh.html)
	+ Meshgrid Matrices, Arrays and Scalars to form all combination dataframe.
	+ **tidyr**: *expand_grid() + expand.grid()*

### [Section 1.3 Matrix][Matrix] links

1. [Matrix Basics](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_generate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix//fs_mat_generate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_generate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_generate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_generate.html)
	+ Generate and combine NA, fixed and random matrixes. Name columns and rows.
	+ Sort all rows and all columns of a matrix.
	+ Replace values outside min and max in matrix by NA values.
	+ **R**: *rep() + rbind() + matrix(NA) + matrix(NA_real_) + matrix(NA_integer_) + colnames() + rownames() + t(apply(mt, 1, sort)) + apply(mt, 2, sort) + colMeans + rowMeans + which()*
2. [Linear Algebra Operations](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_linear_algebra.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix//fs_mat_linear_algebra.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_linear_algebra.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_linear_algebra.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_linear_algebra.html)
3. [Matrix and Household Size Transition Across Lifecycle](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_demo_trans.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix//fs_mat_demo_trans.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_demo_trans.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/matrix/htmlpdfr/fs_mat_demo_trans.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/matrix/htmlpdfr/fs_mat_demo_trans.html)
	+ Matrix and household size transition across the lifecycle.
	+ Transposing and multiplying matrixes.
	+ Standard basis matrix and shift matrix.

### [Section 1.4 Variables in Dataframes][Variables in Dataframes] links

1. [Tibble Basics](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_basics.html)
	+ generate tibbles, rename tibble variables, tibble row and column names
	+ rename numeric sequential columns with string prefix and suffix
	+ **dplyr**: *as_tibble(mt) + rename_all(~c(ar_names)) + rename_at(vars(starts_with("xx")), funs(str_replace(., "yy", "yyyy")) + rename_at(vars(num_range('',ar_it)), funs(paste0(st,.))) + rowid_to_column() + row_number() + min_rank() + dense_rank()*
	+ **base**: *colnames + rownames*
2. [Label and Combine Factor Variables](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_factors.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_factors.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_factors.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_factors.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_factors.html)
	+ Convert numeric variables to factor variables, generate interaction variables (joint factors), and label factors with descriptive words.
	+ Graph MPG and 1/4 Miles Time (qsec) from the mtcars dataset over joint shift-type (am) and engine-type (vs) categories.
	+ **forcats**: *as_factor() + fct_recode() + fct_cross()*
3. [Randomly Draw Subsets of Rows from Matrix](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_random_draws.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_random_draws.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_random_draws.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_random_draws.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_random_draws.html)
	+ Given matrix, randomly sample rows, or select if random value is below threshold.
	+ **r**: *rnorm() + sample() + df[sample(dim(df)[1], it_M, replace=FALSE),]*
	+ **dplyr**: *case_when() + mutate(var = case_when(rnorm(n(),mean=0,sd=1) < 0 ~ 1, TRUE ~ 0)) %>% filter(var == 1)*
4. [Generate Variables Conditional on Other Variables](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_na.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_na.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_na.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html)
	+ Use case_when to generate elseif conditional variables: NA, approximate difference, etc.
	+ **dplyr**: *case_when() + na_if() + mutate(var = na_if(case_when(rnorm(n())< 0 ~ -99, TRUE ~ mpg), -99))*
	+ **r**: *e-notation + all.equal() + isTRUE(all.equal(a,b,tol)) + is.na() + NA_real_ + NA_character_ + NA_integer_*
5. [R Tibble Dataframe String Manipulations](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_string.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble//fs_tib_string.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_string.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/amto/tibble/htmlpdfr/fs_tib_string.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/amto/tibble/htmlpdfr/fs_tib_string.html)
	+ There are multiple CEV files, each containing the same file structure but simulated
	+ with different parameters, gather a subset of columns from different files, and provide
	+ with correct attributes based on CSV file names.
	+ **r**: *cbind(ls_st, ls_st) + as_tibble(mt_st)*

## Summarize Data links

### [Section 2.1 Counting Observation][Counting Observation] links

1. [Counting Basics](https://fanwangecon.github.io/R4Econ/summarize/count/htmlpdfr/fs_count_basics.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count//fs_count_basics.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/htmlpdfr/fs_count_basics.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/htmlpdfr/fs_count_basics.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/count/htmlpdfr/fs_count_basics.html)
	+ uncount to generate panel skeleton from years in survey
	+ **dplyr**: *uncount(yr_n) + group_by() + mutate(yr = row_number() + start_yr)*

### [Section 2.2 Sorting, Indexing, Slicing][Sorting, Indexing, Slicing] links

1. [Sorted Index, Interval Index and Expand Value from One Row](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_index_populate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index//fs_index_populate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_index_populate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_index_populate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_index_populate.html)
	+ Sort and generate index for rows
	+ Generate negative and positive index based on deviations
	+ Populate Values from one row to other rows
	+ **dplyr**: *arrange() + row_number() + mutate(lowest = min(Sepal.Length)) + case_when(row_number()==x ~ Septal.Length) + mutate(Sepal.New = Sepal.Length[Sepal.Index == 1])*
2. [R Within-group Ascending and Descending Sort, Selection, and Differencing](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_group_sort.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index//fs_group_sort.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_group_sort.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/index/htmlpdfr/fs_group_sort.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/index/htmlpdfr/fs_group_sort.html)
	+ Sort a dataframe by multiple variables, some in descending order.
	+ Select observations with the highest M values from within N groups (top scoring students from each class).
	+ **dplyr**: *arrange(a, b, desc(c)) + group_by() + lag() + lead() + slice_head(n=1)*

### [Section 2.3 Group Statistics][Group Statistics] links

1. [Cummean Test, Cumulative Mean within Group](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_cumall.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_cumall.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_cumall.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_cumall.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_cumall.html)
	+ There is a dataframe with a grouping variable and some statistics sorted by another within group
	+ variable, calculate the cumulative mean of that variable.
	+ **dplyr**: *cummean() + group_by(id, isna = is.na(val)) + mutate(val_cummean = ifelse(isna, NA, cummean(val)))*
2. [Count Unique Groups and Mean within Groups](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_unique_agg.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_unique_agg.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_unique_agg.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_unique_agg.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_unique_agg.html)
	+ Unique groups defined by multiple values and count obs within group.
	+ Mean, sd, observation count for non-NA within unique groups.
	+ **dplyr**: *group_by() + summarise(n()) + summarise_if(is.numeric, funs(mean = mean(., na.rm = TRUE), n = sum(is.na(.)==0)))*
3. [By Groups, One Variable All Statistics](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_one.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_summ_one.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_one.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_one.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_one.html)
	+ Pick stats, overall, and by multiple groups, stats as matrix or wide row with name=(ctsvar + catevar + catelabel).
	+ **tidyr**: *group_by() + summarize_at(, funs()) + rename(!!var := !!sym(var)) + mutate(!!var := paste0(var,'str',!!!syms(vars))) + gather() + unite() + spread(varcates, value)*
4. [By within Individual Groups Variables, Averages](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_wide.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate//fs_group_summ_wide.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_wide.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/aggregate/htmlpdfr/fs_group_summ_wide.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/aggregate/htmlpdfr/fs_group_summ_wide.html)
	+ By Multiple within Individual Groups Variables.
	+ Averages for all numeric variables within all groups of all group variables. Long to Wide to very Wide.
	+ **tidyr**: *gather() + group_by() + summarise_if(is.numeric, funs(mean(., na.rm = TRUE))) + mutate(all_m_cate = paste0(variable, '_c', value)) + unite() + spread()*

### [Section 2.4 Distributional Statistics][Distributional Statistics] links

1. [Tibble Basics](https://fanwangecon.github.io/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist//fst_hist_onevar.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/htmlpdfr/fst_hist_onevar.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/dist/htmlpdfr/fst_hist_onevar.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/dist/htmlpdfr/fst_hist_onevar.html)
	+ input multiple variables with comma separated text strings
	+ quantitative/continuous and categorical/discrete variables
	+ histogram and summary statistics
	+ **tibble**: *ar_one <- c(107.72,101.28) + ar_two <- c(101.72,101.28) + mt_data <- cbind(ar_one, ar_two) + as_tibble(mt_data)*

### [Section 2.5 Summarize Multiple Variables][Summarize Multiple Variables] links

1. [Apply the Same Function over Columns of Matrix](https://fanwangecon.github.io/R4Econ/summarize/multivar/htmlpdfr/fs_func_multivar.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar//fs_func_multivar.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar/htmlpdfr/fs_func_multivar.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/multivar/htmlpdfr/fs_func_multivar.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/summarize/multivar/htmlpdfr/fs_func_multivar.html)
	+ Replace NA values in selected columns by alternative values.
	+ Cumulative sum over multiple variables.
	+ Rename various various with common prefix and suffix appended.
	+ **r**: *cumsum() + gsub() + mutate_at(vars(contains('V')), .funs = list(cumu = ~cumsum(.))) + rename_at(vars(contains("V") ), list(~gsub("M", "", .)))*
	+ **dplyr**: *rename_at() + mutate_at() + rename_at(vars(starts_with("V")), funs(str_replace(., "V", "var"))) + mutate_at(vars(one_of(c('var1', 'var2'))), list(~replace_na(., 99)))*

## Functions links

### [Section 3.1 Dataframe Mutate][Dataframe Mutate] links

1. [Nonlinear Function of Scalars and Arrays over Rows](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_funceval.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef//fs_funceval.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_funceval.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_funceval.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_funceval.html)
	+ Five methods to evaluate scalar nonlinear function over matrix.
	+ Evaluate non-linear function with scalar from rows and arrays as constants.
	+ **r**: *.$fl_A + fl_A=$`(., 'fl_A') + .[[svr_fl_A]]*
	+ **dplyr**: *rowwise() + mutate(out = funct(inputs))*
2. [Evaluate Functions over Rows of Meshes Matrices](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_func_choice_states.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef//fs_func_choice_states.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_func_choice_states.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/mutatef/htmlpdfr/fs_func_choice_states.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/mutatef/htmlpdfr/fs_func_choice_states.html)
	+ Mesh states and choices together and rowwise evaluate many matrixes.
	+ Cumulative sum over multiple variables.
	+ Rename various various with common prefix and suffix appended.
	+ **r**: *ffi <- function(fl_A, ar_B)*
	+ **tidyr**: *expand_grid() + rowwise() + df %>% rowwise() %>% mutate(var = ffi(fl_A, ar_B))*
	+ **ggplot2**: *geom_line() + facet_wrap() + geom_hline() + facet_wrap(. ~ var_id, scales = 'free') + geom_hline(yintercept=0, linetype="dashed", color="red", size=1) +*

### [Section 3.2 Dataframe Do Anything][Dataframe Do Anything] links

1. [Dataframe Row to Array (Mx1 by N) to (MxQ by N+1)](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_expand.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof//fs_funceval_expand.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_expand.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_expand.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_expand.html)
	+ Generate row value specific arrays of varying Length, and stack expanded dataframe.
	+ Given row-specific information, generate row-specific arrays that expand matrix.
	+ **dplyr**: *do() + unnest() + left_join() + df %>% group_by(ID) %>% do(inc = rnorm(.$Q, mean=.$mean, sd=.$sd)) %>% unnest(c(inc))*
2. [Simulate country-specific wage draws and compute country wage GINIs: Dataframe (Mx1 by N) to (MxQ by N+1) to (Mx1 by N](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_group.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof//fs_funceval_group.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_group.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_group.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_group.html)
	+ Define attributes for M groups across N variables, simulate up to Q observations for each of the M Groups, then compute M-specific statistics based on the sample of observations within each M.
	+ Start with a matrix that is (Mx1 by N); Expand this to (MxQ by N+1), where, the additional column contains the MxQ specific variable; Compute statistics for each M based on the Q observations with M, and then present (Mx1 by N+1) dataframe.
	+ **dplyr**: *group_by(ID) + do(inc = rnorm(.$N, mean=.$mn, sd=.$sd)) + unnest(c(inc)) + left_join(df, by="ID")*
3. [Dataframe Subset to Dataframe (MxP by N) to (MxQ by N+Z-1)](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_groupwider.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof//fs_funceval_groupwider.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_groupwider.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/dof/htmlpdfr/fs_funceval_groupwider.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/dof/htmlpdfr/fs_funceval_groupwider.html)
	+ Group by mini dataframes as inputs for function. Stack output dataframes with group id.
	+ **dplyr**: *group_by() + do() + unnest()*

### [Section 3.3 Apply and pmap][Apply and pmap] links

1. [Apply and Sapply function over arrays and rows](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_apply.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop//fs_apply.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_apply.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_apply.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_apply.html)
	+ Evaluate function f(x_i,y_i,c), where c is a constant and x and y vary over each row of a matrix, with index i indicating rows.
	+ Get same results using apply and sapply with defined and anonymous functions.
	+ Convert list of list to table.
	+ **r**: *do.call() + as_tibble(do.call(rbind,ls)) + apply(mt, 1, func) + sapply(ls_ar, func, ar1, ar2)*
2. [Mutate rowwise, mutate pmap, and rowwise do unnest](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_applysapplymutate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop//fs_applysapplymutate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_applysapplymutate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/function/noloop/htmlpdfr/fs_applysapplymutate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/function/noloop/htmlpdfr/fs_applysapplymutate.html)
	+ Evaluate function f(x_i,y_i,c), where c is a constant and x and y vary over each row of a matrix, with index i indicating rows.
	+ Get same results using various types of mutate rowwise, mutate pmap and rowwise do unnest.
	+ **dplyr**: *rowwise() + do() + unnest()*
	+ **purrr**: *pmap(func)*
	+ **tidyr**: *unlist()*

## Multi-dimensional Data Structures links

### [Section 4.1 Generate, Gather, Bind and Join][Generate, Gather, Bind and Join] links

1. [R dplyr Group by Index and Generate Panel Data Structure](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_genpanel.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic//fs_genpanel.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_genpanel.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_genpanel.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_genpanel.html)
	+ Build skeleton panel frame with N observations and T periods with gender and height.
	+ Generate group Index based on a list of grouping variables.  
	+ **r**: *runif() + rnorm() + rbinom(n(), 1, 0.5) + cumsum()*
	+ **dplyr**: *group_by() + row_number() + ungroup() + one_of() + mutate(var = (row_number()==1)*1)*
	+ **tidyr**: *uncount()*
2. [R DPLYR Join Multiple Dataframes Together](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_joining.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic//fs_joining.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_joining.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_joining.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_joining.html)
	+ Join dataframes together with one or multiple keys. Stack dataframes together.
	+ **dplyr**: *filter() + rename(!!sym(vsta) := !!sym(vstb)) + mutate(var = rnom(n())) + left_join(df, by=(c('id'='id', 'vt'='vt'))) + left_join(df, by=setNames(c('id', 'vt'), c('id', 'vt'))) + bind_rows()*
3. [R Gather Data Columns from Multiple CSV Files](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_csv_gather.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic//fs_csv_gather.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_csv_gather.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/basic/htmlpdfr/fs_csv_gather.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/basic/htmlpdfr/fs_csv_gather.html)
	+ There are multiple CEV files, each containing the same file structure but simulated
	+ with different parameters, gather a subset of columns from different files, and provide
	+ with correct attributes based on CSV file names.
	+ Separate numeric and string components of a string variable value apart.
	+ **r**: *file() + writeLines() + readLines() + close() + gsub() + read.csv() + do.call(bind_rows, ls_df) + apply()*
	+ **tidyr**: *separate()*
	+ **regex**: *(?<=[A-Za-z])(?=[-0-9])*

### [Section 4.2 Wide and Long][Wide and Long] links

1. [Convert Table from Long to Wide with dplyr](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotwider.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong//fs_pivotwider.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotwider.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotwider.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotwider.html)
	+ Long attendance roster to wide roster and calculate cumulative attendance by each day for students.
	+ Convert long roster with attendance and test-scores to wide.
	+ **tidyr**: *pivot_wider(id_cols = c(v1), names_from = v2, names_prefix = "id", names_sep = "_", values_from = c(v3, v4))*
	+ **dplyr**: *mutate(var = case_when(rnorm(n()) < 0 ~ 1, TRUE ~ 0)) + rename_at(vars(num_range('', ar_it)), list(~paste0(st_prefix, . , ''))) + mutate_at(vars(contains(str)), list(~replace_na(., 0))) + mutate_at(vars(contains(str)), list(~cumsum(.)))*
2. [Convert Table from Wide to Long with dplyr](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotlonger.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong//fs_pivotlonger.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotlonger.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/widelong/htmlpdfr/fs_pivotlonger.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/widelong/htmlpdfr/fs_pivotlonger.html)
	+ Given a matrix of values with row and column labels, create a table where the unit of observation are the row and column categories, and the values in the matrix is stored in a single variable.
	+ **tidyr**: *pivot_longer(cols = starts_with('zi'), names_to = c('zi'), names_pattern = paste0("zi(.)"), values_to = "ev")*
	+ **dplyr**: *left_join()*

### [Section 4.3 Join and Compare][Join and Compare] links

1. [Find Closest Values Along Grids](https://fanwangecon.github.io/R4Econ/panel/join/htmlpdfr/fs_join_compare.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/join//fs_join_compare.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/join/htmlpdfr/fs_join_compare.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/panel/join/htmlpdfr/fs_join_compare.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/panel/join/htmlpdfr/fs_join_compare.html)
	+ There is an array (matrix) of values, find the index of the values closest to another value.
	+ **r**: *do.call(bind_rows, ls_df)*
	+ **dplyr**: *left_join(tb, by=(c('vr_a'='vr_a', 'vr_b'='vr_b')))*

## Linear Regression links

### [Section 5.1 Linear and Polynomial Fitting][Linear and Polynomial Fitting] links

1. [Find Best Fit of Curves Through Points](https://fanwangecon.github.io/R4Econ/linreg/polynomial/htmlpdfr/fs_lm_poly_fit.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial//fs_lm_poly_fit.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial/htmlpdfr/fs_lm_poly_fit.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial/htmlpdfr/fs_lm_poly_fit.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/polynomial/htmlpdfr/fs_lm_poly_fit.html)
	+ There are three x and y points, find the quadratic curve that fits through them exactly.
	+ There are N sets of x and y points, find the Mth order polynomial fit by regressing y on poly(x, M).
	+ **stats**: *lm(y ~ poly(x, 2), dataset=df) + summary.lm(rs) + predict(rs)*
2. [Fit a Time Series with Polynomial and Analytical Expressions for Coefficients](https://fanwangecon.github.io/R4Econ/linreg/polynomial/htmlpdfr/fs_poly_fit.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial//fs_poly_fit.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial/htmlpdfr/fs_poly_fit.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/polynomial/htmlpdfr/fs_poly_fit.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/polynomial/htmlpdfr/fs_poly_fit.html)
	+ Given a time series of data points from a polynomial data generating process, solve for the polynomial coefficients.
	+ Mth derivative of Mth order polynomial is time invariant, use functions of differences of differences of differences to identify polynomial coefficients analytically.
	+ **R**: *matrix multiplication*

### [Section 5.2 OLS and IV][OLS and IV] links

1. [IV/OLS Regression](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg//fs_lin_ivregrow.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivregrow.html)
	+ R Instrumental Variables and Ordinary Least Square Regression store all Coefficients and Diagnostics as Dataframe Row.
	+ **aer**: *library(aer) + ivreg(as.formula, diagnostics = TRUE) *
2. [M Outcomes and N RHS Alternatives](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivloop.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg//fs_lin_ivloop.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivloop.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/ivreg/htmlpdfr/fs_lin_ivloop.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/ivreg/htmlpdfr/fs_lin_ivloop.html)
	+ There are M outcome variables and N alternative explanatory variables. Regress all M outcome variables on N endogenous/independent right hand side variables one by one, with controls and/or IVs, collect coefficients.
	+ **dplyr**: *bind_rows(lapply(listx, function(x)(bind_rows(lapply(listy, regf.iv))) + starts_with() + ends_with() + reduce(full_join)*

### [Section 5.3 Decomposition][Decomposition] links

1. [Regression Decomposition](https://fanwangecon.github.io/R4Econ/linreg/decompose/htmlpdfr/fs_lin_decompose.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose//fs_lin_decompose.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/htmlpdfr/fs_lin_decompose.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/linreg/decompose/htmlpdfr/fs_lin_decompose.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/linreg/decompose/htmlpdfr/fs_lin_decompose.html)
	+ Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
	+ **dplyr**: *gather() + group_by(var) + mutate_at(vars, funs(mean = mean(.))) + rowSums(mat*mat) + mutate_if(is.numeric, funs(frac = (./value_var)))*

## Nonlinear and Other Regressions links

### [Section 6.1 Logit Regression][Logit Regression] links

1. [Logit Regression](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_births.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit//fs_logit_births.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_births.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_births.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_births.html)
	+ Logit regression testing and prediction.
	+ **stats**: *glm(as.formula(), data, family='binomial') + predict(rs, newdata, type = "response")*
2. [Estimate Logistic Choice Model with Aggregate Shares](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_aggregate_shares.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit//fs_logit_aggregate_shares.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_aggregate_shares.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_aggregate_shares.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_aggregate_shares.html)
	+ Aggregate share logistic OLS with K worker types, T time periods and M occupations.
	+ Estimate logistic choice model with aggregate shares, allowing for occupation-specific wages and occupation-specific intercepts.
	+ Estimate allowing for K and M specific intercepts, K and M specific coefficients, and homogeneous coefficients.
	+ Create input matrix data structures for logistic aggregate share estimation.
	+ **stats**: *lm(y ~ . -1)*
3. [Fit Prices Given Quantities Logistic Choice with Aggregate Data](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_aggregate_share_to_price.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit//fs_logit_aggregate_share_to_price.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_aggregate_share_to_price.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/logit/htmlpdfr/fs_logit_aggregate_share_to_price.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/logit/htmlpdfr/fs_logit_aggregate_share_to_price.html)
	+ A multinomial logistic choice problem generates choice probabilities across alternatives, find the prices that explain aggregate shares. 
	+ **stats**: *lm(y ~ . -1)*

### [Section 6.2 Quantile Regression][Quantile Regression] links

1. [Quantile Regressions with Quantreg](https://fanwangecon.github.io/R4Econ/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/quantreg//fs_quantreg_intro.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.html)
	+ Quantile regression with continuous outcomes. Estimates and tests quantile coefficients.
	+ **quantreg**: *rq(mpg ~ disp + hp + factor(am), tau = c(0.25, 0.50, 0.75), data = mtcars) + anova(rq(), test = "Wald", joint=TRUE) + anova(rq(), test = "Wald", joint=FALSE)*

## Optimization links

### [Section 7.1 Grid Based Optimization][Grid Based Optimization] links

1. [Find the Maximizing or Minimizing Point Given Some Objective Function](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_optimize_grid_loop.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect//fs_optimize_grid_loop.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_optimize_grid_loop.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_optimize_grid_loop.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_optimize_grid_loop.html)
	+ Find the maximizing or minimizing point given some objective function.
	+ **base**: *while + min + which.min + sapply*
2. [Concurrent Bisection over Dataframe Rows](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_bisec_joint.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect//fs_bisec_joint.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_bisec_joint.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/optimization/root_bisect/htmlpdfr/fs_bisec_joint.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/optimization/root_bisect/htmlpdfr/fs_bisec_joint.html)
	+ Post multiple regressions, fraction of outcome variables' variances explained by multiple subsets of right hand side variables.
	+ **tidyr**: *pivot_longer(cols = starts_with('abc'), names_to = c('a', 'b'), names_pattern = paste0('prefix', "(.)_(.)"), values_to = val) + pivot_wider(names_from = !!sym(name), values_from = val) + mutate(!!sym(abc) := case_when(efg < 0 ~ !!sym(opq), TRUE ~ iso))*
	+ **gglot2**: *geom_line() + facet_wrap() + geom_hline()*

## Mathematics links

### [Section 8.1 Basics][Basics] links

1. [Analytical Formula Fit Curves Through Points](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_analytical_poly_fit.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_analytical_poly_fit.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_analytical_poly_fit.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_analytical_poly_fit.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_analytical_poly_fit.html)
	+ There are three pairs of points, formulas for the exact quadratic curve that fits through the points.
	+ There are three pairs of points, we observe only differences in y values, formulas for the linear and quadratic parameters. 
	+ There are three pairs of points, formulas for the linear best fit line through the points.
	+ **stats**: *lm(y ~ x + I(x^2), dataset=df) + lm(y ~ poly(x, 2), dataset=df) + summary.lm(rs) + predict(rs)*
2. [Quadratic and Ratio Rescaling of Parameters with Fixed Min and Max](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_rescale.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_rescale.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_rescale.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_rescale.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_rescale.html)
	+ For 0<theta<1, generate 0 < thetaHat(theta, lambda) < 1, where lambda is between positive and negative infinity, used to rescale theta.
	+ Fit a quadratic function for three points, where the starting and ending points are along the 45 degree line.   
	+ **r**: *sort(unique()) + sapply(ar, func, param=val)*
	+ **ggplot2**: *geom_line() + geom_vline() + labs(title, subtitle, x, y, caption) + scale_y_continuous(breaks, limits)*
3. [Rescaling Bounded Parameter to be Unbounded and Positive and Negative Exponents with Different Bases](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_exponents.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_exponents.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_exponents.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_exponents.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_exponents.html)
	+ Log of alternative bases, bases that are not e, 10 or 2. 
	+ A parameter is constrained between 1 and negative infinity, use exponentials of different bases to scale the bounded parameter to an unbounded parameter.    
	+ Positive exponentials are strictly increasing. Negative exponentials are strictly decreasing.
	+ A positive number below 1 to a negative exponents is above 1, and a positive number above 1 to a negative exponents is below 1.
	+ **graphics**: *plot(x, y) + title() + legend()*
4. [Find the Closest Point Along a Line to Another Point](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_point2line.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_point2line.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_point2line.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_point2line.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_point2line.html)
	+ A line crosses through the origin, what is the closest point along this line to another point.
	+ Graph several functions jointly with points and axis.
	+ **graphics**: *par(mfrow = c(1, 1)) + curve(fc) + points(x, y) + abline(v=0, h=0)*
5. [linear solve x with f(x) = 0](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_solu_x_lin.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions//fs_solu_x_lin.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_solu_x_lin.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/solutions/htmlpdfr/fs_solu_x_lin.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/solutions/htmlpdfr/fs_solu_x_lin.html)
	+ Evaluate and solve statistically relevant problems with one equation and one unknown that permit analytical solutions.

### [Section 8.2 Production Functions][Production Functions] links

1. [Nested Constant Elasticity of Substitution Production Function](https://fanwangecon.github.io/R4Econ/math/func_prod/htmlpdfr/fs_nested_CES.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod//fs_nested_CES.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod/htmlpdfr/fs_nested_CES.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod/htmlpdfr/fs_nested_CES.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_prod/htmlpdfr/fs_nested_CES.html)
	+ A nested-CES production function with nest-specific elasticities.
	+ Re-state the nested-CES problem as several sub-problems.
	+ Marginal products and its relationship to prices in expenditure minimization.
2. [Latent Dynamic Health Production Function](https://fanwangecon.github.io/R4Econ/math/func_prod/htmlpdfr/fs_latent_health.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod//fs_latent_health.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod/htmlpdfr/fs_latent_health.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_prod/htmlpdfr/fs_latent_health.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_prod/htmlpdfr/fs_latent_health.html)
	+ A model of latent health given lagged latent health and health inputs.
	+ Find individual-specific production function coefficient given self-rated discrete health status probabilities.
	+ Persistence of latent health status given observed discrete current and lagged outcomes.

### [Section 8.3 Inequality Models][Inequality Models] links

1. [GINI for Discrete Samples or Discrete Random Variable](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_gini_disc.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_gini_disc.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_gini_disc.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html)
	+ Given sample of data points that are discrete, compute the approximate GINI coefficient.
	+ Given a discrete random variable, compute the GINI coefficient.
	+ **r**: *sort() + cumsum() + sum()*
2. [CES and Atkinson Inequality Index](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_atkinson_ces.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_atkinson_ces.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_atkinson_ces.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_atkinson_ces.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_atkinson_ces.html)
	+ Analyze how changing individual outcomes shift utility given inequality preference parameters.
	+ Discrete a continuous normal random variable with a binomial discrete random variable.
	+ Draw Cobb-Douglas, Utilitarian and Leontief indifference curve.
	+ **r**: *apply(mt, 1, funct(x){}) + do.call(rbind, ls_mt)*
	+ **tidyr**: *expand_grid()*
	+ **ggplot2**: *geom_line() + facet_wrap()*
	+ **econ**: *Atkinson (JET, 1970)*
3. [Within and Across Group Variations in Ambient Climate Exposures Across Socio-Demographic Groups](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_pop_loc_pollution.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_pop_loc_pollution.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_pop_loc_pollution.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_pop_loc_pollution.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_pop_loc_pollution.html)
	+ Simulate pollution exposures by location.
	+ Compute share of pollution burden for a population group relative to the share of overall population accounted for by this population group.
	+ Compute gini, atkinson, and coefficient variations over group means.
	+ Compute within group variations and across group variations, using percentile ratios. 
	+ **r**: *matrix()*
	+ **stats**: *runif()*
	+ **dplyr**: *group_by() + left_join() + filter() + slice()*
4. [Environmental Exposures, Population across Locations and Time](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_pop_loc_pollution_time.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq//fs_pop_loc_pollution_time.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_pop_loc_pollution_time.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/math/func_ineq/htmlpdfr/fs_pop_loc_pollution_time.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_pop_loc_pollution_time.html)
	+ Discussions on environmental exposures along locational-paths and distribution of population across locations and socio-demographic groups.
	+ Discussions on moments of within-person locational-path-specific distributions environmental exposures. 
	+ Simulate environmental exposures across locations and time. 
	+ Compute moments of within-person environmental exposures. 
	+ **r**: *matrix() + sample()*
	+ **stats**: *rlnorm()*

## Statistics links

### [Section 9.1 Random Draws][Random Draws] links

1. [Randomly Perturb Some Parameter Value with Varying Magnitudes](https://fanwangecon.github.io/R4Econ/statistics/random/htmlpdfr/fs_perturb_parameter.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/random//fs_perturb_parameter.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/random/htmlpdfr/fs_perturb_parameter.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/random/htmlpdfr/fs_perturb_parameter.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/random/htmlpdfr/fs_perturb_parameter.html)
	+ Given some existing parameter value, with an intensity value between 0 and 1, decide how to perturb the value.
	+ **r**: *matrix*
	+ **stats**: *qlnorm()*
	+ **graphics**: *par() + hist() + abline()*

### [Section 9.2 Distributions][Distributions] links

1. [Integrate Normal Shocks](https://fanwangecon.github.io/R4Econ/statistics/integration/htmlpdfr/fs_integrate_normal.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/integration//fs_integrate_normal.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/integration/htmlpdfr/fs_integrate_normal.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/integration/htmlpdfr/fs_integrate_normal.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/integration/htmlpdfr/fs_integrate_normal.html)
	+ Random Sampling (Monte Carlo) integrate shocks.
	+ Trapezoidal rule (symmetric rectangles) integrate normal shock.

### [Section 9.3 Discrete Random Variable][Discrete Random Variable] links

1. [Binomial Approximation of Normal](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_disc_approx_cts.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar//fs_disc_approx_cts.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_disc_approx_cts.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_disc_approx_cts.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_disc_approx_cts.html)
	+ Approximate a continuous normal random variable with a discrete binomial random variable.
	+ **r**: *hist() + plot()*
	+ **stats**: *dbinom() + rnorm()*
2. [Estimate Parameters for Discrete Distributions](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_disc_fit.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar//fs_disc_fit.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_disc_fit.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_disc_fit.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_disc_fit.html)
	+ Given a discrete random variable, estimate the parameters of a discrete probability function.
	+ Given a discrete random variable, generated from discretizing a normal random variable, estimate binomial parameters that provide the best fit for the observe data.
	+ **r**: *%*% + which.max*
	+ **stats**: *dbinom()*
	+ **dplyr**: *min_rank()*
3. [Gestation (Binomial), Conception (Mixture), and Temperature (Sine wave and AR(1))](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_birth_shock.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar//fs_birth_shock.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_birth_shock.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_birth_shock.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_birth_shock.html)
	+ Simulate the distribution of gestational periods at birth following a binomial distribution.
	+ Simulate the distribution of conception time following a potentially bimodal distribution.
	+ Compute which births are pre-term given a simulated dataset of conception and birth dates.
	+ Simulate temperature over days across years using a sine wave combined with a first order markov process with normal shocks.
	+ **stats**: *dbinom() + pbinom() + rnorm() + runif() + lm(binary ~ continuous + factor(dates))*
	+ **ggplot**: *geom_point() + geom_bar() + geom_line() + geom_density() + geom_vline()*
4. [Obtaining Joint Distribution from Marginal with Rectilinear Restrictions](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_discrandvar_marg2joint.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar//fs_discrandvar_marg2joint.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_discrandvar_marg2joint.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_discrandvar_marg2joint.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_discrandvar_marg2joint.html)
	+ Solve for joint distributional mass given marginal distributional mass given rectilinear assumptions.
	+ **r**: *qr()*
5. [Obtaining Joint Distribution from Conditional with Rectilinear Restrictions](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_discrandvar_condi2joint.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar//fs_discrandvar_condi2joint.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_discrandvar_condi2joint.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/statistics/discrandvar/htmlpdfr/fs_discrandvar_condi2joint.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/statistics/discrandvar/htmlpdfr/fs_discrandvar_condi2joint.html)
	+ Solve for joint distributional mass given conditional distributional mass given rectilinear assumptions.
	+ **r**: *qr() + solve() + matrix()*

## Tables and Graphs links

### [Section 10.1 R Base Plots][R Base Plots] links

1. [R Base Plot Line with Curves and Scatter](https://fanwangecon.github.io/R4Econ/tabgraph/baseplot/htmlpdfr/fs_base_curve.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/baseplot//fs_base_curve.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/baseplot/htmlpdfr/fs_base_curve.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/baseplot/htmlpdfr/fs_base_curve.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/tabgraph/baseplot/htmlpdfr/fs_base_curve.html)
	+ Plot scatter points, line plot and functional curve graphs together.
	+ Set margins for legend to be outside of graph area, change line, point, label and legend sizes.
	+ Generate additional lines for plots successively, record successively, and plot all steps, or initial steps results.
	+ **r**: *plot() + curve() + legend() + title() + axis() + par() + recordPlot()*

### [Section 10.2 ggplot Line Related Plots][ggplot Line Related Plots] links

1. [ggplot Line Plot Multiple Categorical Variables With Continuous Variable](https://fanwangecon.github.io/R4Econ/tabgraph/ggline/htmlpdfr/fs_ggline_mgrp_ncts.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggline//fs_ggline_mgrp_ncts.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggline/htmlpdfr/fs_ggline_mgrp_ncts.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggline/htmlpdfr/fs_ggline_mgrp_ncts.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/tabgraph/ggline/htmlpdfr/fs_ggline_mgrp_ncts.html)
	+ One category is subplot, one category is line-color, one category is line-type.
	+ One category is subplot, one category is differentiated by line-color, line-type and scatter-shapes.
	+ One category are separate plots, two categories are subplots rows and columns, one category is differentiated by line-color, line-type and scatter-shapes.
	+ **ggplot**: *ggplot() + facet_wrap() + facet_grid() + geom_line()  + geom_point()       + geom_smooth() + geom_hline() + scale_colour_manual() + scale_shape_manual() + scale_shape_discrete() + scale_linetype_manual() + scale_x_continuous() + scale_y_continuous() + theme_bw() + theme() + guides() + theme() + ggsave()*
	+ **dplyr**: *filter(vara %in% c(1, 2) & varb == "val") + mutate_if() + !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)   *

### [Section 10.3 ggplot Scatter Related Plots][ggplot Scatter Related Plots] links

1. [ggplot Scatter Plot Grouped or Unique Patterns and Colors](https://fanwangecon.github.io/R4Econ/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_3cts_mdisc.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter//fs_ggscatter_3cts_mdisc.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_3cts_mdisc.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_3cts_mdisc.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_3cts_mdisc.html)
	+ Scatter Plot Three Continuous Variables and Multiple Categorical Variables
	+ Two continuous variables for the x-axis and the y-axis, another continuous variable for size of scatter, other categorical variables for scatter shape and size.
	+ Scatter plot with unique pattern and color for each scatter point. 
	+ Y and X label axis with two layers of text in levels and deviation from some mid-point values. 
	+ **tibble**: *rownames_to_column()*
	+ **ggplot**: *ggplot() + geom_jitter() + geom_smooth() + geom_point(size=1, stroke=1) + scale_colour_manual() + scale_shape_discrete() + scale_linetype_manual() + scale_x_continuous() + scale_y_continuous() + theme_bw() + theme()*
2. [ggplot Multiple Scatter-Lines and Facet Wrap Over Categories](https://fanwangecon.github.io/R4Econ/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_facet_wrap.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter//fs_ggscatter_facet_wrap.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_facet_wrap.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_facet_wrap.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/tabgraph/ggscatter/htmlpdfr/fs_ggscatter_facet_wrap.html)
	+ ggplot multiple lines with scatter as points and connecting lines.
	+ Facet wrap to generate subfigures for sub-categories.
	+ Generate separate plots from data saved separately.
	+ **r**: *apply*
	+ **ggplot**: *facet_wrap() + geom_smooth() + geom_point() + facet_wrap() + scale_colour_manual() + scale_shape_manual() + scale_linetype_manual()*

### [Section 10.4 Write and Read Plots][Write and Read Plots] links

1. [Base R Save Images At Different Sizes](https://fanwangecon.github.io/R4Econ/tabgraph/inout/htmlpdfr/fs_img_io.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/inout//fs_img_io.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/inout/htmlpdfr/fs_img_io.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/tabgraph/inout/htmlpdfr/fs_img_io.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/tabgraph/inout/htmlpdfr/fs_img_io.html)
	+ Base R store image core, add legends/titles/labels/axis of different sizes to save figures of different sizes.
	+ **r**: *png() + setEPS() + postscript() + dev.off()*

## Get Data links

### [Section 11.1 Environmental Data][Environmental Data] links

1. [CDS ECMWF Global Enviornmental Data Download](https://fanwangecon.github.io/R4Econ/getdata/envir/htmlpdfr/fs_ecmwf.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/getdata/envir//fs_ecmwf.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/getdata/envir/htmlpdfr/fs_ecmwf.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/getdata/envir/htmlpdfr/fs_ecmwf.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/getdata/envir/htmlpdfr/fs_ecmwf.html)
	+ Using Python API get get ECMWF ERA5 data.
	+ Dynamically modify a python API file, run python inside a Conda virtual environment with R-reticulate.
	+ **r**: *file() + writeLines() + unzip() + list.files() + unlink()*
	+ **r-reticulate**: *use_python() + Sys.setenv(RETICULATE_PYTHON = spth_conda_env)*

## Code and Development links

### [Section 12.1 Installation][Installation] links

1. [R, RTools, Rstudio Installation and Update with VSCode](https://fanwangecon.github.io/R4Econ/development/install/htmlpdfr/fs_install_R.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/install//fs_install_R.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/install/htmlpdfr/fs_install_R.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/install/htmlpdfr/fs_install_R.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/install/htmlpdfr/fs_install_R.html)
	+ Install and update R, RTools, and Rstudio.
	+ Set-up R inside VSCode.
	+ **installr**: *updateR()*

### [Section 12.2 Files In and Out][Files In and Out] links

1. [Decompose File Paths to Get Folder and Files Names](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_path.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout//fs_path.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_path.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_path.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_path.html)
	+ Decompose file path and get file path folder names and file name.
	+ Check if file name exists.
	+ **r**: *.Platform$file.sep + tail() + strsplit() + basename() + dirname() + substring() + dir.exists() + file.exists()*
2. [Save Text to File, Read Text from File, Replace Text in File](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_text_save.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout//fs_text_save.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_text_save.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_text_save.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_text_save.html)
	+ Save data to file, read text from file, replace text in file.
	+ **r**: *kable() + file() + writeLines() + readLines() + close() + gsub()*
3. [Convert R Markdown File to R, PDF and HTML](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_rmd_pdf_html.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout//fs_rmd_pdf_html.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_rmd_pdf_html.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/inout/htmlpdfr/fs_rmd_pdf_html.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/inout/htmlpdfr/fs_rmd_pdf_html.html)
	+ Find all files in a folder with a particula suffix, with exclusion.
	+ Convert R Markdow File to R, PDF and HTML.
	+ Modify markdown pounds hierarchy.
	+ **r**: *file() + writeLines() + readLines() + close() + gsub()*

### [Section 12.3 Python with R][Python with R] links

1. [Python in R with Reticulate](https://fanwangecon.github.io/R4Econ/development/python/htmlpdfr/fs_python_reticulate.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/python//fs_python_reticulate.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/python/htmlpdfr/fs_python_reticulate.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/python/htmlpdfr/fs_python_reticulate.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/python/htmlpdfr/fs_python_reticulate.html)
	+ Use Python in R with Reticulate
	+ **reticulate**: *py_config() + use_condaenv() + py_run_string() + Sys.which('python')*

### [Section 12.4 Command Line][Command Line] links

1. [System and Shell Commands in R](https://fanwangecon.github.io/R4Econ/development/system/htmlpdfr/fs_system_shell.html): [**rmd**](https://github.com/FanWangEcon/R4Econ/blob/master/development/system//fs_system_shell.Rmd) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/development/system/htmlpdfr/fs_system_shell.R) \| [**pdf**](https://github.com/FanWangEcon/R4Econ/blob/master/development/system/htmlpdfr/fs_system_shell.pdf) \| [**html**](https://fanwangecon.github.io/R4Econ/development/system/htmlpdfr/fs_system_shell.html)
	+ Run system executable and shell commands.
	+ Activate conda environment with shell script.
	+ **r**: *system() + shell()*
