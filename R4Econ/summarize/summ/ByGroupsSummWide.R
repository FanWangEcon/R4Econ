# Data Function
# https://fanwangecon.github.io/R4Econ/summarize/summ/ByGroupsSummWide.html
f.by.groups.summ.wide <- function(df.groups.to.average,
                                  vars.not.groups2avg,
                                  vars.indi.grp = c('S.country','ID'),
                                  display=TRUE) {

# 1. generate categoricals for full year (m.12), half year (m.6), quarter year (m.4)
# 2. generate categoricals also for uneven years (m12t14) using stagger (+2 rather than -1)
# 3. reshape wide to long, so that all categorical date groups appear in var=value, and categories in var=variable
# 4. calculate mean for all numeric variables for all date groups
# 5. combine date categorical variable and value, single var: m.12.c1= first year average from m.12 averaging

#######################################
# Step 1
#######################################
# 1. generate categoricals for full year (m.12), half year (m.6), quarter year (m.4)
# 2. generate categoricals also for uneven years (m12t14) using stagger (+2 rather than -1)
# df.groups.to.average<- raw.selected.df %>%
#         filter(!!sym(mth.var) >= 0 & !!sym(mth.var) <= 24)  %>%
#         mutate(m12t24=(floor((!!sym(mth.var) - 12) %/% 14) + 1),
#                m8t24=(floor((!!sym(mth.var) - 8) %/% 18) + 1),
#                m12 = pmax((floor((!!sym(mth.var)-1) %/% 12) + 1), 1),
#                m6 = pmax((floor((!!sym(mth.var)-1) %/% 6) + 1), 1),
#                m3 = pmax((floor((!!sym(mth.var)-1) %/% 3) + 1), 1))

#######################################
# S2: reshape wide to long, so that all categorical date groups appear in var=value, and categories in var=variable; calculate mean for all numeric variables for all date groups
#######################################
df.avg.long <- df.groups.to.average %>%
       gather(variable, value, -one_of(c(vars.indi.grp,
                                         vars.not.groups2avg))) %>%
       group_by(!!!syms(vars.indi.grp), variable, value) %>%
       summarise_if(is.numeric, funs(mean(., na.rm = TRUE)))

if (display){
  dim(df.avg.long)
  options(repr.matrix.max.rows=10, repr.matrix.max.cols=20)
  print(df.avg.long)
}

#######################################
# S3 combine date categorical variable and value, single var: m.12.c1= first year average from m.12 averaging; to do this make data even longer first
#######################################

# We already have the averages, but we want them to show up as variables, mean for each group of each variable.
# drop_na() %>%
df.avg.allvars.wide <- df.avg.long %>%
   ungroup() %>%
   mutate(all_m_cate = paste0(variable, '_c', value)) %>%
   select(all_m_cate, everything(), -variable, -value) %>%
   gather(variable, value, -one_of(vars.indi.grp), -all_m_cate) %>%
   unite('var_mcate', variable, all_m_cate) %>%
   spread(var_mcate, value)

if (display){
  dim(df.avg.allvars.wide)
  options(repr.matrix.max.rows=10, repr.matrix.max.cols=10)
  print(df.avg.allvars.wide)
}

return(df.avg.allvars.wide)
}
