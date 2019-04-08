# Generate logs of all variables satisfing some condition, new log variables end with _log suffix
df <- df %>% mutate_at(vars(matches('prot|cal'), -contains('p.A.')), funs(log = log(.)))

# Subtract from all variables some value, add suffix to name, then rename
mutate_at(vars(starts_with('hgtp.')), funs(hgtdiff = . - !!sym(var.hgt))) %>% 
  rename_at(vars(ends_with("hgtdiff")), funs(gsub("hgtp.", "hgtpd.", gsub("_hgtdiff", "", .))))
