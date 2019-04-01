# Generate logs of all variables satisfing some condition, new log variables end with _log suffix
df <- df %>% mutate_at(vars(matches('prot|cal'), -contains('p.A.')), funs(log = log(.)))
