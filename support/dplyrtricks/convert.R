# dplyr convert set of variablea all to factor
mutate_at(vars(vars.group), funs(as.factor(.)))
