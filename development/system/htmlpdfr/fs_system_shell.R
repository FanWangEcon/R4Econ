## ----global_options, include = FALSE-----------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------
# detect current path
print(toString(shell(paste0("echo %cd%"), intern=TRUE)))
# Show directory
print(toString(shell(paste0("dir"), intern=TRUE)))


## ----------------------------------------------------------------------------------------------------------------
# activate conda env
print(toString(shell(paste0("activate base & python --version"), intern=TRUE)))


## ----------------------------------------------------------------------------------------------------------------
spg_runpython <- paste0("activate base &",
                        "python --version &",
                        "python -c ",
                        "\"st_var='this is string var';",
                        "print(f'{st_var}');",
                        "\"")
print(toString(shell(spg_runpython, intern=TRUE)))

