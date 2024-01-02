## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
print(rlang::search_envs())


## -------------------------------------------------------------------------------------
tidyverse_conflicts()


## ---- eval=FALSE----------------------------------------------------------------------
## library(stats)
## library(dplyr)
## as_tibble(mtcars, rownames = "car") %>% filter(car == "Valiant")
## 
## # Error message
## # > as_tibble(mtcars, rownames = "car") %>% filter(car == "Valiant")
## # Error: object 'car' not found


## -------------------------------------------------------------------------------------
library(stats)
library(dplyr)
print(as_tibble(mtcars, rownames = "car") %>% dplyr::filter(car == "Valiant"))


## ---- eval = FALSE--------------------------------------------------------------------
## library(conflicted)
## conflict_prefer("filter", "dplyr", "stats")
## library(stats)
## library(dplyr)
## print(as_tibble(mtcars, rownames = "car") %>% filter(car == "Valiant"))
## 
## # > conflict_prefer("filter", "dplyr", "stats")
## # [conflicted] Will prefer dplyr::filter over stats::filter.
## # > library(stats)
## # > library(dplyr)
## # > print(as_tibble(mtcars, rownames = "car") %>% filter(car == "Valiant"))
## # # A tibble: 1 × 12
## #   car       mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
## #   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## # 1 Valiant  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1

