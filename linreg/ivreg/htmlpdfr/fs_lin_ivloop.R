## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ff_reg_mbyn <- function(list.vars.y, list.vars.x,
                        vars.c, vars.z, df,
                        return_all = FALSE,
                        stats_ends = 'value', time = FALSE) {
  
  # regf.iv() function is from C:\Users\fan\R4Econ\linreg\ivreg\ivregdfrow.R
  if (time) {
    start_time <- Sys.time()
  }
  
  if (return_all) {
    df.reg.out.all <- 
      bind_rows(lapply(list.vars.x,
                       function(x) (
                         bind_rows(
                           lapply(list.vars.y, regf.iv, 
                                  vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df))
                       )))
    
  } else {
    df.reg.out.all <- 
      (lapply(list.vars.x,
              function(x) (
                bind_rows(
                  lapply(list.vars.y, regf.iv, 
                         vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df)) %>%
                  select(vars_var.y, starts_with(x)) %>%
                  select(vars_var.y, ends_with(stats_ends))
              ))) %>% reduce(full_join)
  }
  
  if (time) {
    end_time <- Sys.time()
    print(paste0('Estimation for all ys and xs took (seconds):', 
                 end_time - start_time))
  }
  
  return(df.reg.out.all)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Library
library(tidyverse)
library(AER)

# Load Sample Data
setwd('C:/Users/fan/R4Econ/_data/')
df <- read_csv('height_weight.csv')

# Source Dependency
source('C:/Users/fan/R4Econ/linreg/ivreg/ivregdfrow.R')

# Setting
options(repr.matrix.max.rows=50, repr.matrix.max.cols=50)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
var.y1 <- c('hgt')
var.y2 <- c('wgt')
var.y3 <- c('vil.id')
list.vars.y <- c(var.y1, var.y2, var.y3)

var.x1 <- c('prot')
var.x2 <- c('cal')
var.x3 <- c('wealthIdx')
var.x4 <- c('p.A.prot')
var.x5 <- c('p.A.nProt')
list.vars.x <- c(var.x1, var.x2, var.x3, var.x4, var.x5)

vars.z <- c('indi.id')
vars.c <- c('sex', 'wgt0', 'hgt0', 'svymthRound')


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- NULL
suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
              vars.c, vars.z, df,
              return_all = FALSE,
              stats_ends = 'value'))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- c('indi.id')
suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
              vars.c, vars.z, df,
              return_all = FALSE,
              stats_ends = 'value'))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- NULL
suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
              vars.c, vars.z, df,
              return_all = FALSE,
              stats_ends = 'Estimate'))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- c('indi.id')
suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
              vars.c, vars.z, df,
              return_all = FALSE,
              stats_ends = 'Estimate'))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- NULL
t(suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
            vars.c, vars.z, df,
            return_all = TRUE,
            stats_ends = 'Estimate')))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- c('indi.id')
t(suppressWarnings(suppressMessages(
  ff_reg_mbyn(list.vars.y, list.vars.x,
            vars.c, vars.z, df,
            return_all = TRUE,
            stats_ends = 'Estimate')))) %>%
  kable() %>%
  kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
vars.z <- c('indi.id')
vars.z <- NULL
vars.c <- c('sex', 'wgt0', 'hgt0', 'svymthRound')


## --------------------------------------------------------------------------------------------------------------------------------------------------------
df.reg.out <- as_tibble(
  bind_rows(lapply(list.vars.y, regf.iv, 
                   vars.x=var.x1, vars.c=vars.c, vars.z=vars.z, df=df)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
lapply(list.vars.y, function(y) (mean(df[[var.x1]], na.rm=TRUE) + 
                                   mean(df[[y]], na.rm=TRUE)))
lapplytwice <- lapply(
  list.vars.x, function(x) (
    lapply(list.vars.y, function(y) (mean(df[[x]], na.rm=TRUE) + 
                                       mean(df[[y]], na.rm=TRUE)))))
# lapplytwice


## --------------------------------------------------------------------------------------------------------------------------------------------------------
df.reg.out.all <- bind_rows(
  lapply(list.vars.x,
         function(x) (
           bind_rows(
             lapply(list.vars.y, regf.iv, 
                    vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df))
         )))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# df.reg.out.all %>%
#   kable() %>%
#   kable_styling_fc_wide()


## --------------------------------------------------------------------------------------------------------------------------------------------------------
df.reg.out.all <- 
  (lapply(list.vars.x,
          function(x) (
            bind_rows(lapply(list.vars.y, regf.iv, 
                             vars.x=x, vars.c=vars.c, vars.z=vars.z, df=df)) %>%
              select(vars_var.y, starts_with(x)) %>%
              select(vars_var.y, ends_with('value'))
          ))) %>% reduce(full_join)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
df.reg.out.all %>%
  kable() %>%
  kable_styling_fc_wide()

