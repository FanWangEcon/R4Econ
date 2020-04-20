## ----global_options, include = FALSE-----------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------
# IV regression function
# The code below uses the AER library's regresison function
# All results are stored in a single row as data_frame
# This functoin could work with dplyr do
# var.y is single outcome, vars.x, vars.c and vars.z are vectors of endogenous variables, controls and instruments.
regf.iv <- function(var.y, vars.x, 
                    vars.c, vars.z, df, transpose=TRUE) {
  
  # A. Set-Up Equation
  str.vars.x <- paste(vars.x, collapse='+')
  str.vars.c <- paste(vars.c, collapse='+')
  
  df <- df %>% 
    select(one_of(var.y, vars.x, vars.c, vars.z)) %>% 
    drop_na() %>% filter_all(all_vars(!is.infinite(.)))
  
  if (length(vars.z) >= 1) {
    #     library(AER)
    str.vars.z <- paste(vars.z, collapse='+')
    equa.iv <- paste(var.y,
                     paste(paste(str.vars.x, str.vars.c, sep='+'),
                           paste(str.vars.z, str.vars.c, sep='+'),
                           sep='|'),
                     sep='~')
    #     print(equa.iv)
    
    # B. IV Regression
    ivreg.summ <- summary(ivreg(as.formula(equa.iv), data=df),
                          vcov = sandwich, df = Inf, diagnostics = TRUE)
    
    # C. Statistics from IV Regression
    #     ivreg.summ$coef
    #     ivreg.summ$diagnostics
    
    # D. Combine Regression Results into a Matrix
    df.results <- suppressWarnings(suppressMessages(
      as_tibble(ivreg.summ$coef, rownames='rownames') %>%
        full_join(as_tibble(ivreg.summ$diagnostics, rownames='rownames')) %>%
        full_join(tibble(rownames=c('vars'),
                         var.y=var.y,
                         vars.x=str.vars.x,
                         vars.z=str.vars.z,
                         vars.c=str.vars.c))))
  } else {
    
    # OLS regression
    equa.ols <- paste(var.y,
                      paste(paste(vars.x, collapse='+'),
                            paste(vars.c, collapse='+'), sep='+'),
                      sep='~')
    
    lmreg.summ <- summary(lm(as.formula(equa.ols), data=df))
    
    lm.diagnostics <- as_tibble(
      list(df1=lmreg.summ$df[[1]],
           df2=lmreg.summ$df[[2]],
           df3=lmreg.summ$df[[3]],
           sigma=lmreg.summ$sigma,
           r.squared=lmreg.summ$r.squared,
           adj.r.squared=lmreg.summ$adj.r.squared)) %>%
      gather(variable, value) %>%
      rename(rownames = variable) %>%
      rename(v = value)
    
    df.results <- suppressWarnings(suppressMessages(
      as_tibble(lmreg.summ$coef, rownames='rownames') %>%
        full_join(lm.diagnostics) %>%
        full_join(tibble(rownames=c('vars'),
                         var.y=var.y,
                         vars.x=str.vars.x,
                         vars.c=str.vars.c))))
  }
  
  # E. Flatten Matrix, All IV results as a single tibble 
  # row to be combined with other IV results
  df.row.results <- df.results %>%
    gather(variable, value, -rownames) %>%
    drop_na() %>%
    unite(esti.val, rownames, variable) %>%
    mutate(esti.val = gsub(' ', '', esti.val))
  
  if (transpose) {
    df.row.results <- df.row.results %>% spread(esti.val, value)
  }
  
  # F. Return
  return(data.frame(df.row.results))
}


## ----------------------------------------------------------------------------------------------------
# Library
library(tidyverse)
library(AER)

# Load Sample Data
setwd('C:/Users/fan/R4Econ/_data/')
df <- read_csv('height_weight.csv')


## ----------------------------------------------------------------------------------------------------
# One Instrucments
var.y <- c('hgt')
vars.x <- c('prot')
vars.z <- NULL
vars.c <- c('sex', 'hgt0', 'wgt0')
# Regression
regf.iv(var.y, vars.x, vars.c, vars.z, df, transpose=FALSE) %>%
  kable() %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------
# One Instrucments
var.y <- c('hgt')
vars.x <- c('prot')
vars.z <- c('momEdu')
vars.c <- c('sex', 'hgt0', 'wgt0')
# Regression
regf.iv(var.y, vars.x, vars.c, vars.z, df, transpose=FALSE) %>%
  kable() %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------
# Multiple Instrucments
var.y <- c('hgt')
vars.x <- c('prot')
vars.z <- c('momEdu', 'wealthIdx', 'p.A.prot', 'p.A.nProt')
vars.c <- c('sex', 'hgt0', 'wgt0')
# Regression
regf.iv(var.y, vars.x, vars.c, vars.z, df, transpose=FALSE) %>%
  kable() %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------
# Multiple Instrucments
var.y <- c('hgt')
vars.x <- c('prot', 'cal')
vars.z <- c('momEdu', 'wealthIdx', 'p.A.prot', 'p.A.nProt')
vars.c <- c('sex', 'hgt0', 'wgt0')
# Regression
regf.iv(var.y, vars.x, vars.c, vars.z, df, transpose=FALSE) %>%
  kable() %>%
  kable_styling_fc()


## ----------------------------------------------------------------------------------------------------
# Selecting Variables
var.y <- c('hgt')
vars.x <- c('prot', 'cal')
vars.z <- c('momEdu', 'wealthIdx', 'p.A.prot', 'p.A.nProt')
vars.c <- c('sex', 'hgt0', 'wgt0')


## ----------------------------------------------------------------------------------------------------
# A. create Equation
str.vars.x <- paste(vars.x, collapse='+')
str.vars.c <- paste(vars.c, collapse='+')
str.vars.z <- paste(vars.z, collapse='+')
print(str.vars.x)
print(str.vars.c)
print(str.vars.z)
equa.iv <- paste(var.y,
                 paste(paste(str.vars.x, str.vars.c, sep='+'),
                       paste(str.vars.z, str.vars.c, sep='+'),
                       sep='|'),
                 sep='~')
print(equa.iv)

# B. regression
res.ivreg <- ivreg(as.formula(equa.iv), data=df)
coef(res.ivreg)


## ----------------------------------------------------------------------------------------------------
# C. Regression Summary
ivreg.summ <- summary(res.ivreg, vcov = sandwich, df = Inf, diagnostics = TRUE)

ivreg.summ$coef
ivreg.summ$diagnostics


## ----------------------------------------------------------------------------------------------------
# D. Combine Regression Results into a Matrix
df.results <- suppressMessages(as_tibble(ivreg.summ$coef, rownames='rownames') %>%
    full_join(as_tibble(ivreg.summ$diagnostics, rownames='rownames')) %>%
    full_join(tibble(rownames=c('vars'),
                     var.y=var.y,
                     vars.x=str.vars.x,
                     vars.z=str.vars.z,
                     vars.c=str.vars.c)))
# E. Flatten Matrix, All IV results as a single tibble row to be combined with other IV results
df.row.results <- df.results %>%
    gather(variable, value, -rownames) %>%
    drop_na() %>%
    unite(esti.val, rownames, variable) %>%
    mutate(esti.val = gsub(' ', '', esti.val))


## ----------------------------------------------------------------------------------------------------
# F. Results as Single Colum
# df.row.results


## ----------------------------------------------------------------------------------------------------
# G. Results as Single Row
# df.row.results


## ----------------------------------------------------------------------------------------------------
# t(df.row.results %>% spread(esti.val, value)) %>%
#   kable() %>%
#   kable_styling_fc_wide()

