## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## ---- eval=FALSE----------------------------------------------------------------------
## # Fou words with metacharacters
## ls_st_regex_charclass <- c(
##   '00d',
##   'z\\12323_4',
##   'pa(_2+\\3',
##   'p99.9_sdfasdpf0',
##   'k9p.e_d+fd')
## # Matches any characters with the letter p
## print(grepl("[p]", ls_st_regex_charclass))
## # Matches any characters with backslash
## print(grepl("[\\]", ls_st_regex_charclass))
## # Matches any characters with the number 3
## print(grepl("[3]", ls_st_regex_charclass))
## 
## # > print(grepl("[p]", ls_st_regex_charclass))
## # [1] FALSE FALSE  TRUE  TRUE  TRUE
## # > print(grepl("[\\]", ls_st_regex_charclass))
## # [1] FALSE  TRUE  TRUE FALSE FALSE
## # > print(grepl("[3]", ls_st_regex_charclass))
## # [1] FALSE  TRUE  TRUE FALSE FALSEZ


## ---- eval=FALSE----------------------------------------------------------------------
## # Matches any characters eithr with letter p or d
## print(grepl('[pd]', ls_st_regex_charclass))
## # Matches any characters eithr with letter p or _
## print(grepl('[p_]', ls_st_regex_charclass))
## # Matches any characters eithr with letter p or _ or 0
## print(grepl('[p_0]', ls_st_regex_charclass))
## 
## # > print(grepl('[pd]', ls_st_regex_charclass))
## # [1]  TRUE FALSE  TRUE  TRUE  TRUE
## # > print(grepl('[p_]', ls_st_regex_charclass))
## # [1] FALSE  TRUE  TRUE  TRUE  TRUE
## # > print(grepl('[p_0]', ls_st_regex_charclass))
## # [1] TRUE TRUE TRUE TRUE TRUE


## ---- eval=FALSE----------------------------------------------------------------------
## # Finds strings that has anything other than d and 0
## print(grepl('[^d0]', ls_st_regex_charclass))
## 
## # > print(grepl('[^d0]', ls_st_regex_charclass))
## # [1] FALSE  TRUE  TRUE  TRUE  TRUE


## ---- eval=FALSE----------------------------------------------------------------------
## # Fou words with metacharacters
## ls_st_regex_rep_quantifer <- c(
##   '00d',
##   'z\\12323_40',
##   'ppa(_2+\\3',
##   'p99.9_sdfasdpf0',
##   'k9p.e_d+fd')
## # Matches any characters pp
## print(grepl("[p]{2}", ls_st_regex_rep_quantifer))
## # Matches any characters with the number 3
## print(grepl("[9]{2}", ls_st_regex_rep_quantifer))
## 
## # > print(grepl("[p]{2}", ls_st_regex_rep_quantifer))
## # [1] FALSE FALSE  TRUE FALSE FALSE
## # > print(grepl("[9]{2}", ls_st_regex_rep_quantifer))
## # [1] FALSE FALSE FALSE  TRUE FALSE


## ---- eval=FALSE----------------------------------------------------------------------
## ls_st_regex_joint <- c(
##   '_asdf123p',
##   'pz12p323_40_',
##   'ppa(_2+\\3',
##   'p9_sdfasdpf0',
##   'p_k9p.e_d+fd',
##   'p123k_dfk')


## ---- eval=FALSE----------------------------------------------------------------------
## # Start with p, followed by _
## print(grepl("p_", ls_st_regex_joint))
## # Start with p, followed by a single alpha-numeric, then _
## print(grepl("p[[:alnum:]]_", ls_st_regex_joint))
## # Start with p, followed by either:
## # 1 single alpha-numeric, then _
## # no alpha-numeric, then _
## print(grepl("p[[:alnum:]]?_", ls_st_regex_joint))
## 
## # > print(grepl("p_", ls_st_regex_joint))
## # [1] FALSE FALSE FALSE FALSE  TRUE FALSE
## # > print(grepl("p[[:alnum:]]_", ls_st_regex_joint))
## # [1] FALSE FALSE FALSE  TRUE FALSE FALSE
## # > print(grepl("p[[:alnum:]]?_", ls_st_regex_joint))
## # [1] FALSE FALSE FALSE  TRUE  TRUE FALSE


## ---- eval=FALSE----------------------------------------------------------------------
## print(grepl("p[[:alnum:]]*_", ls_st_regex_joint))
## 
## # > print(grepl("p[[:alnum:]]*_", ls_st_regex_joint))
## # [1] FALSE  TRUE FALSE  TRUE  TRUE  TRUE


## ---- eval=FALSE----------------------------------------------------------------------
## # p and _ separated by at least 1 alpha numerics
## print(grepl("p[[:alnum:]]+_", ls_st_regex_joint))
## 
## # > print(grepl("p[[:alnum:]]+_", ls_st_regex_joint))
## # [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE

