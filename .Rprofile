# Load Libraries
library(tidyverse)
library(tidyr)
library(knitr)
library(kableExtra)
library(REconTools)

# RMD Options
options(knitr.duplicate.label = "allow")

# Output HTML or Latex
if (knitr::is_latex_output()) {
  options(knitr.table.format = "latex")
} else {
  options(knitr.table.format = "html")
}

# Table Output Options
kable_styling_fc = function(kable_input){
  kable_styling(kable_input,
    bootstrap_options = c("striped", "hover", "responsive"),
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    fixed_thead = T,
    position = "center",
    font_size = NULL,
    row_label_position = "l")
}

# Table Output Options:
# 1. scale_down for TEX
# 2. box width: see R4Econ\style.css for body width, set width to bodywidth - 225
if (knitr::is_latex_output()) {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l")
  }
} else {
  kable_styling_fc_wide = function(kable_input){
    kable_styling(kable_input,
      bootstrap_options = c("striped", "hover", "responsive"),
      latex_options = c("striped", "scale_down", "hold_position"),
      full_width = FALSE,
      fixed_thead = T,
      position = "center",
      font_size = NULL,
      row_label_position = "l") %>%
    scroll_box(width = "875px")
  }
}

# Common Shared Text and Strings
total_area <- (800 * 7) / 2
text_shared_preamble_one <- "> Go back to [fan](http://fanwangecon.github.io/CodeDynaAsset/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository, or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository."
text_shared_preamble_two <- ""
text_shared_preamble_thr <- ""

if (knitr::is_latex_output()) {
    text_top_count <- ""
    text_end_count <- ""
} else {
    text_top_count <- "[![HitCount](http://hits.dwyl.io/fanwangecon/R4Econ.svg)](https://github.com/FanWangEcon/R4Econ)  [![Star](https://img.shields.io/github/stars/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/R4Econ?style=social)](https://github.com/FanWangEcon/R4Econ/watchers)"
    text_end_count <- "[![](https://img.shields.io/github/last-commit/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/R4Econ)](https://github.com/FanWangEcon/R4Econ/pulls)"
}
