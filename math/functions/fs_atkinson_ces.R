#' ---
#' title: "Anazlying Atkinson Family Utility (CES)"
#' output:
#'   pdf_document: default
#'   word_document: default
#'   html_document: default
#'   html_notebook: default
#' urlcolor: blue
#' always_allow_html: yes
#' ---
#' 
#' Go back to [fan](http://fanwangecon.github.io/CodeDynaAsset/)'s [REconTools](https://fanwangecon.github.io/REconTools/) Package, [R4Econ](https://fanwangecon.github.io/R4Econ/) Repository, or [Intro Stats with R](https://fanwangecon.github.io/Stat4Econ/) Repository.
#' 
## ----GlobalOptions, echo = T, results = 'hide', message=F, warning=F----------
rm(list = ls(all.names = TRUE))
options(knitr.duplicate.label = 'allow')

#' 
## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(REconTools)
# file name
st_file_name = 'fs_atkinson_ces'
# Generate R File
purl(paste0(st_file_name, ".Rmd"), output=paste0(st_file_name, ".R"), documentation = 2)
# Generate PDF and HTML
# rmarkdown::render("C:/Users/fan/R4Econ/math/function/fs_atkinson_ces", "pdf_document")
# rmarkdown::render("C:/Users/fan/R4Econ/math/function/fs_atkinson_ces", "html_document")

#' 
#' # Atkinson Family Utility
#' 
#' How does the Aktinson Family utility function work? THe Atkinson Family Utility has the following functional form. 
#' 
#' $$
#' V^{\text{social}}
#' =
#' \left(
#' \alpha
#' \cdot 
#' A^{\lambda}
#' +
#' \beta
#' \cdot
#' B^{\lambda}
#' \right)^{\frac{1}{\lambda}}
#' $$
#' 
#' Several key issues here:
#' 
#' 1. $V^{\text{social}}$ is the utility of some social planner
#' 2. $A$ and $B$ are allocations for Alex and Ben. 
#' 3. $\alpha$ and $\beta$ are biases that a social planner has for Alex and Ben: $\alpha+\beta=1$, $\alpha>0$, and $\beta>0$
#' 4. $-\infty < \lambda \le 1$ is a measure of inequality aversion
#'     * $\lambda=1$ is when the planner cares about weighted total allocations (efficient, Utilitarian)
#'     * $\lambda=-\infty$ is when the planner cares about only the minimum between $A$ and $B$ allocations (equality, Rawlsian)
#' 
#' ## What if only care about Alex?
#' 
#' Clearly, if the planner only cares about Ben, $\beta=1$, then:
#'  
#' $$
#' V^{\text{social}} 
#' =
#' \left(
#' B^{\lambda}
#' \right)^{\frac{1}{\lambda}}
#' = B
#' $$
#' 
#' Clearly, regardless of the value of $\lambda$, as $B$ increases $V$ increases. 
#' 
#' 
#' ## What Happens to V when A or B increases?
#' 
#' What is the derivative of $V$ with respect to $A$ or $B$?
#' 
#' $$
#' \frac{\partial V}{\partial A}
#' =
#' \frac{1}{\lambda}
#' \left(
#' \alpha
#' A^{\lambda}
#' +
#' \beta
#' B^{\lambda}
#' \right)^{\frac{1}{\lambda}-1}
#' \cdot
#' \lambda
#' \alpha
#' A^{\lambda -1}
#' $$
#' 
#' With just a little bit of simplification, we have:
#' $$
#' \frac{\partial V}{\partial A}
#' =
#' \left(
#' \alpha
#' A^{\lambda}
#' +
#' \beta
#' B^{\lambda}
#' \right)^{\frac{1-\lambda}{\lambda}}
#' \cdot
#' \alpha
#' A^{\lambda -1}
#' >0
#' $$
#' 
#' It is important to note that $\frac{\partial V}{\partial A}>0$. When $\lambda <0$, $Z^{\lambda}>0$. For example $10^{-2}=\frac{1}{100}$. And For example $0.1^{\frac{3}{-2}}=\frac{1}{0.1^{1.5}}$. Still Positive.
#' 
#' This might be surprising, because when $\lambda < 0$:
#' $$
#' \text{ if } \lambda <0
#' \thinspace\thinspace
#' \text{ then }
#' \thinspace\thinspace
#' \frac{d \left(\alpha A^{\lambda} + \beta B^{\lambda}\right)}{dA}=\alpha\lambda A^{\lambda -1}<0
#' $$
#' so if we did not have the outter $\frac{1}{\lambda}$ power, negative $\lambda$ would lead to decreasing weighted sum. But: 
#' $$
#' \text{ if } \lambda <0
#' \thinspace\thinspace
#' \text{ then }
#' \thinspace\thinspace
#' \frac{dG^{\frac{1}{\lambda}}}{dG}=\frac{1}{\lambda}\cdot G^{\frac{1-\lambda}{\lambda}}<0
#' $$
#' so when $G$ is increasing and $\lambda <0$, $V$ would decrease. But when $G\left(A,B\right)$ is decreasing, due to increasing $A$ when $\lambda <0$, $V$ will actually increase.
#' 
#' 
#' 
#' 
