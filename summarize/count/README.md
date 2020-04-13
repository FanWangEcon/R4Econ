Files in the count folder count observations, unique identifiers within subgroups. Various types of within group counting calculations.

<!-- 2. [Tabulation Categorical as Matrix](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R): ipynb \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/tabulate/ListUniqueCateNAsMat.R) \| html \| pdf
    + Many-Category Categorical Variable, Tabulation shown as Matrix.
    + **tidy**: *group_by + summarise(freq = n()) + mutate + min(ceiling(sqrt(count))) + substring + dim/reshape*
3. [By Groups, Count Variables Observations](summarize/count/ByGroupCountAllVarNonNA.html): [**ipynb**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.ipynb) \| [**r**](https://github.com/FanWangEcon/R4Econ/blob/master/summarize/count/ByGroupCountAllVarNonNA.R) \|  [**html**](https://fanwangecon.github.io/R4Econ/summarize/count/ByGroupCountAllVarNonNA.html) \| pdf
    + By Groups, Count non-NA observations of All Variables.
    + **tidy**: *group_by + summarise_if(is.numeric, funs(sum(is.na(.)==0)))* -->
