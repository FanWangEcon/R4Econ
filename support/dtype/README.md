# Naming Conventions

## Across Language Naming Conventions

### Numeric Structures

**Single Data Element**

- *it*: integer
- *fl*: float
- *cx*: complex
- *nn*: NoneType (Python)

**Multiple Data Elements**

- *ls*: list (python)
- *ar*: single dimensional array (reserved for NumPy in Python)
- *mt*: two dimensional matrix
- *ts*: three dimensional tensor
- *mn*: any larger than two dimensional array

**Dataframes Pandas**

- *sr*: for pandas series 
- *df*: for pandas dataframe 

And a special name is

- *gn*: stands for generic type, could be typed differently, the function will parse the parameter type and then behave accordingly.

### Other Structures

- *tp*: immutable tuple if tuple is supported by the language
- *pl*: plot or figure object
- *ob*: a generic object of any time
- *fc*: a function anonymous function for example, function to be passed as parameters

### Strings

- *st*: any kind of words, single sentence.

#### Special Strings

**Strings for Special Purposes**

- *stf*: prefix for entire string files, all lines, including read in and to be reading in.
- *stm*: time variables values etc that can used for calculations
- *stg*: graph strings

**Strings but not Actually Strings**

- *svr*: variable string, as in data variable name
- *sfc*: function string name
- *spg*: not string, but code, to be interpreted as code later

#### String Path and File Names

- *spt*: absolute string path, path only, does not include name.
- *spn*: absolute string path, including name.

- *srt*: relative file path, no name
- *srn*: relative file path, with name

- *snm*: function name with suffix, no path.
- *sna*: no suffix function name

- *suf*: suffix only

### Language specific variable conveitons

#### R Conventions

- *tb*: a tibble
- *df*: a dataframe

#### Matlab Conventions

- *cl*: matlab cell
	+ *cln*: cell matrix, meaning some n by m cells
- *mp*: matlab container
- *sa*: structure array

#### python conventions

- *co*: class object

## R Conventions

### R Files naming conventions

#### Vignette files 

Vignette can be used to call functions to generate results, or be used to illustrate, demonstrate function usages. 

- ffv: generic prefix for vignettes
- _ge: files that generates new files, might be difficult ot distinguish _ge and _su
- _su: files that documents key data facts/summary stats
- _an: analysis vignettes
- _fu: function building files, demonstrating step by step creation of function
- words after: do not exceed two words separated by underscore, three underscore overall


## Github Projects Naming Conventions

Personal non-organizational 

Research Projects:

- First set of letters:
  - starts with: Prj
- Second set of letters:
  - if country-data-oriented: Country short or full name, PrjThai, PrjChina, PrjUK, PrjMex
  - if global-data-oriented: PrjComp, Comp for comparative. 
- Third set of letters:
  - Acronyms to keep things short and not too clear should be included under each project.
  - Avoid using co-author acrynyms to avoid lack of ability to expand project team

## Project Folder Naming Conventions

## Title and heading conventions

To make life easier, in general, for all section heading, use sentence case, so only need to capitalize the first letter. 